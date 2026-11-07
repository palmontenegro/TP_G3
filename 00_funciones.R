# =====================================================================
# 00_funciones.R
# Parámetros del diseño y funciones compartidas por todos los scripts.
# No se ejecuta solo: los otros scripts lo cargan con source("00_funciones.R")
# =====================================================================

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(ggplot2)
  library(mgcv)
})

# ---------------------------------------------------------------------
# PARÁMETROS DEL DISEÑO (coinciden con la Introducción del informe)
# ---------------------------------------------------------------------
PARAM <- list(
  # Región a analizar. Con la base de prueba: "PRUEBA_AMBA".
  # Con datos reales: cambiar a la región que cargues (p. ej. "AMBA").
  region             = "GBA",
  prop_entrenamiento = 0.80,                       # 80 % entrenamiento / 20 % prueba
  mape_max           = 10,                         # MAPE (%) a partir del cual P = 0
  t_max              = 300,                        # segundos a partir de los cuales T = 0
  repeticiones_t     = 5,                          # repeticiones para medir T
  pesos              = c(A = 0.25, P = 0.35, E = 0.25, T = 0.15),
  umbral             = 0.70,                       # Eficiencia >= 0,70
  dia_ref            = "Miercoles",                # día de referencia para dibujar la curva
  semilla            = 2026
)

DIAS <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")

# ---------------------------------------------------------------------
# CONEXIÓN A SUPABASE (cifrada por SSL, credenciales en variables de entorno)
# ---------------------------------------------------------------------
conectar_db <- function() {
  vars <- c("SUPABASE_HOST", "SUPABASE_PORT", "SUPABASE_DB",
            "SUPABASE_USER", "SUPABASE_PASSWORD")
  faltan <- vars[Sys.getenv(vars) == ""]
  if (length(faltan) > 0) {
    stop("Faltan variables de entorno: ", paste(faltan, collapse = ", "),
         ". Revisá el archivo .Renviron (ver README) y reiniciá R.")
  }
  dbConnect(
    RPostgres::Postgres(),
    host     = Sys.getenv("SUPABASE_HOST"),
    port     = as.integer(Sys.getenv("SUPABASE_PORT")),
    dbname   = Sys.getenv("SUPABASE_DB"),
    user     = Sys.getenv("SUPABASE_USER"),
    password = Sys.getenv("SUPABASE_PASSWORD"),
    sslmode  = "require"
  )
}

# ---------------------------------------------------------------------
# LECTURA DE LA VISTA datos_unificados
# ---------------------------------------------------------------------
leer_datos <- function(con, region = PARAM$region) {
  sql <- "SELECT fecha, region, demanda_mw, temp_media_c, temp_max_c, temp_min_c,
                 dia_semana, es_feriado, mes, estacion
          FROM datos_unificados
          WHERE region = $1
          ORDER BY fecha"
  df <- dbGetQuery(con, sql, params = list(region))
  if (nrow(df) == 0) {
    stop("La vista datos_unificados no devolvió filas para la región '", region,
         "'. Revisá PARAM$region en 00_funciones.R y que haya datos de demanda y temperatura para las mismas fechas.")
  }
  df |>
    mutate(
      fecha        = as.Date(fecha),
      demanda_mw   = as.numeric(demanda_mw),
      temp_media_c = as.numeric(temp_media_c),
      dia_semana   = factor(dia_semana, levels = DIAS),
      es_feriado   = as.integer(es_feriado),
      mes          = as.integer(mes)
    ) |>
    filter(!is.na(demanda_mw), !is.na(temp_media_c)) |>
    arrange(fecha)
}

# ---------------------------------------------------------------------
# DIVISIÓN CRONOLÓGICA ENTRENAMIENTO / PRUEBA (80 % / 20 %)
# ---------------------------------------------------------------------
dividir_train_test <- function(df, prop = PARAM$prop_entrenamiento) {
  df <- df |> arrange(fecha)
  n_train <- floor(nrow(df) * prop)
  list(
    train = df[seq_len(n_train), ],
    test  = df[(n_train + 1):nrow(df), ]
  )
}

# ---------------------------------------------------------------------
# MODELO: demanda_mw ~ s(temp_media_c) + dia_semana + es_feriado
# GAM gaussiano, spline de regresión penalizado, estimación REML
# ---------------------------------------------------------------------
ajustar_gam <- function(df) {
  gam(demanda_mw ~ s(temp_media_c) + dia_semana + es_feriado,
      data = df, family = gaussian(), method = "REML")
}

# Curva ajustada sobre una grilla de temperatura (día de referencia, no feriado)
curva_gam <- function(modelo, grilla, ref_dia = PARAM$dia_ref) {
  nd <- data.frame(
    temp_media_c = grilla,
    dia_semana   = factor(ref_dia, levels = DIAS),
    es_feriado   = 0L
  )
  p <- predict(modelo, newdata = nd, se.fit = TRUE)
  data.frame(temp = grilla, y = as.numeric(p$fit), se = as.numeric(p$se.fit))
}

# ---------------------------------------------------------------------
# VARIABLE A: ajuste  ->  A = max(0, R2 ajustado) en entrenamiento
# ---------------------------------------------------------------------
calcular_A <- function(modelo) {
  max(0, summary(modelo)$r.sq)
}

# ---------------------------------------------------------------------
# VARIABLE P: precisión predictiva  ->  P = max(0, 1 - MAPE/10 %)
# ---------------------------------------------------------------------
calcular_P <- function(modelo, test) {
  pred <- as.numeric(predict(modelo, newdata = test))
  mape <- mean(abs(test$demanda_mw - pred) / test$demanda_mw) * 100
  list(P = max(0, 1 - mape / PARAM$mape_max), mape = mape, pred = pred)
}

# ---------------------------------------------------------------------
# VARIABLE E: estabilidad de la estimación
# Serie completa partida en meses pares / impares; pendiente media de la
# curva en calor (> percentil 90) y en frío (< percentil 10) por subconjunto.
# ---------------------------------------------------------------------
pendiente_extremo <- function(curva, tipo, p10, p90) {
  sub <- if (tipo == "calor") curva[curva$temp > p90, ] else curva[curva$temp < p10, ]
  if (nrow(sub) < 3) return(NA_real_)
  mean(diff(sub$y) / diff(sub$temp))        # diferencias finitas, MW/°C
}

similitud <- function(s1, s2) {
  if (anyNA(c(s1, s2))) return(0)
  den <- max(abs(s1), abs(s2))
  if (den == 0) return(1)
  max(0, 1 - abs(s1 - s2) / den)
}

calcular_E <- function(df) {
  p10 <- quantile(df$temp_media_c, 0.10, names = FALSE)
  p90 <- quantile(df$temp_media_c, 0.90, names = FALSE)

  grupos <- list(
    pares   = df[month(df$fecha) %% 2 == 0, ],
    impares = df[month(df$fecha) %% 2 == 1, ]
  )

  pend <- sapply(grupos, function(g) {
    m      <- ajustar_gam(g)
    grilla <- seq(min(g$temp_media_c), max(g$temp_media_c), by = 0.1)
    curva  <- curva_gam(m, grilla)
    c(calor = pendiente_extremo(curva, "calor", p10, p90),
      frio  = pendiente_extremo(curva, "frio",  p10, p90))
  })

  E_calor <- similitud(pend["calor", 1], pend["calor", 2])
  E_frio  <- similitud(pend["frio",  1], pend["frio",  2])
  list(E = (E_calor + E_frio) / 2, E_calor = E_calor, E_frio = E_frio,
       pendientes = pend, p10 = p10, p90 = p90)
}

# ---------------------------------------------------------------------
# FLUJO COMPLETO: lectura -> división -> ajuste -> A, P, E
# (esto es lo que se cronometra para la variable T)
# ---------------------------------------------------------------------
ejecutar_flujo <- function(con, region = PARAM$region) {
  df     <- leer_datos(con, region)
  partes <- dividir_train_test(df)
  modelo <- ajustar_gam(partes$train)
  A <- calcular_A(modelo)
  Pr <- calcular_P(modelo, partes$test)
  Er <- calcular_E(df)
  list(A = A, P = Pr$P, mape = Pr$mape, pred_test = Pr$pred,
       E = Er$E, E_calor = Er$E_calor, E_frio = Er$E_frio, pendientes = Er$pendientes,
       modelo = modelo, df = df, train = partes$train, test = partes$test)
}

# ---------------------------------------------------------------------
# VARIABLE T: tiempo de ejecución  ->  T = max(0, 1 - t / 300 s)
# ---------------------------------------------------------------------
medir_T <- function(con, region = PARAM$region, repeticiones = PARAM$repeticiones_t) {
  tiempos <- numeric(repeticiones)
  for (i in seq_len(repeticiones)) {
    tiempos[i] <- system.time(ejecutar_flujo(con, region))[["elapsed"]]
  }
  t_prom <- mean(tiempos)
  list(T_val = max(0, 1 - t_prom / PARAM$t_max), t_prom = t_prom, tiempos = tiempos)
}

# ---------------------------------------------------------------------
# EFICIENCIA = 0,25*A + 0,35*P + 0,25*E + 0,15*T
# ---------------------------------------------------------------------
calcular_eficiencia <- function(A, P, E, T_val) {
  sum(PARAM$pesos * c(A, P, E, T_val))
}

registrar_evaluacion <- function(con, region, A, P, E, T_val, mape, t_prom, notas = "") {
  dbExecute(
    con,
    "INSERT INTO evaluaciones (region, a, p, e, t, mape_pct, t_promedio_s, notas)
     VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
    params = list(region, A, P, E, T_val, mape, t_prom, notas)
  )
}

# ---------------------------------------------------------------------
# CARGA A LA BASE (inserta o actualiza; evita duplicados por clave primaria)
# Requiere conexión directa o "session pooler" (puerto 5432) por la tabla temporal.
# ---------------------------------------------------------------------
upsert_tabla <- function(con, df, tabla, claves) {
  df <- df[!duplicated(df[claves]), ]
  stage <- paste0("stg_", tabla)
  q <- function(x) as.character(dbQuoteIdentifier(con, x))
  dbWriteTable(con, stage, df, temporary = TRUE, overwrite = TRUE)

  cols    <- names(df)
  noclave <- setdiff(cols, claves)
  accion  <- if (length(noclave) > 0) {
    paste0("DO UPDATE SET ",
           paste(sprintf("%s = EXCLUDED.%s", q(noclave), q(noclave)), collapse = ", "))
  } else "DO NOTHING"

  sql <- sprintf(
    "INSERT INTO %s (%s) SELECT %s FROM %s ON CONFLICT (%s) %s",
    q(tabla), paste(q(cols), collapse = ", "), paste(q(cols), collapse = ", "),
    q(stage), paste(q(claves), collapse = ", "), accion
  )
  n <- dbExecute(con, sql)
  dbExecute(con, sprintf("DROP TABLE IF EXISTS %s", q(stage)))
  n
}
