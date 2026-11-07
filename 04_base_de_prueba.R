# =====================================================================
# 04_base_de_prueba.R
# Genera y carga la BASE DE PRUEBA: 366 registros diarios de 2024, región
# "PRUEBA_AMBA", con demanda simulada a partir de una función en "U"
# asimétrica de la temperatura + efecto de fin de semana y feriados + ruido
# normal. Sirve SOLO para verificar que el flujo funciona, NO para estimar
# la sensibilidad térmica.
# Se carga con una región propia ("PRUEBA_AMBA") para no mezclarse con los
# datos reales.
# =====================================================================

source("00_funciones.R")
set.seed(PARAM$semilla)

fechas <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")   # 366 días
n      <- length(fechas)

# Temperatura media simulada: ciclo anual (máximo en enero, hemisferio sur) + ruido
doy        <- yday(fechas)
temp_media <- 18 + 7.5 * cos(2 * pi * (doy - 15) / 366) + rnorm(n, 0, 2.2)

# Feriados nacionales 2024 aproximados (solo para la simulación)
feriados_prueba <- as.Date(c(
  "2024-01-01", "2024-02-12", "2024-02-13", "2024-03-24", "2024-03-28",
  "2024-03-29", "2024-04-02", "2024-05-01", "2024-05-25", "2024-06-17",
  "2024-06-20", "2024-07-09", "2024-10-11", "2024-11-18", "2024-12-25"
))
es_feriado <- as.integer(fechas %in% feriados_prueba)
dow        <- wday(fechas, week_start = 1)                                # 1 = lunes

# Demanda en forma de "U" asimétrica: mínimo cerca de 17,5 °C;
# el calor eleva la demanda más que el frío.
t0       <- 17.5
u_curva  <- ifelse(temp_media >= t0,
                   65 * (temp_media - t0)^2,
                   31 * (temp_media - t0)^2)
efecto_finde    <- ifelse(dow == 6, -1200, ifelse(dow == 7, -1900, 0))
efecto_feriado  <- -1800 * es_feriado
demanda <- 15000 + u_curva + efecto_finde + efecto_feriado + rnorm(n, 0, 450)

# ---- Carga a Supabase ----
con <- conectar_db()

upsert_tabla(con, data.frame(id_estacion = "PRUEBA", nombre = "Estación de prueba (simulada)",
                             provincia = "N/A"),
             "estaciones_smn", "id_estacion")
upsert_tabla(con, data.frame(region = "PRUEBA_AMBA", descripcion = "Región simulada para verificar el flujo",
                             id_estacion = "PRUEBA"),
             "regiones", "region")
upsert_tabla(con, data.frame(fecha = fechas, region = "PRUEBA_AMBA",
                             demanda_mw = round(demanda, 1)),
             "demanda_cammesa", c("fecha", "region"))
upsert_tabla(con, data.frame(fecha = fechas, id_estacion = "PRUEBA",
                             temp_media_c = round(temp_media, 1)),
             "temperatura_smn", c("fecha", "id_estacion"))
upsert_tabla(con, data.frame(fecha = feriados_prueba, descripcion = "Feriado (prueba)"),
             "feriados", "fecha")

n_vista <- dbGetQuery(con, "SELECT count(*) AS n FROM datos_unificados WHERE region = 'PRUEBA_AMBA'")$n
cat("Registros de la base de prueba disponibles en datos_unificados:", n_vista, "(esperado: 366)\n")
dbDisconnect(con)
