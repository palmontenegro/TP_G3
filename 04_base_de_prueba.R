# Genera y carga una base de prueba: 366 registros diarios de 2024 para la región
# simulada "PRUEBA_AMBA". La demanda se simula con una función en "U" asimétrica de la
# temperatura, un efecto de fin de semana y de feriados y ruido normal.
# Se utiliza solo para verificar el funcionamiento del flujo, no para estimar la
# sensibilidad térmica. Tiene región propia para no mezclarse con los datos reales.

source("00_funciones.R")
set.seed(PARAM$semilla)

fechas <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")   # 366 días
n      <- length(fechas)

# Temperatura media simulada: ciclo anual con máximo en enero (hemisferio sur) más ruido normal
doy        <- yday(fechas)
temp_media <- 18 + 7.5 * cos(2 * pi * (doy - 15) / 366) + rnorm(n, 0, 2.2)

# Feriados nacionales de 2024 (aproximados; solo se usan en la simulación)
feriados_prueba <- as.Date(c(
  "2024-01-01", "2024-02-12", "2024-02-13", "2024-03-24", "2024-03-28",
  "2024-03-29", "2024-04-02", "2024-05-01", "2024-05-25", "2024-06-17",
  "2024-06-20", "2024-07-09", "2024-10-11", "2024-11-18", "2024-12-25"
))
es_feriado <- as.integer(fechas %in% feriados_prueba)
dow        <- wday(fechas, week_start = 1)                                # 1 = lunes

# Demanda: "U" asimétrica con mínimo en t0 = 17,5 °C y mayor curvatura en calor que en frío,
# más descuentos por sábado, domingo y feriado
t0       <- 17.5
u_curva  <- ifelse(temp_media >= t0,
                   65 * (temp_media - t0)^2,
                   31 * (temp_media - t0)^2)
efecto_finde    <- ifelse(dow == 6, -1200, ifelse(dow == 7, -1900, 0))
efecto_feriado  <- -1800 * es_feriado
demanda <- 15000 + u_curva + efecto_finde + efecto_feriado + rnorm(n, 0, 450)

# Carga a Supabase
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