# Carga a Supabase los datos reales:
#   - Demanda: CAMMESA, "Base Demanda Diaria 2017 2026" (hoja "Datos Región",
#     columna GRAN BS.AS., demanda neta media diaria en MW).
#   - Temperatura: SMN, estación Aeroparque (datos diarios).
#   - Feriados: derivados de la columna "Tipo día" de la base de CAMMESA.
# Los archivos de origen están en la carpeta datos/ del proyecto.

source("00_funciones.R")
library(readxl)

# Configuración
REGION        <- "GBA"
DESC_REGION   <- "Gran Buenos Aires (región GRAN BS.AS. de CAMMESA)"
ID_ESTACION   <- "87582"                       # código OMM de Aeroparque
NOM_ESTACION  <- "AEROPARQUE AERO"             # debe coincidir con la columna NOMBRE del SMN
PROV_ESTACION <- "CIUDAD AUTONOMA DE BUENOS AIRES"

# Años completos; la base de CAMMESA se extiende hasta 2026-09-30
FECHA_DESDE <- as.Date("2017-01-01")
FECHA_HASTA <- as.Date("2025-12-31")

# Busca cada archivo por patrón en el nombre, para tolerar espacios, guiones bajos o sufijos como "(1)"
buscar_archivo <- function(patron, carpeta = "datos", obligatorio = TRUE) {
  if (!dir.exists(carpeta)) {
    stop("No existe la carpeta '", carpeta, "' dentro de ", getwd(), ".\n",
         "Crearla con dir.create(\"datos\") y copiar allí los archivos, o cambiar la carpeta de trabajo ",
         "con Session > Set Working Directory > Choose Directory.")
  }
  f <- list.files(carpeta, pattern = patron, full.names = TRUE, ignore.case = TRUE)
  if (length(f) == 0) {
    if (!obligatorio) return(NA_character_)
    stop("No se encontró ningún archivo que coincida con '", patron, "' en ", normalizePath(carpeta), ".\n",
         "Archivos en esa carpeta: ",
         if (length(list.files(carpeta))) paste(list.files(carpeta), collapse = " | ") else "(vacía)")
  }
  message("Usando: ", f[1])
  f[1]
}
ARCHIVO_DEMANDA       <- buscar_archivo("DEMANDA_DIARIA.*\\.xlsx$")
ARCHIVO_SMN_XLSX <- buscar_archivo("SMN_DATOS.*\\.xlsx$")
# Datos del SMN desde 2021: archivo .lst separado por tabuladores, con fechas dd/mm/aaaa.
# Si no está, el período se corta en 2020-12-31 (solo sirve para probar la carga).
ARCHIVO_SMN_2021      <- buscar_archivo("\\.lst$", obligatorio = FALSE)
SEP_SMN_2021          <- "\t"

parsear_fecha <- function(x) {
  as.Date(parse_date_time(as.character(x),
                          orders = c("Ymd HMS", "Ymd HM", "Ymd", "dmY HMS", "dmY HM", "dmY"),
                          quiet = TRUE))
}
# Convierte a número aceptando la coma como separador decimal
num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))

if (is.na(ARCHIVO_SMN_2021) && FECHA_HASTA > as.Date("2020-12-31")) {
  message("Sin archivo del SMN desde 2021: el período se corta en 2020-12-31 (solo para probar la carga).")
  FECHA_HASTA <- as.Date("2020-12-31")
}

# Demanda
dem_raw <- read_excel(ARCHIVO_DEMANDA, sheet = 2, skip = 4)     # 2.ª hoja: "Datos Región"
i_fecha <- grep("^Fecha$", names(dem_raw))
i_tipo  <- grep("^Tipo",   names(dem_raw))
i_gba   <- grep("^GRAN",   names(dem_raw))
if (length(i_fecha) != 1 || length(i_tipo) != 1 || length(i_gba) != 1) {
  stop("No se encontraron las columnas Fecha / Tipo día / GRAN BS.AS. Columnas leídas: ",
       paste(names(dem_raw), collapse = " | "))
}
dem_all <- tibble(
  fecha      = as.Date(dem_raw[[i_fecha]]),
  tipo_dia   = as.character(dem_raw[[i_tipo]]),
  demanda_mw = as.numeric(dem_raw[[i_gba]])
) |> filter(!is.na(fecha), !is.na(demanda_mw))

# Feriados: días que CAMMESA clasifica como "Domingo o Feriado" y que no caen en domingo
feriados <- dem_all |>
  filter(grepl("Feriado", tipo_dia, ignore.case = TRUE), wday(fecha, week_start = 1) != 7) |>
  transmute(fecha, descripcion = "Feriado (tipo de día CAMMESA)") |>
  filter(fecha >= FECHA_DESDE, fecha <= FECHA_HASTA)

dem <- dem_all |>
  filter(fecha >= FECHA_DESDE, fecha <= FECHA_HASTA, demanda_mw > 0) |>
  transmute(fecha, region = REGION, demanda_mw)

# Temperatura
# Excel del SMN (1.ª hoja): se leen solo NOMBRE, FECHA, TMAX, TMIN y TMEDIA, ubicadas por nombre.
# El archivo completo es muy grande y la lectura puede tardar unos minutos.
message("Leyendo el Excel del SMN (puede tardar unos minutos)...")
nm <- trimws(names(read_excel(ARCHIVO_SMN_XLSX, sheet = 1, n_max = 0)))
nec_x <- c("NOMBRE", "FECHA", "TMAX", "TMIN", "TMEDIA")
if (!all(nec_x %in% nm)) {
  stop("El Excel del SMN no tiene las columnas esperadas (", paste(nec_x, collapse = ", "),
       "). Columnas leídas: ", paste(nm, collapse = " | "))
}
tipos <- ifelse(nm == "FECHA", "guess", ifelse(nm == "NOMBRE", "text",
                                               ifelse(nm %in% c("TMAX", "TMIN", "TMEDIA"), "text", "skip")))
smn1 <- read_excel(ARCHIVO_SMN_XLSX, sheet = 1, col_types = tipos)
names(smn1) <- trimws(names(smn1))
a_fecha <- function(x) if (inherits(x, c("POSIXct", "Date"))) as.Date(x) else parsear_fecha(x)
tmp <- smn1 |>
  filter(trimws(NOMBRE) == NOM_ESTACION) |>
  transmute(fecha = a_fecha(FECHA), temp_max_c = num(TMAX), temp_min_c = num(TMIN), temp_media_c = num(TMEDIA))
rm(smn1)
cat("Excel del SMN:", nrow(tmp), "días de", NOM_ESTACION, "entre",
    format(min(tmp$fecha, na.rm = TRUE)), "y", format(max(tmp$fecha, na.rm = TRUE)), "\n")

# Archivo del SMN desde 2021
if (!is.na(ARCHIVO_SMN_2021)) {
  s2 <- read_delim(ARCHIVO_SMN_2021, delim = SEP_SMN_2021, show_col_types = FALSE,
                   col_types = cols(.default = col_character()),
                   locale = locale(encoding = "latin1"))
  names(s2) <- trimws(names(s2))             # los encabezados del .lst vienen con espacios de relleno
  nec <- c("NOMBRE", "FECHA", "TMAX", "TMIN", "TMEDIA")
  if (!all(nec %in% names(s2))) {
    stop("El archivo desde 2021 no tiene las columnas esperadas (", paste(nec, collapse = ", "),
         "). Columnas leídas: ", paste(names(s2), collapse = " | "))
  }
  s2 <- s2 |>
    filter(trimws(NOMBRE) == NOM_ESTACION) |>
    transmute(fecha = parsear_fecha(FECHA), temp_max_c = num(TMAX), temp_min_c = num(TMIN), temp_media_c = num(TMEDIA))
  tmp <- bind_rows(tmp, s2)
}

# Se unifican ambas fuentes, se recorta al período y se elimina cualquier fecha repetida
tmp <- tmp |>
  filter(!is.na(fecha), fecha >= FECHA_DESDE, fecha <= FECHA_HASTA) |>
  distinct(fecha, .keep_all = TRUE) |>
  arrange(fecha)

# Si falta la media pero existen máxima y mínima, se aproxima como (Tmax + Tmin) / 2
faltan <- is.na(tmp$temp_media_c) & !is.na(tmp$temp_max_c) & !is.na(tmp$temp_min_c)
if (any(faltan)) {
  message("Temperatura media estimada como (Tmax + Tmin)/2 en ", sum(faltan), " días (indicarlo en el informe).")
  tmp$temp_media_c[faltan] <- (tmp$temp_max_c[faltan] + tmp$temp_min_c[faltan]) / 2
}

# Se descartan los registros fuera de las restricciones de dominio de la base
antes <- nrow(tmp)
tmp <- tmp |>
  filter(!is.na(temp_media_c), between(temp_media_c, -30, 55),
         is.na(temp_max_c) | between(temp_max_c, -30, 55),
         is.na(temp_min_c) | between(temp_min_c, -30, 55),
         is.na(temp_max_c) | is.na(temp_min_c) | temp_min_c <= temp_max_c) |>
  mutate(id_estacion = ID_ESTACION) |>
  select(fecha, id_estacion, temp_media_c, temp_max_c, temp_min_c)
if (nrow(tmp) < antes) message("Se descartan ", antes - nrow(tmp), " filas de temperatura sin dato o inválidas")

# Resumen de lo que se va a cargar
comunes <- length(intersect(as.character(dem$fecha), as.character(tmp$fecha)))
cat("\nDemanda:", nrow(dem), "días | Temperatura:", nrow(tmp), "días | Fechas en común:", comunes,
    "| Feriados:", nrow(feriados), "\n")
cat("Período:", format(min(dem$fecha)), "a", format(max(dem$fecha)), "\n")
if (comunes < 365 * 5) warning("Menos de 5 años de fechas en común: sirve para probar la carga, no para el análisis final.")

# Carga a la base (las claves primarias evitan duplicados)
con <- conectar_db()
upsert_tabla(con, data.frame(id_estacion = ID_ESTACION, nombre = NOM_ESTACION, provincia = PROV_ESTACION),
             "estaciones_smn", "id_estacion")
upsert_tabla(con, data.frame(region = REGION, descripcion = DESC_REGION, id_estacion = ID_ESTACION),
             "regiones", "region")
upsert_tabla(con, as.data.frame(dem),  "demanda_cammesa", c("fecha", "region"))
upsert_tabla(con, as.data.frame(tmp),  "temperatura_smn", c("fecha", "id_estacion"))
if (nrow(feriados) > 0) upsert_tabla(con, as.data.frame(feriados), "feriados", "fecha")

n_vista <- dbGetQuery(con, "SELECT count(*) AS n FROM datos_unificados WHERE region = $1",
                      params = list(REGION))$n
cat("Registros disponibles en datos_unificados para", REGION, ":", n_vista, "\n")
cat("Para analizar esta región, usar region = \"", REGION, "\" en PARAM de 00_funciones.R y ejecutar 02 y 03.\n", sep = "")
dbDisconnect(con)