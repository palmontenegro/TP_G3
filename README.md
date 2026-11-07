# Prototipo TP3 – Demanda eléctrica vs. temperatura (Grupo 3)

Código del prototipo descripto en la Introducción y en Materiales y Métodos del informe.

## Archivos

| Archivo | Qué hace |
|---|---|
| `01_schema_supabase.sql` | Crea las 6 tablas y la vista `datos_unificados` en Supabase |
| `00_funciones.R` | Parámetros del diseño y funciones (conexión, GAM, A, P, E, T, eficiencia) |
| `04_base_de_prueba.R` | Genera y carga 366 días simulados de 2024 (región `PRUEBA_AMBA`) |
| `02_prototipo_R.R` | Gráficos, división 80/20, ajuste del GAM, predicción en prueba |
| `03_evaluacion_R.R` | Calcula A, P, E, T, la eficiencia y la compara con 0,70 |
| `05_cargar_datos_reales.R` | Carga los CSV reales de CAMMESA y SMN a la base |
| `06_resultados_hipotesis.R` | Lineal vs. cuadrático vs. GAM, forma de "U", % vs. 17 °C, MW/°C en calor/frío con IC |
| `Renviron_ejemplo.txt` | Plantilla de credenciales |

## Paso 1 – Instalar R y paquetes (una vez)

En RStudio:

```r
install.packages(c("DBI", "RPostgres", "dplyr", "lubridate", "readr",
                   "ggplot2", "mgcv", "jsonlite"))
```

## Paso 2 – Crear la base en Supabase

1. Entrá a supabase.com, creá un proyecto y guardá la contraseña de la base.
2. Abrí **SQL Editor → New query**, pegá todo `01_schema_supabase.sql` y ejecutá (**Run**).
3. Buscá los datos de conexión: botón **Connect** del proyecto → **Session pooler** (puerto 5432). Ahí vas a ver host, puerto, base y usuario (`postgres.xxxxxxxx`).
   Usá el puerto **5432** (sesión o conexión directa), no el 6543: el script de carga usa tablas temporales.
4. Copiá `Renviron_ejemplo.txt` como `.Renviron` en la carpeta del proyecto, completalo y **reiniciá R** (Session → Restart R).

## Paso 3 – Verificar que todo funciona con datos de prueba

1. En RStudio: **File → New Project → Existing Directory** y elegí esta carpeta.
2. Ejecutar en orden: `04_base_de_prueba.R` → `02_prototipo_R.R` → `03_evaluacion_R.R`.
3. Debe imprimir "366" registros, generar los PNG en `resultados/` y mostrar la tabla de eficiencia.

Con un solo año de datos simulados, el 20 % de prueba cae al final del año (otoño-primavera) y puede quedar fuera del rango de temperatura del entrenamiento: los valores de P y E de la prueba **no significan nada**. Solo se verifica que el flujo corre.

## Paso 4 – Datos reales (esto lo hacen ustedes)

**Qué necesitan:**
1. **Demanda diaria** de CAMMESA para una región (p. ej. AMBA/Gran Buenos Aires) y un período.
2. **Temperatura diaria** del SMN de **una** estación de referencia de esa región (datos abiertos: datos.smn.gob.ar).
3. (Opcional) feriados; el script intenta bajarlos solo.

**Decisiones que tienen que tomar y registrar en Materiales y Métodos:**
- Región y estación (las que hoy figuran como `[completar]` en el informe).
- Período: **mínimo 5 años** (p. ej. 2019-2025). Con el corte cronológico 80/20, el 20 % final equivale a más de un año y cubre todas las estaciones del año. Con menos, P y E se distorsionan. Consideren qué hacer con 2020 (pandemia).
- Qué es "demanda diaria": pico diario (`max`) o promedio (`mean`). Se configura en `AGREGACION_DEMANDA`.
- Si el SMN no entrega temperatura media, el script usa (Tmax + Tmin)/2 y hay que aclararlo en el informe.

**Cómo cargarlos:**
1. Guardá los archivos como CSV en una carpeta `datos/` (`demanda_cammesa.csv`, `temperatura_smn.csv`).
2. Abrí `05_cargar_datos_reales.R` y completá la sección **CONFIGURACIÓN** (nombres de columnas, separador, estación, período).
3. Ejecutalo.
4. En `00_funciones.R` cambiá `region = "PRUEBA_AMBA"` por tu región (p. ej. `"AMBA"`).
5. Ejecutá `02_prototipo_R.R`, `03_evaluacion_R.R` y `06_resultados_hipotesis.R`.

Los formatos de descarga de CAMMESA y del SMN cambian y no los pude verificar desde acá. Si el script no los lee, pasame los nombres de las columnas y las primeras filas de cada archivo y adapto la sección de lectura.

## Paso 5 – Completar los `[completar]` del informe

```r
R.version.string                         # versión de R
parallel::detectCores()                  # núcleos del procesador
Sys.info()[c("sysname", "release", "machine")]   # sistema operativo
# Memoria RAM: Windows -> Administrador de tareas > Rendimiento; Mac -> Acerca de esta Mac
```

Repositorio: subí los scripts `.R` y `.sql` a GitHub **sin** el `.Renviron` ni la carpeta `datos/` si no corresponde, y pegá el enlace en la sección de Flujo de análisis.

## Cómo se calcula cada variable (coincide con el informe)

- **A** = máx(0, R² ajustado del GAM en entrenamiento).
- **P** = máx(0, 1 − MAPE/10 %), MAPE sobre el 20 % final.
- **E** = promedio de las similitudes de pendiente (calor > p90, frío < p10) entre meses pares e impares.
- **T** = máx(0, 1 − t/300 s), t = promedio de 5 ejecuciones del flujo completo (incluye la consulta a la base).
- **Eficiencia** = 0,25·A + 0,35·P + 0,25·E + 0,15·T; cumple si ≥ 0,70.
