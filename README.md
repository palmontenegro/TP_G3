# Demanda eléctrica y temperatura: prototipo de análisis (Grupo 3)

Trabajo Práctico N°4 de **Metodología del Aprendizaje** (ITBA, segundo cuatrimestre de 2026), Comisión S1, Grupo 3.

Este repositorio contiene el código del prototipo descripto en la Introducción y en Materiales y Métodos del informe. El prototipo estima la relación entre la **demanda eléctrica diaria** (CAMMESA) y la **temperatura media diaria** (SMN) con un modelo aditivo generalizado (GAM) y evalúa la eficiencia del diseño analítico con cuatro variables (A, P, E y T).

**Integrantes:** Paloma Montenegro, Mariam Mawarí Guerra Morales, Helena Buraglia, Mila Gotlib y Fiorella Del Papa.

---

## Contenido del repositorio

| Archivo | Qué hace |
|---|---|
| `01_schema_supabase.sql` | Crea en Supabase las 6 tablas (`estaciones_smn`, `regiones`, `demanda_cammesa`, `temperatura_smn`, `feriados`, `evaluaciones`) y la vista `datos_unificados`. |
| `00_funciones.R` | Parámetros del diseño (`PARAM`) y funciones compartidas: conexión, lectura, división 80/20, GAM, variables A, P, E y T, y eficiencia. No se ejecuta solo. |
| `04_base_de_prueba.R` | Genera y carga 366 días simulados de 2024 (región `PRUEBA_AMBA`). Sirve solo para verificar que el flujo funciona. |
| `05_cargar_datos_reales.R` | Lee los archivos reales de CAMMESA y del SMN y los carga en la base. |
| `02_prototipo_R.R` | Gráficos exploratorios, división 80/20, ajuste del GAM, diagnóstico y predicción sobre el conjunto de prueba. |
| `03_evaluacion_R.R` | Calcula A, P, E y T, la eficiencia, y la compara con el umbral de 0,70. |
| `06_resultados_hipotesis.R` | Compara los modelos lineal, cuadrático y GAM, y estima la forma de la curva, el mínimo de demanda, la variación respecto de 17 °C y las pendientes en calor y frío extremos. |
| `datos/` | Carpeta donde se colocan los archivos de entrada. |
| `resultados/` | Carpeta donde los scripts guardan gráficos y tablas. |

---

## Requisitos

- **R** (versión 4.6.1 en las pruebas del informe) y **RStudio**.
- Una cuenta en **Supabase** (plan gratuito alcanza).
- Paquetes de R:

```r
install.packages(c("DBI", "RPostgres", "dplyr", "lubridate", "readr",
                   "ggplot2", "mgcv", "readxl"))
```

---

## Cómo reproducir el análisis

### Paso 1. Crear la base de datos en Supabase

1. Crear un proyecto en [supabase.com](https://supabase.com) y guardar la contraseña de la base.
2. Abrir **SQL Editor → New query**, pegar todo el contenido de `01_schema_supabase.sql` y ejecutar (**Run**). Se ejecuta una sola vez.
3. Abrir el botón **Connect** del proyecto y copiar los datos de **Session pooler**: host, puerto, base y usuario (con la forma `postgres.xxxxxxxx`). Usar el puerto **5432**; el puerto 6543 no sirve porque la carga usa tablas temporales.

### Paso 2. Configurar las credenciales

Crear en la carpeta del proyecto un archivo llamado `.Renviron` con este contenido, reemplazando los valores:

```
SUPABASE_HOST=aws-0-xxxx.pooler.supabase.com
SUPABASE_PORT=5432
SUPABASE_DB=postgres
SUPABASE_USER=postgres.xxxxxxxx
SUPABASE_PASSWORD=la_contraseña_de_la_base
```

Después reiniciar R (**Session → Restart R**).

> **Importante:** el archivo `.Renviron` contiene la contraseña de la base y **nunca debe subirse a GitHub**. Debe figurar en el `.gitignore`. Los scripts leen las credenciales de las variables de entorno y no las incluyen en el código.

### Paso 3. Verificar el flujo con datos de prueba (opcional)

1. En RStudio: **File → New Project → Existing Directory** y elegir la carpeta del repositorio.
2. Ejecutar `04_base_de_prueba.R`. Debe informar 366 registros.
3. En `00_funciones.R`, cambiar `region = "GBA"` por `region = "PRUEBA_AMBA"`.
4. Ejecutar `02_prototipo_R.R` y `03_evaluacion_R.R`.

Los datos son simulados y el conjunto de prueba abarca solo parte del año, por lo que los valores de P y E obtenidos con esta base **no tienen significado**: solo se comprueba que el flujo corre. Antes de seguir, volver a poner `region = "GBA"`.

### Paso 4. Cargar los datos reales

Descargar los archivos y copiarlos en la carpeta `datos/`:

| Fuente | Archivo | Qué se usa |
|---|---|---|
| CAMMESA | Base de demanda diaria 2017-2026 (Excel, el nombre debe contener `DEMANDA_DIARIA` y terminar en `.xlsx`) | Hoja *Datos Región*, columna **GRAN BS.AS.** (demanda neta media diaria, en MW) y columna **Tipo día** (para identificar feriados). |
| SMN | Datos meteorológicos diarios en Excel, hasta 2020 (el nombre debe contener `SMN_DATOS` y terminar en `.xlsx`) | Columnas `NOMBRE`, `FECHA`, `TMAX`, `TMIN` y `TMEDIA` de la estación **AEROPARQUE AERO** (código OMM 87582). |
| SMN | Datos meteorológicos diarios desde 2021 (archivo `.lst`, separado por tabuladores) | Las mismas columnas. |

Las fuentes están citadas en la Bibliografía del informe. El script busca los archivos por patrón de nombre, de modo que no importa si el nombre tiene espacios o un `(1)` al final.

Luego ejecutar `05_cargar_datos_reales.R`. Al terminar, la consola debe informar **3.287** fechas en común (1/1/2017 al 31/12/2025). Revisar que no aparezcan mensajes de filas descartadas ni de temperatura media estimada como (Tmax + Tmin)/2; si aparecen, hay que declararlo en el informe.

Decisiones de datos que se aplican en el script (configurables en su sección `CONFIGURACIÓN`):

- Región `GBA`, estación de referencia Aeroparque.
- Período: 1 de enero de 2017 al 31 de diciembre de 2025 (años completos). No se excluye ningún intervalo, en particular 2020.
- Feriados: días que CAMMESA clasifica como "Domingo o Feriado" y que no caen en domingo.

### Paso 5. Ejecutar el análisis

Con `region = "GBA"` en `00_funciones.R`, ejecutar en este orden:

1. `02_prototipo_R.R`: gráficos, GAM y predicción sobre el conjunto de prueba.
2. `03_evaluacion_R.R`: A, P, E, T y eficiencia. Repite 5 veces el flujo completo para medir T, por lo que tarda más.
3. `06_resultados_hipotesis.R`: comparación de modelos y estimación de la curva.

---

## Resultados generados

Todo se guarda en la carpeta `resultados/`.

| Archivo | Contenido | Figura o tabla del informe |
|---|---|---|
| `02_series_temporales.png` | Series de demanda y temperatura | Figura 2 |
| `01_dispersion_demanda_temperatura.png` | Demanda vs. temperatura, sin controles de día ni feriado | Figura 3 |
| `06_curva_final.png` | Curva del GAM sobre la serie completa (día hábil no feriado) | Figura 4 |
| `06_comparacion_modelos.png` | Curvas lineal, cuadrática y GAM (entrenamiento) | Figura 5 |
| `05_prueba_observada_vs_predicha.png` | Demanda observada y predicha en el conjunto de prueba | Figura 6 |
| `06_resultados_hipotesis.csv` | Mínimo, variación respecto de 17 °C y pendientes | Tabla 1 |
| `06_comparacion_modelos.csv` | R² ajustado, AIC y MAPE de los tres modelos | Tabla 2 |
| `evaluacion.csv` | A, P, E, T, ponderaciones y eficiencia | Tabla 3 |
| `03_diagnostico_gam.png` | Gráficos de diagnóstico de residuos del GAM | (no incluido en el informe) |
| `04_curva_gam.png` | Curva del GAM sobre el conjunto de entrenamiento | (no incluido en el informe) |

---

## Modelo y variables de evaluación

El modelo es un GAM gaussiano con estimación REML:

```r
demanda_mw ~ s(temp_media_c) + dia_semana + es_feriado
```

Los datos se dividen cronológicamente: el 80 % inicial para entrenamiento y el 20 % final para prueba. Las variables de evaluación son adimensionales y están acotadas entre 0 (peor) y 1 (mejor):

| Variable | Definición | Fórmula |
|---|---|---|
| **A** (ajuste) | R² ajustado del GAM en el conjunto de entrenamiento | A = máx(0; R²aj) |
| **P** (precisión predictiva) | Error porcentual absoluto medio (MAPE) en el conjunto de prueba | P = máx(0; 1 − MAPE / 10 %) |
| **E** (estabilidad) | Similitud de las pendientes de la curva en calor (> percentil 90) y frío (< percentil 10) al ajustar por separado los meses pares y los impares | E = (E_calor + E_frío) / 2 |
| **T** (tiempo de ejecución) | Promedio de 5 ejecuciones del flujo completo, desde la consulta a la base hasta el cálculo de A, P y E | T = máx(0; 1 − t / 300 s) |

**Eficiencia = 0,25·A + 0,35·P + 0,25·E + 0,15·T.** El diseño cumple la condición si la eficiencia es mayor o igual que **0,70**.

Los parámetros (proporción 80/20, MAPE de referencia, tiempo máximo, ponderaciones, umbral, día de referencia y semilla) están en la lista `PARAM` de `00_funciones.R`.

---

## Notas

- **T depende del equipo y de la conexión a internet**, porque el flujo incluye la consulta a Supabase. El equipo de referencia utilizado se describe en Materiales y Métodos del informe.
- La semilla (`PARAM$semilla`) solo afecta a la base de prueba y a las simulaciones de `06_resultados_hipotesis.R`; el ajuste del GAM es determinístico.
- Los datos de CAMMESA y del SMN pertenecen a sus respectivas fuentes y deben citarse al reutilizarlos.
