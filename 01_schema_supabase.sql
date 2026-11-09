-- Esquema de la base de datos del prototipo (PostgreSQL en Supabase).
-- Se ejecuta una única vez: SQL Editor > New query > pegar > Run.

-- Tablas de datos de entrada

CREATE TABLE IF NOT EXISTS estaciones_smn (
    id_estacion  TEXT PRIMARY KEY,
    nombre       TEXT NOT NULL,
    provincia    TEXT
);

-- Asigna una estación meteorológica de referencia a cada región
CREATE TABLE IF NOT EXISTS regiones (
    region       TEXT PRIMARY KEY,
    descripcion  TEXT,
    id_estacion  TEXT NOT NULL REFERENCES estaciones_smn (id_estacion)
);

-- Demanda neta media diaria por región (MW). La clave primaria impide duplicados.
CREATE TABLE IF NOT EXISTS demanda_cammesa (
    fecha        DATE    NOT NULL,
    region       TEXT    NOT NULL REFERENCES regiones (region),
    demanda_mw   NUMERIC NOT NULL CHECK (demanda_mw > 0),
    PRIMARY KEY (fecha, region)
);

-- Temperatura diaria por estación (°C). Las restricciones CHECK validan el dominio de los datos.
CREATE TABLE IF NOT EXISTS temperatura_smn (
    fecha         DATE    NOT NULL,
    id_estacion   TEXT    NOT NULL REFERENCES estaciones_smn (id_estacion),
    temp_media_c  NUMERIC NOT NULL CHECK (temp_media_c BETWEEN -30 AND 55),
    temp_max_c    NUMERIC CHECK (temp_max_c BETWEEN -30 AND 55),
    temp_min_c    NUMERIC CHECK (temp_min_c BETWEEN -30 AND 55),
    PRIMARY KEY (fecha, id_estacion),
    CHECK (temp_min_c IS NULL OR temp_max_c IS NULL OR temp_min_c <= temp_max_c)
);

CREATE TABLE IF NOT EXISTS feriados (
    fecha        DATE PRIMARY KEY,
    descripcion  TEXT
);

-- Resultados de la evaluación del diseño: una fila por ejecución.
-- Las columnas a, p, e, t son las variables de evaluación (entre 0 y 1);
-- eficiencia y cumple_umbral se calculan con las ponderaciones y el umbral de la Introducción.
CREATE TABLE IF NOT EXISTS evaluaciones (
    id_evaluacion    BIGSERIAL PRIMARY KEY,
    fecha_ejecucion  TIMESTAMPTZ NOT NULL DEFAULT now(),
    region           TEXT NOT NULL REFERENCES regiones (region),
    a                NUMERIC NOT NULL CHECK (a BETWEEN 0 AND 1),
    p                NUMERIC NOT NULL CHECK (p BETWEEN 0 AND 1),
    e                NUMERIC NOT NULL CHECK (e BETWEEN 0 AND 1),
    t                NUMERIC NOT NULL CHECK (t BETWEEN 0 AND 1),
    mape_pct         NUMERIC,
    t_promedio_s     NUMERIC,
    eficiencia       NUMERIC GENERATED ALWAYS AS
                     (0.25 * a + 0.35 * p + 0.25 * e + 0.15 * t) STORED,
    cumple_umbral    BOOLEAN GENERATED ALWAYS AS
                     ((0.25 * a + 0.35 * p + 0.25 * e + 0.15 * t) >= 0.70) STORED,
    notas            TEXT
);

-- Vista que unifica demanda y temperatura por fecha y región
-- (a través de la estación de referencia de cada región) y deriva
-- día de la semana, condición de feriado, mes y estación del año (hemisferio sur).
CREATE OR REPLACE VIEW datos_unificados AS
SELECT
    d.fecha,
    d.region,
    d.demanda_mw,
    t.temp_media_c,
    t.temp_max_c,
    t.temp_min_c,
    CASE EXTRACT(ISODOW FROM d.fecha)::int
        WHEN 1 THEN 'Lunes'
        WHEN 2 THEN 'Martes'
        WHEN 3 THEN 'Miercoles'
        WHEN 4 THEN 'Jueves'
        WHEN 5 THEN 'Viernes'
        WHEN 6 THEN 'Sabado'
        ELSE 'Domingo'
    END                                          AS dia_semana,
    (f.fecha IS NOT NULL)::int                   AS es_feriado,
    EXTRACT(MONTH FROM d.fecha)::int             AS mes,
    CASE
        WHEN EXTRACT(MONTH FROM d.fecha) IN (12, 1, 2)  THEN 'Verano'
        WHEN EXTRACT(MONTH FROM d.fecha) IN (3, 4, 5)   THEN 'Otono'
        WHEN EXTRACT(MONTH FROM d.fecha) IN (6, 7, 8)   THEN 'Invierno'
        ELSE 'Primavera'
    END                                          AS estacion
FROM demanda_cammesa d
JOIN regiones r          ON r.region = d.region
JOIN temperatura_smn t   ON t.id_estacion = r.id_estacion
                        AND t.fecha = d.fecha
LEFT JOIN feriados f     ON f.fecha = d.fecha;

-- Seguridad: Supabase expone las tablas del esquema public por su API REST.
-- Activar RLS sin políticas bloquea el acceso con la clave pública; la conexión
-- directa desde R (usuario postgres) no se ve afectada.
ALTER TABLE estaciones_smn   ENABLE ROW LEVEL SECURITY;
ALTER TABLE regiones         ENABLE ROW LEVEL SECURITY;
ALTER TABLE demanda_cammesa  ENABLE ROW LEVEL SECURITY;
ALTER TABLE temperatura_smn  ENABLE ROW LEVEL SECURITY;
ALTER TABLE feriados         ENABLE ROW LEVEL SECURITY;
ALTER TABLE evaluaciones     ENABLE ROW LEVEL SECURITY;
ALTER VIEW  datos_unificados SET (security_invoker = on);