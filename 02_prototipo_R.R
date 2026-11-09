# Flujo de análisis: lectura de datos_unificados, gráficos exploratorios,
# división cronológica 80/20, ajuste del GAM, diagnóstico y predicción.
# Los gráficos se guardan en la carpeta resultados/.

source("00_funciones.R")
dir.create("resultados", showWarnings = FALSE)

con <- conectar_db()
df  <- leer_datos(con)
dbDisconnect(con)

cat("Región:", PARAM$region, "| Observaciones:", nrow(df),
    "| Período:", format(min(df$fecha)), "a", format(max(df$fecha)), "\n")
print(summary(df[, c("demanda_mw", "temp_media_c")]))

# 1. Dispersión de la demanda en función de la temperatura, con curva suave
# (todos los días, sin controles de día de la semana ni de feriado)
g1 <- ggplot(df, aes(temp_media_c, demanda_mw)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), colour = "firebrick") +
  labs(x = "Temperatura media diaria (°C)", y = "Demanda eléctrica (MW)",
       title = "Demanda eléctrica en función de la temperatura") +
  theme_minimal(base_size = 12)
ggsave("resultados/01_dispersion_demanda_temperatura.png", g1, width = 8, height = 5, dpi = 200)

# 2. Series temporales de demanda y temperatura.
# La temperatura se reescala al rango de la demanda para dibujarla sobre el mismo
# gráfico; el eje secundario deshace esa transformación y muestra °C.
coef <- diff(range(df$demanda_mw)) / diff(range(df$temp_media_c))
dmin <- min(df$demanda_mw); tmin <- min(df$temp_media_c)
g2 <- ggplot(df, aes(fecha)) +
  geom_line(aes(y = demanda_mw), colour = "steelblue", linewidth = 0.4) +
  geom_line(aes(y = (temp_media_c - tmin) * coef + dmin), colour = "darkorange",
            linewidth = 0.4, alpha = 0.8) +
  scale_y_continuous(name = "Demanda eléctrica (MW)",
                     sec.axis = sec_axis(~ (. - dmin) / coef + tmin, name = "Temperatura media (°C)")) +
  labs(x = NULL, title = "Series temporales de demanda (azul) y temperatura (naranja)") +
  theme_minimal(base_size = 12)
ggsave("resultados/02_series_temporales.png", g2, width = 10, height = 4.5, dpi = 200)

# 3. División cronológica y ajuste del GAM sobre el conjunto de entrenamiento
partes <- dividir_train_test(df)
cat("\nEntrenamiento:", nrow(partes$train), "obs (",
    format(min(partes$train$fecha)), "a", format(max(partes$train$fecha)), ")\n")
cat("Prueba:       ", nrow(partes$test), "obs (",
    format(min(partes$test$fecha)), "a", format(max(partes$test$fecha)), ")\n")

modelo <- ajustar_gam(partes$train)
cat("\n===== Resumen del GAM (conjunto de entrenamiento) =====\n")
print(summary(modelo))

# Diagnóstico de residuos y de la suficiencia de la base del spline
png("resultados/03_diagnostico_gam.png", width = 1400, height = 1000, res = 150)
par(mfrow = c(2, 2)); gam.check(modelo); par(mfrow = c(1, 1))
dev.off()

# 4. Curva ajustada con intervalo de confianza del 95 % (±1,96 errores estándar)
grilla <- seq(min(partes$train$temp_media_c), max(partes$train$temp_media_c), by = 0.1)
curva  <- curva_gam(modelo, grilla)
g4 <- ggplot() +
  geom_point(data = partes$train, aes(temp_media_c, demanda_mw), alpha = 0.25) +
  geom_ribbon(data = curva, aes(temp, ymin = y - 1.96 * se, ymax = y + 1.96 * se),
              fill = "firebrick", alpha = 0.25) +
  geom_line(data = curva, aes(temp, y), colour = "firebrick", linewidth = 1) +
  labs(x = "Temperatura media diaria (°C)", y = "Demanda eléctrica (MW)",
       title = "Curva GAM ajustada (día hábil de referencia, no feriado)") +
  theme_minimal(base_size = 12)
ggsave("resultados/04_curva_gam.png", g4, width = 8, height = 5, dpi = 200)

# 5. Predicción sobre el conjunto de prueba (datos no usados en el ajuste)
res_P <- calcular_P(modelo, partes$test)
cat(sprintf("\nMAPE sobre el conjunto de prueba: %.2f %%\n", res_P$mape))

g5 <- ggplot(partes$test, aes(fecha)) +
  geom_line(aes(y = demanda_mw, colour = "Observada")) +
  geom_line(aes(y = res_P$pred, colour = "Predicha por el GAM")) +
  scale_colour_manual(values = c("Observada" = "steelblue", "Predicha por el GAM" = "firebrick")) +
  labs(x = NULL, y = "Demanda eléctrica (MW)", colour = NULL,
       title = "Conjunto de prueba: demanda observada vs. predicha") +
  theme_minimal(base_size = 12) + theme(legend.position = "bottom")
ggsave("resultados/05_prueba_observada_vs_predicha.png", g5, width = 9, height = 4.5, dpi = 200)

cat("\nListo. Gráficos guardados en la carpeta 'resultados/'.\n")