
# Pruebas de contrastación 1 y 2 del informe, con los datos reales:
#   Prueba 1: forma funcional (lineal vs. cuadrático vs. GAM).
#   Prueba 2: forma de "U", temperatura de mínima demanda, variación respecto de un
#             día base de 17 °C e impacto marginal (MW/°C) en calor y frío extremos,
#             con intervalos por simulación de la distribución posterior del GAM.
# También calcula A y P de los tres modelos (insumo parcial de la Prueba 4).

source("00_funciones.R")
dir.create("resultados", showWarnings = FALSE)
set.seed(PARAM$semilla)

con <- conectar_db()
df  <- leer_datos(con)
dbDisconnect(con)

# Ajuste sobre la serie completa: se usa para estimar la relación, no para evaluar el modelo
m     <- ajustar_gam(df)
m_lin <- gam(demanda_mw ~ temp_media_c + dia_semana + es_feriado, data = df, method = "REML")

# Prueba 1: ¿la relación es no lineal?
cat("\n===== 1. ¿Relación no lineal? =====\n")
print(summary(m)$s.table)
cat("\nComparación modelo lineal vs. GAM (test F):\n")
print(anova(m_lin, m, test = "F"))

# Comparación de los tres modelos con los mismos controles (día de la semana y feriado):
# ajuste con el 80 % inicial y evaluación con el 20 % final
partes <- dividir_train_test(df)
modelos <- list(
  Lineal     = lm(demanda_mw ~ temp_media_c + dia_semana + es_feriado, data = partes$train),
  Cuadratico = lm(demanda_mw ~ poly(temp_media_c, 2, raw = TRUE) + dia_semana + es_feriado,
                  data = partes$train),
  GAM        = ajustar_gam(partes$train)
)
r2aj <- c(summary(modelos$Lineal)$adj.r.squared,
          summary(modelos$Cuadratico)$adj.r.squared,
          summary(modelos$GAM)$r.sq)
mape_modelo <- sapply(modelos, function(mod) {
  pred <- as.numeric(predict(mod, newdata = partes$test))
  mean(abs(partes$test$demanda_mw - pred) / partes$test$demanda_mw) * 100
})
comparacion <- data.frame(
  modelo      = names(modelos),
  R2_ajustado = round(r2aj, 4),
  AIC         = round(sapply(modelos, AIC), 1),
  MAPE_prueba_pct = round(mape_modelo, 2)
)
cat("\n===== 1b. Lineal vs. cuadrático vs. GAM (entrenamiento 80 % / prueba 20 %) =====\n")
print(comparacion, row.names = FALSE)
cat("\nTest F: lineal vs. cuadrático (entrenamiento):\n")
print(anova(modelos$Lineal, modelos$Cuadratico))
write_csv(comparacion, "resultados/06_comparacion_modelos.csv")

grilla_cmp <- seq(min(partes$train$temp_media_c), max(partes$train$temp_media_c), by = 0.1)
nd_cmp <- data.frame(temp_media_c = grilla_cmp,
                     dia_semana = factor(PARAM$dia_ref, levels = DIAS), es_feriado = 0L)
curvas_cmp <- do.call(rbind, lapply(names(modelos), function(nm) {
  data.frame(modelo = nm, temp = grilla_cmp,
             y = as.numeric(predict(modelos[[nm]], newdata = nd_cmp)))
}))
g_cmp <- ggplot() +
  geom_point(data = partes$train, aes(temp_media_c, demanda_mw), alpha = 0.2, size = 0.8) +
  geom_line(data = curvas_cmp, aes(temp, y, colour = modelo), linewidth = 1) +
  labs(x = "Temperatura media diaria (°C)", y = "Demanda eléctrica (MW)", colour = "Modelo",
       title = "Comparación de formas funcionales: lineal, cuadrática y GAM") +
  theme_minimal(base_size = 12)
ggsave("resultados/06_comparacion_modelos.png", g_cmp, width = 9, height = 5.5, dpi = 200)

# Prueba 2: curva estimada y temperatura de mínima demanda (zona de confort)
grilla <- seq(min(df$temp_media_c), max(df$temp_media_c), by = 0.1)
curva  <- curva_gam(m, grilla)
t_min  <- curva$temp[which.min(curva$y)]
cat(sprintf("\n===== 2. Mínimo de demanda (zona de confort) en %.1f °C =====\n", t_min))

# Variación porcentual de la demanda respecto de un día base de 17 °C
pct_vs_base <- function(temps, base = 17) {
  y <- curva_gam(m, c(base, temps))$y
  100 * (y[-1] / y[1] - 1)
}
cat("\n===== 3. Variación de demanda respecto de un día base de 17 °C =====\n")
pts <- c(10.5, 24.5)   # valores centrales de los rangos de Martinez Villa (2026)
res_pct <- data.frame(temperatura_c = pts, variacion_pct = round(pct_vs_base(pts), 1))
print(res_pct, row.names = FALSE)
cat("(Bibliografía: +10,1 % a 10-11 °C y +20,8 % a 24-25 °C, AMBA, Martinez Villa 2026)\n")

# Impacto marginal en los extremos térmicos (percentiles 10 y 90 de la serie completa).
# Se simulan 2.000 curvas a partir de la distribución posterior de los coeficientes del GAM;
# la pendiente de cada curva es el cambio de demanda sobre el cambio de temperatura en cada extremo.
p10 <- quantile(df$temp_media_c, 0.10, names = FALSE)
p90 <- quantile(df$temp_media_c, 0.90, names = FALSE)
nd  <- data.frame(temp_media_c = grilla,
                  dia_semana = factor(PARAM$dia_ref, levels = DIAS), es_feriado = 0L)
Xp   <- predict(m, newdata = nd, type = "lpmatrix")
sims <- Xp %*% t(mgcv::rmvn(2000, coef(m), vcov(m)))      # matriz grilla x 2000 curvas

pend_sim <- function(idx) {
  i1 <- min(idx); i2 <- max(idx)
  (sims[i2, ] - sims[i1, ]) / (grilla[i2] - grilla[i1])    # MW/°C
}
s_calor <- pend_sim(which(grilla > p90))
s_frio  <- pend_sim(which(grilla < p10))
ic <- function(x) round(quantile(x, c(0.025, 0.5, 0.975)), 1)   # IC 95 % y mediana

cat(sprintf("\n===== 4. Impacto marginal (MW/°C) | p10 = %.1f °C, p90 = %.1f °C =====\n", p10, p90))
cat("Calor extremo (> p90): "); print(ic(s_calor))
cat("Frío extremo (< p10):  "); print(ic(s_frio))
cat("\nRazón |calor| / |frío| (IC 95 %):\n"); print(round(quantile(s_calor / abs(s_frio), c(0.025, 0.5, 0.975)), 2))
# Proporción de curvas simuladas que cumplen cada condición
cat(sprintf("\nP(forma de U: pendiente de frío < 0 y de calor > 0) = %.3f\n", mean(s_frio < 0 & s_calor > 0)))
cat(sprintf("P(sensibilidad en calor > sensibilidad en frío)      = %.3f\n", mean(s_calor > abs(s_frio))))

# Gráfico final del informe (Figura 4)
g <- ggplot(curva, aes(temp, y)) +
  geom_point(data = df, aes(temp_media_c, demanda_mw), alpha = 0.2, size = 0.8) +
  geom_ribbon(aes(ymin = y - 1.96 * se, ymax = y + 1.96 * se), fill = "firebrick", alpha = 0.25) +
  geom_line(colour = "firebrick", linewidth = 1) +
  geom_vline(xintercept = c(p10, p90), linetype = "dashed", colour = "grey40") +
  geom_vline(xintercept = t_min, linetype = "dotted", colour = "steelblue") +
  labs(x = "Temperatura media diaria (°C)", y = "Demanda eléctrica (MW)",
       title = "Respuesta de la demanda a la temperatura (GAM, serie completa)",
       subtitle = "Punteadas grises: percentiles 10 y 90 | Punteada azul: mínimo de demanda") +
  theme_minimal(base_size = 12)
ggsave("resultados/06_curva_final.png", g, width = 9, height = 5.5, dpi = 200)

write_csv(data.frame(
  indicador = c("t_minimo_C", "var_pct_10.5C", "var_pct_24.5C",
                "pend_calor_mediana_MW_C", "pend_frio_mediana_MW_C"),
  valor = c(t_min, res_pct$variacion_pct, median(s_calor), median(s_frio))
), "resultados/06_resultados_hipotesis.csv")
cat("\nListo. Resultados en 'resultados/'.\n")