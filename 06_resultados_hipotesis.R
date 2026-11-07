# =====================================================================
# 06_resultados_hipotesis.R
# Responde al OBJETIVO y a la HIPÓTESIS del informe con los datos reales:
#   - ¿Hay no linealidad? (GAM vs. modelo lineal)
#   - ¿Forma de "U"? ¿Dónde está el mínimo (zona de confort)?
#   - Aumento de demanda respecto de un día base de 17 °C (comparable con
#     Martínez Villa, 2026: +20,8 % a 24-25 °C y +10,1 % a 10-11 °C)
#   - Impacto marginal (MW/°C) en calor extremo (> p90) y frío extremo (< p10)
#     con intervalos de confianza por simulación del posterior del GAM.
# Ejecutar con PARAM$region apuntando a la región REAL.
# =====================================================================

source("00_funciones.R")
dir.create("resultados", showWarnings = FALSE)
set.seed(PARAM$semilla)

con <- conectar_db()
df  <- leer_datos(con)
dbDisconnect(con)

# Se ajusta con la serie completa (estimación de la relación, no evaluación)
m     <- ajustar_gam(df)
m_lin <- gam(demanda_mw ~ temp_media_c + dia_semana + es_feriado, data = df, method = "REML")

cat("\n===== 1. ¿Relación no lineal? =====\n")
print(summary(m)$s.table)
cat("\nComparación modelo lineal vs. GAM (test F):\n")
print(anova(m_lin, m, test = "F"))

# ---- Comparación de modelos: lineal vs. cuadrático vs. GAM ----
# Justifica por qué se usa un GAM. Los tres modelos llevan los mismos controles
# (día de la semana y feriado) y se ajustan con el 80 % inicial y se evalúan con el 20 % final.
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

# ---- Curva y zona de confort ----
grilla <- seq(min(df$temp_media_c), max(df$temp_media_c), by = 0.1)
curva  <- curva_gam(m, grilla)
t_min  <- curva$temp[which.min(curva$y)]
cat(sprintf("\n===== 2. Mínimo de demanda (zona de confort) en %.1f °C =====\n", t_min))

# ---- Aumento porcentual respecto del día base de 17 °C ----
pct_vs_base <- function(temps, base = 17) {
  y <- curva_gam(m, c(base, temps))$y
  100 * (y[-1] / y[1] - 1)
}
cat("\n===== 3. Variación de demanda respecto de un día base de 17 °C =====\n")
pts <- c(10.5, 24.5)
res_pct <- data.frame(temperatura_c = pts, variacion_pct = round(pct_vs_base(pts), 1))
print(res_pct, row.names = FALSE)
cat("(Referencia de la bibliografía: +10,1 % a 10-11 °C y +20,8 % a 24-25 °C, AMBA, Martínez Villa 2026)\n")

# ---- Impacto marginal en extremos con IC por simulación ----
p10 <- quantile(df$temp_media_c, 0.10, names = FALSE)
p90 <- quantile(df$temp_media_c, 0.90, names = FALSE)
nd  <- data.frame(temp_media_c = grilla,
                  dia_semana = factor(PARAM$dia_ref, levels = DIAS), es_feriado = 0L)
Xp   <- predict(m, newdata = nd, type = "lpmatrix")
sims <- Xp %*% t(mgcv::rmvn(2000, coef(m), vcov(m)))      # curvas simuladas (grilla x 2000)

pend_sim <- function(idx) {
  i1 <- min(idx); i2 <- max(idx)
  (sims[i2, ] - sims[i1, ]) / (grilla[i2] - grilla[i1])    # pendiente media, MW/°C
}
s_calor <- pend_sim(which(grilla > p90))
s_frio  <- pend_sim(which(grilla < p10))
ic <- function(x) round(quantile(x, c(0.025, 0.5, 0.975)), 1)

cat(sprintf("\n===== 4. Impacto marginal (MW/°C) | p10 = %.1f °C, p90 = %.1f °C =====\n", p10, p90))
cat("Calor extremo (> p90): "); print(ic(s_calor))
cat("Frío extremo (< p10):  "); print(ic(s_frio))
cat("\nRazón |calor| / |frío| (IC 95 %):\n"); print(round(quantile(s_calor / abs(s_frio), c(0.025, 0.5, 0.975)), 2))
cat(sprintf("\nP(forma de U: pendiente de frío < 0 y de calor > 0) = %.3f\n", mean(s_frio < 0 & s_calor > 0)))
cat(sprintf("P(sensibilidad en calor > sensibilidad en frío)      = %.3f\n", mean(s_calor > abs(s_frio))))

# ---- Gráfico final ----
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
