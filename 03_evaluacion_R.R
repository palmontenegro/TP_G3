# =====================================================================
# 03_evaluacion_R.R
# Evaluación del diseño: calcula A, P, E y T, la eficiencia
#   Eficiencia = 0,25*A + 0,35*P + 0,25*E + 0,15*T
# y la compara con el umbral 0,70. Guarda el resultado en la tabla
# 'evaluaciones' y en resultados/evaluacion.csv
# =====================================================================

source("00_funciones.R")
dir.create("resultados", showWarnings = FALSE)
set.seed(PARAM$semilla)

con <- conectar_db()

# ---- A, P y E: una única ejecución del flujo ----
res <- ejecutar_flujo(con, PARAM$region)

# ---- T: 5 repeticiones del flujo completo (consulta a la base incluida) ----
cat("Midiendo el tiempo de ejecución (", PARAM$repeticiones_t, "repeticiones)...\n")
tt <- medir_T(con, PARAM$region, PARAM$repeticiones_t)

# ---- Eficiencia ----
efi    <- calcular_eficiencia(res$A, res$P, res$E, tt$T_val)
cumple <- efi >= PARAM$umbral

tabla <- data.frame(
  variable    = c("A (ajuste)", "P (precisión predictiva)", "E (estabilidad)", "T (tiempo)"),
  valor       = round(c(res$A, res$P, res$E, tt$T_val), 4),
  ponderacion = unname(PARAM$pesos)
)
tabla$aporte <- round(tabla$valor * tabla$ponderacion, 4)

cat("\n===================== EVALUACIÓN DEL DISEÑO =====================\n")
cat("Región:", PARAM$region, "\n\n")
print(tabla, row.names = FALSE)
cat(sprintf("\nMAPE sobre el conjunto de prueba : %.2f %%\n", res$mape))
cat(sprintf("E_calor = %.3f | E_frío = %.3f\n", res$E_calor, res$E_frio))
cat(sprintf("Pendientes (MW/°C) -> calor: meses pares %.1f, impares %.1f | frío: pares %.1f, impares %.1f\n",
            res$pendientes["calor", 1], res$pendientes["calor", 2],
            res$pendientes["frio", 1],  res$pendientes["frio", 2]))
cat(sprintf("Tiempos (s): %s | promedio = %.2f s\n",
            paste(round(tt$tiempos, 2), collapse = ", "), tt$t_prom))
cat(sprintf("\nEFICIENCIA = %.4f  (umbral %.2f)  ->  %s\n", efi, PARAM$umbral,
            if (cumple) "CUMPLE la condición" else "NO cumple la condición"))
if (res$P == 0) cat("Atención: P = 0 (MAPE >= 10 %), la eficiencia no puede alcanzar el umbral.\n")
cat("=================================================================\n")

# ---- Registro en la base y en CSV ----
registrar_evaluacion(con, PARAM$region, res$A, res$P, res$E, tt$T_val,
                     res$mape, tt$t_prom,
                     notas = paste("Ejecución", format(Sys.time(), "%Y-%m-%d %H:%M")))
write_csv(cbind(tabla, eficiencia = efi, cumple_umbral = cumple),
          "resultados/evaluacion.csv")

dbDisconnect(con)
