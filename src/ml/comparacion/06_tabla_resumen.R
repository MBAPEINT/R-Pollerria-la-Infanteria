# ============================================================
# 06_TABLA_RESUMEN.R
# Compila los 5 CSVs de comparacion y genera:
# - Tabla resumen con scores ponderados
# - Heatmap de ganadores
# - CSV final
# ============================================================

source("src/ml/comparacion/utils_comparacion.R")
iniciar_script("TABLA RESUMEN - COMPARACION MULTI-MODELO")

cat("\n[1/4] Cargando resultados de cada comparacion...\n")

# Leer CSVs (si existen)
leer_csv_seguro <- function(ruta) {
  if (file.exists(ruta)) {
    read.csv(ruta, stringsAsFactors = FALSE)
  } else {
    cat(sprintf("  [AVISO] No encontrado: %s\n", ruta))
    NULL
  }
}

comp_ventas       <- leer_csv_seguro("data/output/comparacion/comparacion_ventas.csv")
comp_segmentacion <- leer_csv_seguro("data/output/comparacion/comparacion_segmentacion.csv")
comp_combos       <- leer_csv_seguro("data/output/comparacion/comparacion_combos.csv")
comp_agotamiento  <- leer_csv_seguro("data/output/comparacion/comparacion_agotamiento.csv")
comp_anomalias    <- leer_csv_seguro("data/output/comparacion/comparacion_anomalias.csv")

# ---- 2. Calcular scores ponderados ----
cat("\n[2/4] Aplicando criterios ponderados...\n")

tabla_final <- data.frame()

# --- Ventas ---
if (!is.null(comp_ventas)) {
  cat("  - Ventas...\n")
  criterios_ventas <- data.frame(
    metrica = c("MAPE", "RMSE", "R2"),
    peso    = c(0.40, 0.25, 0.20),
    menor_mejor = c(TRUE, TRUE, FALSE),
    stringsAsFactors = FALSE
  )
  scores_v <- calcular_score_ponderado(comp_ventas, criterios_ventas)

  # Ganador
  ganador_v <- comp_ventas$Modelo[which.min(comp_ventas$MAPE)]

  tabla_final <- rbind(tabla_final, data.frame(
    Prediccion  = "Ventas",
    Modelo_1    = comp_ventas$Modelo[1],
    Score_1     = scores_v[1],
    Modelo_2    = comp_ventas$Modelo[2],
    Score_2     = scores_v[2],
    Modelo_3    = comp_ventas$Modelo[3],
    Score_3     = scores_v[3],
    Ganador     = ganador_v,
    Metrica_Clave = sprintf("MAPE=%.1f%%", comp_ventas$MAPE[comp_ventas$Modelo == ganador_v]),
    stringsAsFactors = FALSE
  ))
}

# --- Segmentacion ---
if (!is.null(comp_segmentacion)) {
  cat("  - Segmentacion...\n")
  criterios_seg <- data.frame(
    metrica = c("Silhouette"),
    peso    = c(0.55),
    menor_mejor = c(FALSE),
    stringsAsFactors = FALSE
  )
  scores_s <- calcular_score_ponderado(comp_segmentacion, criterios_seg)

  ganador_s <- comp_segmentacion$Modelo[which.max(comp_segmentacion$Silhouette)]

  tabla_final <- rbind(tabla_final, data.frame(
    Prediccion  = "Segmentacion",
    Modelo_1    = comp_segmentacion$Modelo[1],
    Score_1     = scores_s[1],
    Modelo_2    = comp_segmentacion$Modelo[2],
    Score_2     = scores_s[2],
    Modelo_3    = comp_segmentacion$Modelo[3],
    Score_3     = scores_s[3],
    Ganador     = ganador_s,
    Metrica_Clave = sprintf("Sil=%.3f", comp_segmentacion$Silhouette[comp_segmentacion$Modelo == ganador_s]),
    stringsAsFactors = FALSE
  ))
}

# --- Combos ---
if (!is.null(comp_combos)) {
  cat("  - Combos...\n")
  criterios_combos <- data.frame(
    metrica = c("Lift_Maximo", "N_Reglas_Utiles"),
    peso    = c(0.35, 0.30),
    menor_mejor = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  scores_c <- calcular_score_ponderado(comp_combos, criterios_combos)

  ganador_c <- comp_combos$Modelo[which.max(comp_combos$Lift_Maximo)]

  tabla_final <- rbind(tabla_final, data.frame(
    Prediccion  = "Combos",
    Modelo_1    = comp_combos$Modelo[1],
    Score_1     = scores_c[1],
    Modelo_2    = comp_combos$Modelo[2],
    Score_2     = scores_c[2],
    Modelo_3    = comp_combos$Modelo[3],
    Score_3     = scores_c[3],
    Ganador     = ganador_c,
    Metrica_Clave = sprintf("Lift=%.1fx", comp_combos$Lift_Maximo[comp_combos$Modelo == ganador_c]),
    stringsAsFactors = FALSE
  ))
}

# --- Agotamiento ---
if (!is.null(comp_agotamiento)) {
  cat("  - Agotamiento...\n")
  criterios_agot <- data.frame(
    metrica = c("F1_Test", "Recall_Test"),
    peso    = c(0.35, 0.25),
    menor_mejor = c(FALSE, FALSE),
    stringsAsFactors = FALSE
  )
  scores_a <- calcular_score_ponderado(comp_agotamiento, criterios_agot)

  ganador_a <- comp_agotamiento$Modelo[which.max(comp_agotamiento$F1_Test)]

  tabla_final <- rbind(tabla_final, data.frame(
    Prediccion  = "Agotamiento",
    Modelo_1    = comp_agotamiento$Modelo[1],
    Score_1     = scores_a[1],
    Modelo_2    = comp_agotamiento$Modelo[2],
    Score_2     = scores_a[2],
    Modelo_3    = comp_agotamiento$Modelo[3],
    Score_3     = scores_a[3],
    Ganador     = ganador_a,
    Metrica_Clave = sprintf("F1=%.3f", comp_agotamiento$F1_Test[comp_agotamiento$Modelo == ganador_a]),
    stringsAsFactors = FALSE
  ))
}

# --- Anomalias ---
if (!is.null(comp_anomalias)) {
  cat("  - Anomalias...\n")

  # Para anomalias el criterio principal es Pct_Anomalias cercano a ~1%
  # y Consenso entre modelos
  ganador_anom <- comp_anomalias$Modelo[
    which.min(abs(comp_anomalias$Pct_Anomalias[1:3] - 1))
  ]

  tabla_final <- rbind(tabla_final, data.frame(
    Prediccion  = "Anomalias",
    Modelo_1    = comp_anomalias$Modelo[1],
    Score_1     = round(100 - abs(comp_anomalias$Pct_Anomalias[1] - 1) * 10, 1),
    Modelo_2    = comp_anomalias$Modelo[2],
    Score_2     = round(100 - abs(comp_anomalias$Pct_Anomalias[2] - 1) * 10, 1),
    Modelo_3    = comp_anomalias$Modelo[3],
    Score_3     = round(100 - abs(comp_anomalias$Pct_Anomalias[3] - 1) * 10, 1),
    Ganador     = ganador_anom,
    Metrica_Clave = sprintf("Detecta %.2f%%", comp_anomalias$Pct_Anomalias[comp_anomalias$Modelo == ganador_anom]),
    stringsAsFactors = FALSE
  ))
}

# ---- 3. Exportar tabla y graficos ----
cat("\n[3/4] Exportando resultados...\n")

# Guardar CSV
guardar_resultados(tabla_final, "tabla_resumen_final.csv")

# Tabla bonita en consola
cat("\n")
cat(rep("=", 85), "\n", sep = "")
cat("  TABLA RESUMEN FINAL - COMPARACION DE MODELOS\n")
cat("  Polleria la Infanteria\n")
cat(rep("=", 85), "\n", sep = "")

for (i in seq_len(nrow(tabla_final))) {
  r <- tabla_final[i, ]
  cat(sprintf("\n  %s\n", r$Prediccion))
  cat(sprintf("    %-20s Score: %5.1f\n", r$Modelo_1, r$Score_1))
  cat(sprintf("    %-20s Score: %5.1f\n", r$Modelo_2, r$Score_2))
  cat(sprintf("    %-20s Score: %5.1f\n", r$Modelo_3, r$Score_3))
  cat(sprintf("    >>> GANADOR: %s (%s)\n", r$Ganador, r$Metrica_Clave))
}

cat(sprintf("\n%s\n", paste(rep("=", 85), collapse = "")))

# ---- 4. Generar graficos resumen ----
cat("\n[4/4] Generando graficos finales...\n")

# Heatmap de ganadores
if (nrow(tabla_final) > 0) {
  # Preparar datos para heatmap
  df_long <- data.frame()
  for (i in seq_len(nrow(tabla_final))) {
    r <- tabla_final[i, ]
    df_long <- rbind(df_long, data.frame(
      Prediccion = r$Prediccion,
      Modelo     = r$Modelo_1,
      Score      = r$Score_1
    ))
    df_long <- rbind(df_long, data.frame(
      Prediccion = r$Prediccion,
      Modelo     = r$Modelo_2,
      Score      = r$Score_2
    ))
    df_long <- rbind(df_long, data.frame(
      Prediccion = r$Prediccion,
      Modelo     = r$Modelo_3,
      Score      = r$Score_3
    ))
  }

  # Heatmap
  p_heat <- ggplot(df_long, aes(x = Prediccion, y = Modelo, fill = Score)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = sprintf("%.0f", Score)), size = 4.5, fontface = "bold") +
    scale_fill_gradient(low = "white", high = BRAND_RED, limits = c(0, 100)) +
    labs(
      title    = "Heatmap de Scores - Comparacion Multi-Modelo",
      subtitle = "Puntuacion ponderada por criterios (0-100). Mas alto = mejor.",
      x        = NULL,
      y        = NULL,
      fill     = "Score",
      caption  = "Polleria la Infanteria"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title    = element_text(face = "bold", size = 14, color = BRAND_DARK),
      plot.subtitle = element_text(size = 10, color = BRAND_BROWN),
      axis.text     = element_text(size = 11),
      legend.position = "right"
    )

  ggsave("outputs/predicciones/comparacion/06_heatmap_ganadores.png", p_heat,
         width = 9, height = 5, dpi = 150, bg = "white")
  cat("  - Heatmap guardado: outputs/predicciones/comparacion/06_heatmap_ganadores.png\n")

  # Barras de ganadores
  df_ganadores <- df_long %>%
    group_by(Prediccion) %>%
    filter(Score == max(Score)) %>%
    ungroup()

  p_ganadores <- ggplot(df_long, aes(x = Prediccion, y = Score, fill = Modelo)) +
    geom_col(position = "dodge", width = 0.7, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.0f", Score)),
              position = position_dodge(width = 0.7),
              vjust = -0.5, size = 3.5, fontface = "bold") +
    scale_fill_manual(
      values = c("Prophet" = BRAND_RED, "ARIMA/SARIMA" = BRAND_GOLD,
                 "XGBoost" = BRAND_ORANGE,
                 "K-Means" = BRAND_RED, "DBSCAN" = BRAND_GOLD, "GMM" = BRAND_ORANGE,
                 "Apriori" = BRAND_RED, "FP-Growth" = BRAND_GOLD, "Eclat" = BRAND_ORANGE,
                 "Random Forest" = BRAND_RED, "Regresion Logistica" = BRAND_GOLD,
                 "Isolation Forest" = BRAND_RED, "LOF" = BRAND_GOLD,
                 "One-Class SVM" = BRAND_ORANGE,
                 "Score 5D (baseline)" = "gray50")
    ) +
    labs(
      title    = "Comparacion Completa de Modelos por Prediccion",
      subtitle = "Scores ponderados (0-100)",
      x        = NULL,
      y        = "Score Ponderado",
      fill      = "Modelo",
      caption  = "Polleria la Infanteria - Julio 2026"
    ) +
    tema_comparacion +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))

  ggsave("outputs/predicciones/comparacion/06_barras_comparacion.png", p_ganadores,
         width = 11, height = 5.5, dpi = 150, bg = "white")
  cat("  - Grafico de barras guardado: outputs/predicciones/comparacion/06_barras_comparacion.png\n")
}

cat("\n")
cat(rep("=", 60), "\n", sep = "")
cat("  RESUMEN DE GANADORES POR PREDICCION:\n")
for (i in seq_len(nrow(tabla_final))) {
  cat(sprintf("  %s -> %s\n", tabla_final$Prediccion[i], tabla_final$Ganador[i]))
}
cat(rep("=", 60), "\n", sep = "")

finalizar_script()
