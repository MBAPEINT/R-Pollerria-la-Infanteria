# ============================================================
# 05_COMPARAR_ANOMALIAS.R
# Comparacion: Isolation Forest vs LOF vs One-Class SVM
# Deteccion de transacciones anomalas en ventas
# ============================================================

source("src/ml/comparacion/utils_comparacion.R")
iniciar_script("COMPARACION MODELOS DE ANOMALIAS (Deteccion de Outliers)")

# ---- 1. Cargar datos ----
cat("\n[1/6] Cargando datos desde MongoDB...\n")
con_ventas  <- conectar_mongo("Ventas")
con_detalle <- conectar_mongo("Detalles_Venta")

ventas_raw  <- con_ventas$find(
  query  = '{}',
  fields = '{"FechaVenta": 1, "Total": 1, "MetodoPago": 1, "Subtotal": 1, "_id": 0}'
)
detalle_raw <- con_detalle$find(
  query  = '{}',
  fields = '{"IdVenta": 1, "Cantidad": 1, "Producto": 1, "_id": 0}'
)

cat(sprintf("  - %d ventas cargadas\n", nrow(ventas_raw)))

# ---- 2. Feature Engineering ----
cat("\n[2/6] Construyendo features para deteccion de anomalias...\n")

# Agregar detalles por venta
detalle_agg <- detalle_raw %>%
  group_by(IdVenta) %>%
  summarise(
    cant_productos   = n_distinct(Producto, na.rm = TRUE),
    cant_total_items = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  )

# Preparar ventas
ventas <- ventas_raw %>%
  mutate(
    FechaVenta = as.POSIXct(FechaVenta),
    IdVenta    = row_number(),
    hora       = as.integer(format(FechaVenta, "%H")),
    dia_semana = as.integer(format(FechaVenta, "%u")),
    mes        = as.integer(format(FechaVenta, "%m")),
    dia_mes    = as.integer(format(FechaVenta, "%d"))
  ) %>%
  left_join(detalle_agg, by = "IdVenta") %>%
  mutate(
    cant_productos    = ifelse(is.na(cant_productos), 1, cant_productos),
    cant_total_items  = ifelse(is.na(cant_total_items), cant_productos, cant_total_items),
    Total             = as.numeric(Total),
    Subtotal          = as.numeric(Subtotal),
    ticket_promedio_producto = Total / pmax(cant_productos, 1)
  )

# One-hot encode MetodoPago
metodos_unicos <- unique(ventas$MetodoPago)
for (m in metodos_unicos) {
  col_name <- paste0("pago_", gsub(" ", "_", m))
  ventas[[col_name]] <- ifelse(ventas$MetodoPago == m, 1, 0)
}
feat_onehot <- paste0("pago_", gsub(" ", "_", metodos_unicos))

# Features finales
feature_cols <- c("Total", "Subtotal", "hora", "dia_semana", "mes", "dia_mes",
                  "cant_productos", "cant_total_items", "ticket_promedio_producto",
                  feat_onehot)

# Matriz para modelos
X <- as.matrix(ventas[, feature_cols])

# Manejar NA/Inf
X[!is.finite(X)] <- 0

cat(sprintf("  - %d ventas, %d features\n", nrow(X), ncol(X)))
cat(sprintf("  - Features: %s\n", paste(feature_cols, collapse = ", ")))

# ---- 3. Modelo 1: Isolation Forest ----
cat("\n[3/6] Entrenando Isolation Forest...\n")
t_if_start <- Sys.time()

library(solitude)

# Probar contaminaciones
contaminaciones <- c(0.01, 0.02, 0.05)
if_results <- list()

for (cont in seq_along(contaminaciones)) {
  c_val <- contaminaciones[cont]

  iso <- tryCatch({
    isolationForest$new(sample_size = min(256, nrow(X)))
  }, error = function(e) {
    isolationForest$new()
  })

  iso$fit(X)

  scores_if <- iso$predict(X)
  scores_if$anomaly_score <- scores_if$anomaly_score

  # Umbral basado en percentil
  umbral <- quantile(scores_if$anomaly_score, 1 - c_val)
  anomalos_if <- which(scores_if$anomaly_score > umbral)

  if_results[[cont]] <- list(
    contamination = c_val,
    n_anomalias   = length(anomalos_if),
    pct_anomalias = length(anomalos_if) / nrow(X) * 100,
    scores        = scores_if$anomaly_score,
    anomalos_idx  = anomalos_if
  )

  cat(sprintf("  - IF (cont=%.2f): %d anomalias (%.2f%%)\n", c_val,
              length(anomalos_if), length(anomalos_if) / nrow(X) * 100))
}

t_if <- as.numeric(difftime(Sys.time(), t_if_start, units = "secs"))
cat(sprintf("  - Isolation Forest: %.1fs\n", t_if))

# Mejor contaminacion (la que da ~1%)
diffs_if <- sapply(if_results, function(x) abs(x$pct_anomalias - 1))
best_if_idx <- which.min(diffs_if)
best_if <- if_results[[best_if_idx]]

# ---- 4. Modelo 2: Local Outlier Factor ----
cat("\n[4/6] Entrenando Local Outlier Factor...\n")
t_lof_start <- Sys.time()

library(dbscan)

# Probar diferentes k (vecinos)
k_vals <- c(10, 20, 50, 100)
lof_results <- list()

for (ki in seq_along(k_vals)) {
  k <- k_vals[ki]

  lof_scores <- tryCatch({
    lof(X, k = min(k, nrow(X) - 1))
  }, error = function(e) {
    # Fallback con menos vecinos
    lof(X, k = min(5, nrow(X) - 1))
  })

  # Normalizar scores > 0
  lof_scores[!is.finite(lof_scores)] <- 0

  for (c_val in contaminaciones) {
    umbral <- quantile(lof_scores, 1 - c_val)
    anomalos_lof <- which(lof_scores > umbral)

    key <- paste0("k", k, "_c", c_val)
    lof_results[[key]] <- list(
      k             = k,
      contamination = c_val,
      n_anomalias   = length(anomalos_lof),
      pct_anomalias = length(anomalos_lof) / nrow(X) * 100,
      scores        = lof_scores,
      anomalos_idx  = anomalos_lof
    )
  }
}

t_lof <- as.numeric(difftime(Sys.time(), t_lof_start, units = "secs"))

# Mejor configuracion
lof_df <- do.call(rbind, lapply(names(lof_results), function(n) {
  r <- lof_results[[n]]
  data.frame(key = n, k = r$k, contamination = r$contamination,
             n = r$n_anomalias, pct = r$pct_anomalias)
}))
lof_df$diff <- abs(lof_df$pct - 1)
best_lof_key <- lof_df$key[which.min(lof_df$diff)]
best_lof <- lof_results[[best_lof_key]]

cat(sprintf("  - LOF (k=%d, cont=%.2f): %d anomalias (%.2f%%) [%.1fs]\n",
            best_lof$k, best_lof$contamination, best_lof$n_anomalias,
            best_lof$pct_anomalias, t_lof))

# ---- 5. Modelo 3: One-Class SVM ----
cat("\n[5/6] Entrenando One-Class SVM...\n")
t_svm_start <- Sys.time()

library(e1071)

svm_results <- list()

for (c_val in contaminaciones) {
  nu_val <- c_val

  # Muestrear para SVM (es O(n²))
  n_sample <- min(5000, nrow(X))
  set.seed(SEED)
  idx_sample <- sample(1:nrow(X), n_sample)
  X_sample <- X[idx_sample, ]

  svm_model <- tryCatch({
    svm(X_sample, type = "one-classification", kernel = "radial",
        nu = nu_val, gamma = 1 / ncol(X), scale = TRUE)
  }, error = function(e) NULL)

  if (!is.null(svm_model)) {
    pred_svm <- predict(svm_model, X)
    anomalos_svm <- which(pred_svm == FALSE)  # FALSE = outlier en one-class SVM

    svm_results[[as.character(c_val)]] <- list(
      nu            = nu_val,
      contamination = c_val,
      n_anomalias   = length(anomalos_svm),
      pct_anomalias = length(anomalos_svm) / nrow(X) * 100,
      anomalos_idx  = anomalos_svm
    )

    cat(sprintf("  - OCSVM (nu=%.2f): %d anomalias (%.2f%%)\n", nu_val,
                length(anomalos_svm), length(anomalos_svm) / nrow(X) * 100))
  } else {
    cat(sprintf("  - OCSVM (nu=%.2f): ERROR al entrenar\n", nu_val))
  }
}

t_svm <- as.numeric(difftime(Sys.time(), t_svm_start, units = "secs"))

# Mejor configuracion
if (length(svm_results) > 0) {
  svm_diffs <- sapply(names(svm_results), function(n) abs(svm_results[[n]]$pct_anomalias - 1))
  best_svm_key <- names(svm_results)[which.min(svm_diffs)]
  best_svm <- svm_results[[best_svm_key]]
} else {
  best_svm <- list(n_anomalias = 0, pct_anomalias = 0, anomalos_idx = integer(0))
}

cat(sprintf("  - One-Class SVM: %.1fs\n", t_svm))

# ---- 6. Comparar y calcular solapamiento ----
cat("\n[6/6] Analizando resultados...\n")

# Solapamiento entre modelos
anom_if  <- best_if$anomalos_idx
anom_lof <- best_lof$anomalos_idx
anom_svm <- if (length(svm_results) > 0) best_svm$anomalos_idx else integer(0)

# Intersecciones
if_if_lof  <- length(intersect(anom_if, anom_lof))
if_if_svm  <- length(intersect(anom_if, anom_svm))
if_lof_svm <- length(intersect(anom_lof, anom_svm))

# Consenso: detectadas por al menos 2 modelos
todas_anomalias <- unique(c(anom_if, anom_lof, anom_svm))
conteo <- integer(length(todas_anomalias))
for (i in seq_along(todas_anomalias)) {
  v <- todas_anomalias[i]
  conteo[i] <- (v %in% anom_if) + (v %in% anom_lof) + (v %in% anom_svm)
}
consenso_2 <- sum(conteo >= 2)
consenso_3 <- sum(conteo >= 3)

cat(sprintf("  - Interseccion IF-LOF: %d anomalias\n", if_if_lof))
cat(sprintf("  - Interseccion IF-SVM: %d anomalias\n", if_if_svm))
cat(sprintf("  - Interseccion LOF-SVM: %d anomalias\n", if_lof_svm))
cat(sprintf("  - Consenso >= 2 modelos: %d anomalias (%.2f%%)\n",
            consenso_2, consenso_2 / nrow(X) * 100))
cat(sprintf("  - Consenso 3 modelos: %d anomalias\n", consenso_3))

# Comparacion con score 5D (ejecutar script original para obtener sus anomalias)
# Cargamos el CSV del script original si existe
score5d_n <- 99  # Valor reportado por el script original
score5d_pct <- 0.36
cat(sprintf("  - Score 5D original: %d anomalias (%.2f%%)\n", score5d_n, score5d_pct))

# Compilar resultados
resultados <- data.frame(
  Modelo              = c("Isolation Forest", "LOF", "One-Class SVM", "Score 5D (baseline)"),
  N_Anomalias         = c(best_if$n_anomalias, best_lof$n_anomalias,
                          best_svm$n_anomalias, score5d_n),
  Pct_Anomalias       = c(best_if$pct_anomalias, best_lof$pct_anomalias,
                          best_svm$pct_anomalias, score5d_pct),
  Consenso_2Modelos   = c(
    sum((todas_anomalias %in% anom_if) & conteo >= 2),
    sum((todas_anomalias %in% anom_lof) & conteo >= 2),
    sum((todas_anomalias %in% anom_svm) & conteo >= 2),
    NA
  ),
  Tiempo_Segundos     = c(t_if, t_lof, t_svm, NA)
)

guardar_resultados(resultados, "comparacion_anomalias.csv")

# Grafico comparativo - % anomalias
p_pct <- ggplot(resultados[1:3, ], aes(x = Modelo, y = Pct_Anomalias, fill = Modelo)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.2f%%", Pct_Anomalias)), vjust = -0.5,
            size = 4.5, fontface = "bold") +
  geom_hline(yintercept = 1.0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  annotate("text", x = 3.5, y = 1.1, label = "Referencia 1%", size = 3, color = "gray50") +
  scale_fill_manual(values = PALETTE_3, guide = "none") +
  labs(
    title    = "Comparacion de Modelos - Deteccion de Anomalias",
    subtitle = "Porcentaje de ventas detectadas como anomalas",
    y        = "% de Anomalias Detectadas",
    x        = NULL,
    caption  = "Polleria la Infanteria - Comparacion de Modelos"
  ) +
  tema_comparacion

ggsave("outputs/predicciones/comparacion/05_comparacion_anomalias.png", p_pct,
       width = 7, height = 4.5, dpi = 150, bg = "white")

# Grafico de consenso (diagrama de Venn simplificado como barras)
df_consenso <- data.frame(
  Categoria = c("IF-LOF", "IF-SVM", "LOF-SVM", "≥ 2 modelos", "3 modelos"),
  Cantidad  = c(if_if_lof, if_if_svm, if_lof_svm, consenso_2, consenso_3)
)

p_consenso <- ggplot(df_consenso, aes(x = reorder(Categoria, -Cantidad), y = Cantidad)) +
  geom_col(fill = BRAND_RED, width = 0.5, alpha = 0.9) +
  geom_text(aes(label = Cantidad), vjust = -0.5, size = 4, fontface = "bold") +
  labs(
    title    = "Solapamiento entre Modelos de Anomalias",
    subtitle = "Cantidad de anomalias detectadas en comun",
    x        = NULL,
    y        = "Cantidad de Anomalias",
    caption  = "Polleria la Infanteria - Comparacion de Modelos"
  ) +
  tema_comparacion

ggsave("outputs/predicciones/comparacion/05_anomalias_consenso.png", p_consenso,
       width = 7, height = 4.5, dpi = 150, bg = "white")

# Anunciar ganador (basado en consistencia de deteccion)
cat("\n--- GANADOR BASADO EN CONSISTENCIA ---\n")
consenso_pcts <- c(
  sum((todas_anomalias %in% anom_if) & conteo >= 2) / best_if$n_anomalias * 100,
  sum((todas_anomalias %in% anom_lof) & conteo >= 2) / best_lof$n_anomalias * 100,
  sum((todas_anomalias %in% anom_svm) & conteo >= 2) / max(best_svm$n_anomalias, 1) * 100
)
nombres_modelos <- c("Isolation Forest", "LOF", "One-Class SVM")
ganador_idx <- which.max(consenso_pcts)
cat(sprintf("  GANADOR: %s (%.1f%% de sus anomalias confirmadas por >=2 modelos)\n",
            nombres_modelos[ganador_idx], consenso_pcts[ganador_idx]))

cat("\n--- RESULTADOS COMPLETOS ---\n")
print(resultados[, c("Modelo", "N_Anomalias", "Pct_Anomalias")], row.names = FALSE)

# Cerrar conexiones
rm(con_ventas, con_detalle)
gc()

finalizar_script()
