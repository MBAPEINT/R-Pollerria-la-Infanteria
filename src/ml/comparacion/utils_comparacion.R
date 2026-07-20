# ============================================================
# UTILS COMPARACION - Funciones compartidas para comparacion de modelos
# Polleria la Infanteria - Julio 2026
# ============================================================

# ---- Librerias ----
library(mongolite)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

# ---- Constantes ----
MONGO_URL <- Sys.getenv("MONGO_URL", "mongodb://localhost:27017")
MONGO_DB  <- "Polleria"
SEED      <- 123

# ---- Colores de marca ----
BRAND_RED    <- "#A3000E"
BRAND_GOLD   <- "#E29E1A"
BRAND_ORANGE <- "#E8781E"
BRAND_BROWN  <- "#6E3418"
BRAND_DARK   <- "#65000C"

PALETTE_3 <- c(BRAND_RED, BRAND_GOLD, BRAND_ORANGE)

# ---- Tema ggplot2 unificado ----
tema_comparacion <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 14, color = BRAND_DARK),
    plot.subtitle = element_text(size = 10, color = BRAND_BROWN),
    axis.title    = element_text(size = 11),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

# ---- Conexion MongoDB ----
conectar_mongo <- function(coleccion) {
  mongo(
    collection = coleccion,
    db         = MONGO_DB,
    url        = MONGO_URL
  )
}

# ---- Metricas para series de tiempo ----
calcular_metricas_ts <- function(actual, predicho) {
  # Solo pares validos
  mask    <- !is.na(actual) & !is.na(predicho) & actual > 0
  actual  <- actual[mask]
  predicho <- predicho[mask]

  if (length(actual) == 0) {
    return(data.frame(MAE = NA, RMSE = NA, MAPE = NA, R2 = NA))
  }

  mae  <- mean(abs(actual - predicho))
  rmse <- sqrt(mean((actual - predicho)^2))
  mape <- mean(abs((actual - predicho) / actual)) * 100
  r2   <- 1 - sum((actual - predicho)^2) / sum((actual - mean(actual))^2)

  data.frame(MAE = round(mae, 2), RMSE = round(rmse, 2),
             MAPE = round(mape, 2), R2 = round(r2, 4))
}

# ---- Metricas para clasificacion ----
# Matriz de confusion -> metricas
calcular_metricas_clasif <- function(conf_matrix) {
  # conf_matrix: caret::confusionMatrix o tabla 2x2 con TP,TN,FP,FN
  if (is.matrix(conf_matrix) && all(dim(conf_matrix) == c(2, 2))) {
    TN <- conf_matrix[1, 1]
    FP <- conf_matrix[1, 2]
    FN <- conf_matrix[2, 1]
    TP <- conf_matrix[2, 2]
  } else if (is.table(conf_matrix) && all(dim(conf_matrix) == c(2, 2))) {
    TN <- conf_matrix[1, 1]
    FP <- conf_matrix[1, 2]
    FN <- conf_matrix[2, 1]
    TP <- conf_matrix[2, 2]
  } else {
    # Asumir que viene como vector
    TN <- conf_matrix[1]; FP <- conf_matrix[2]
    FN <- conf_matrix[3]; TP <- conf_matrix[4]
  }

  accuracy  <- (TP + TN) / (TP + TN + FP + FN)
  precision <- if (TP + FP > 0) TP / (TP + FP) else 0
  recall    <- if (TP + FN > 0) TP / (TP + FN) else 0
  f1        <- if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0

  data.frame(
    Accuracy  = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall    = round(recall, 4),
    F1        = round(f1, 4)
  )
}

# ---- Metricas para clustering ----
# Necesita paquete clusterStats o calculo manual
calcular_metricas_cluster <- function(datos, etiquetas) {
  # Silhouette score via cluster.stats o computo manual simplificado
  if (requireNamespace("cluster", quietly = TRUE)) {
    sil <- tryCatch({
      s <- cluster::silhouette(etiquetas, dist(datos))
      mean(s[, 3])
    }, error = function(e) NA)
  } else {
    sil <- NA
  }

  data.frame(
    Silhouette = round(sil, 4)
  )
}

# ---- Grafico de barras comparativo ----
grafico_comparacion <- function(df, metrica_col, modelo_col, titulo, subtitulo = "",
                                invertir_mejor = FALSE, archivo_salida = NULL) {
  # invertir_mejor = TRUE para metricas donde menor es mejor (MAPE, RMSE, DB)
  df$Modelo <- factor(df[[modelo_col]], levels = df[[modelo_col]])

  direccion <- if (invertir_mejor) "menor es mejor" else "mayor es mejor"

  p <- ggplot(df, aes(x = .data[[modelo_col]], y = .data[[metrica_col]], fill = .data[[modelo_col]])) +
    geom_col(width = 0.6, alpha = 0.9) +
    geom_text(aes(label = sprintf("%.2f", .data[[metrica_col]])),
              vjust = if (all(df[[metrica_col]] >= 0)) -0.5 else 1.2,
              size = 4.5, fontface = "bold") +
    scale_fill_manual(values = PALETTE_3, guide = "none") +
    labs(
      title    = titulo,
      subtitle = paste0(subtitulo, " (", direccion, ")"),
      x        = NULL,
      y        = metrica_col,
      caption  = "Polleria la Infanteria - Comparacion de Modelos"
    ) +
    tema_comparacion

  # Expandir eje Y para que quepan las etiquetas
  y_range <- range(df[[metrica_col]], na.rm = TRUE)
  y_pad   <- diff(y_range) * 0.15
  p <- p + coord_cartesian(ylim = c(y_range[1] - y_pad, y_range[2] + y_pad))

  if (!is.null(archivo_salida)) {
    ggsave(archivo_salida, p, width = 8, height = 5, dpi = 150, bg = "white")
  }

  p
}

# ---- Grafico de multiples metricas lado a lado ----
grafico_multi_metrica <- function(df, modelo_col, metricas, titulo, archivo_salida = NULL) {
  df_long <- df %>%
    select(all_of(c(modelo_col, metricas))) %>%
    pivot_longer(-all_of(modelo_col), names_to = "Metrica", values_to = "Valor")

  p <- ggplot(df_long, aes(x = .data[[modelo_col]], y = Valor, fill = .data[[modelo_col]])) +
    geom_col(width = 0.6, alpha = 0.9) +
    facet_wrap(~ Metrica, scales = "free_y") +
    scale_fill_manual(values = PALETTE_3, guide = "none") +
    labs(
      title    = titulo,
      x        = NULL,
      y        = NULL,
      caption  = "Polleria la Infanteria - Comparacion de Modelos"
    ) +
    tema_comparacion +
    theme(strip.text = element_text(face = "bold", size = 11))

  if (!is.null(archivo_salida)) {
    ggsave(archivo_salida, p, width = 10, height = 6, dpi = 150, bg = "white")
  }

  p
}

# ---- Score ponderado ----
calcular_score_ponderado <- function(metricas_df, criterios) {
  # metricas_df: 1 fila por modelo, columnas = metricas
  # criterios: data.frame(metrica, peso, menor_mejor)
  score_total <- 0
  for (i in seq_len(nrow(criterios))) {
    metrica     <- criterios$metrica[i]
    peso        <- criterios$peso[i]
    menor_mejor <- criterios$menor_mejor[i]

    valores <- metricas_df[[metrica]]
    if (all(is.na(valores))) next

    # Normalizar 0-100
    if (menor_mejor) {
      # Menor = mejor: invertir
      normalized <- 100 * (1 - (valores - min(valores, na.rm = TRUE)) /
                              (max(valores, na.rm = TRUE) - min(valores, na.rm = TRUE) + 1e-10))
    } else {
      normalized <- 100 * (valores - min(valores, na.rm = TRUE)) /
                          (max(valores, na.rm = TRUE) - min(valores, na.rm = TRUE) + 1e-10)
    }
    score_total <- score_total + normalized * peso
  }
  round(score_total, 1)
}

# ---- Guardar CSV de comparacion ----
guardar_resultados <- function(df, nombre_archivo) {
  ruta <- file.path("data/output/comparacion", nombre_archivo)
  dir.create(dirname(ruta), showWarnings = FALSE, recursive = TRUE)
  write.csv(df, ruta, row.names = FALSE)
  cat("\n[OK] Resultados guardados en:", ruta, "\n")
  invisible(ruta)
}

# ---- Anunciar ganador ----
anunciar_ganador <- function(df, modelo_col, metrica_principal, menor_mejor = FALSE) {
  if (menor_mejor) {
    idx <- which.min(df[[metrica_principal]])
  } else {
    idx <- which.max(df[[metrica_principal]])
  }
  ganador    <- df[[modelo_col]][idx]
  valor      <- df[[metrica_principal]][idx]
  cat("\n========================================\n")
  cat(sprintf("  GANADOR: %s (%s = %.2f)\n", ganador, metrica_principal, valor))
  cat("========================================\n")
  invisible(ganador)
}

# ---- Header informativo ----
iniciar_script <- function(titulo) {
  cat("\n")
  cat(rep("=", 60), "\n", sep = "")
  cat(sprintf("  %s\n", titulo))
  cat(sprintf("  Polleria la Infanteria - %s\n", Sys.Date()))
  cat(rep("=", 60), "\n", sep = "")
}

finalizar_script <- function() {
  cat("\n[COMPLETADO] Script finalizado exitosamente.\n\n")
}

cat("[OK] utils_comparacion.R cargado. Funciones disponibles:\n")
cat("  - conectar_mongo()\n")
cat("  - calcular_metricas_ts()\n")
cat("  - calcular_metricas_clasif()\n")
cat("  - grafico_comparacion()\n")
cat("  - grafico_multi_metrica()\n")
cat("  - calcular_score_ponderado()\n")
cat("  - guardar_resultados()\n")
cat("  - anunciar_ganador()\n")
