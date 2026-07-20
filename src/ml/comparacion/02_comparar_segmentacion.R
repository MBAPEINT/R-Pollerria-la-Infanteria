# ============================================================
# 02_COMPARAR_SEGMENTACION.R
# Comparacion: K-Means vs DBSCAN vs Gaussian Mixture Models
# Segmentacion de clientes por habitos de consumo
# ============================================================

source("src/ml/comparacion/utils_comparacion.R")
iniciar_script("COMPARACION MODELOS DE SEGMENTACION (Clustering)")

# ---- 1. Cargar datos ----
cat("\n[1/6] Cargando datos desde MongoDB...\n")
con_ventas  <- conectar_mongo("Ventas")
con_detalle <- conectar_mongo("Detalles_Venta")

ventas_raw  <- con_ventas$find(
  query  = '{}',
  fields = '{"FechaVenta": 1, "Total": 1, "Cliente": 1, "MetodoPago": 1, "_id": 0}'
)
detalle_raw <- con_detalle$find(
  query  = '{}',
  fields = '{"IdVenta": 1, "Producto": 1, "_id": 0}'
)

cat(sprintf("  - %d ventas cargadas\n", nrow(ventas_raw)))
cat(sprintf("  - %d lineas de detalle cargadas\n", nrow(detalle_raw)))

# ---- 2. Feature Engineering RFM Ampliado ----
cat("\n[2/6] Calculando features RFM ampliadas...\n")

hoy <- as.Date(max(ventas_raw$FechaVenta, na.rm = TRUE))

# RFM base por cliente
rfm <- ventas_raw %>%
  mutate(FechaVenta = as.Date(FechaVenta)) %>%
  group_by(Cliente) %>%
  summarise(
    recencia       = as.numeric(hoy - max(FechaVenta)),
    frecuencia     = n(),
    gasto_total    = sum(Total, na.rm = TRUE),
    ticket_promedio = if (frecuencia > 0) gasto_total / frecuencia else 0,
    primera_compra  = as.numeric(hoy - min(FechaVenta)),
    .groups = "drop"
  ) %>%
  filter(!is.na(Cliente) & Cliente != "" & Cliente != "NO_ESPECIFICADO")

# Productos distintos por cliente
# Detalles_Venta tiene IdVenta como row_number. Necesitamos unirlos con Ventas.
# Agregamos FechaVenta y Cliente a los detalles mediante join aproximado
# Estrategia: usar IdVenta de Ventas (row_number) para unir

ventas_con_id <- ventas_raw %>%
  mutate(FechaVenta = as.Date(FechaVenta)) %>%
  arrange(FechaVenta) %>%
  mutate(IdVenta = row_number())

# Join para obtener Cliente en detalles
detalle_con_cliente <- detalle_raw %>%
  inner_join(ventas_con_id %>% select(IdVenta, Cliente), by = "IdVenta")

prod_distintos <- detalle_con_cliente %>%
  filter(!is.na(Producto) & Producto != "" & Producto != "NO_ESPECIFICADO") %>%
  group_by(Cliente) %>%
  summarise(
    productos_distintos = n_distinct(Producto),
    .groups = "drop"
  )

# Metodo de pago principal
metodo_ppal <- ventas_raw %>%
  group_by(Cliente, MetodoPago) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  select(Cliente, metodo_pago_principal = MetodoPago)

# Dia preferido
dia_pref <- ventas_raw %>%
  mutate(dia_semana = as.integer(format(as.Date(FechaVenta), "%u"))) %>%
  group_by(Cliente, dia_semana) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  slice_max(n, n = 1) %>%
  ungroup() %>%
  select(Cliente, dia_preferido = dia_semana)

# Unir todo
clientes_feat <- rfm %>%
  left_join(prod_distintos, by = "Cliente") %>%
  left_join(metodo_ppal, by = "Cliente") %>%
  left_join(dia_pref, by = "Cliente") %>%
  mutate(
    productos_distintos  = ifelse(is.na(productos_distintos), 1, productos_distintos),
    metodo_pago_principal = ifelse(is.na(metodo_pago_principal), "Efectivo", metodo_pago_principal),
    dia_preferido         = ifelse(is.na(dia_preferido), 3, dia_preferido)
  )

cat(sprintf("  - %d clientes con features completas\n", nrow(clientes_feat)))

# Seleccionar features numericas para clustering
feat_numericas <- c("recencia", "frecuencia", "gasto_total", "ticket_promedio",
                    "primera_compra", "productos_distintos")

# One-hot encode metodo_pago_principal
metodos_unicos <- unique(clientes_feat$metodo_pago_principal)
for (m in metodos_unicos) {
  col_name <- paste0("pago_", gsub(" ", "_", m))
  clientes_feat[[col_name]] <- ifelse(clientes_feat$metodo_pago_principal == m, 1, 0)
}
feat_onehot <- paste0("pago_", gsub(" ", "_", metodos_unicos))

# One-hot dia_preferido
for (d in 1:7) {
  clientes_feat[[paste0("dia_", d)]] <- ifelse(clientes_feat$dia_preferido == d, 1, 0)
}
feat_dia <- paste0("dia_", 1:7)

# Todas las features
todas_feat <- c(feat_numericas, feat_onehot, feat_dia)

# Matriz para clustering (estandarizar solo las numericas)
X_num <- scale(clientes_feat[, feat_numericas])
colnames(X_num) <- feat_numericas

# Unir con one-hot (no se estandarizan)
X <- cbind(X_num, as.matrix(clientes_feat[, c(feat_onehot, feat_dia)]))

cat(sprintf("  - Features finales: %d columnas (%d numericas + %d one-hot)\n",
            ncol(X), length(feat_numericas), length(c(feat_onehot, feat_dia))))

# ---- 3. Modelo 1: K-Means ----
cat("\n[3/6] Evaluando K-Means...\n")
t_km_start <- Sys.time()
library(cluster)

# Buscar k optimo
k_range <- 3:8
km_metrics <- data.frame()

for (k in k_range) {
  set.seed(SEED)
  km <- kmeans(X, centers = k, nstart = 25, iter.max = 30)
  sil <- tryCatch({
    s <- silhouette(km$cluster, dist(X))
    mean(s[, 3])
  }, error = function(e) NA)

  km_metrics <- rbind(km_metrics, data.frame(
    k = k,
    Silhouette = sil,
    WSS = km$tot.withinss,
    stringsAsFactors = FALSE
  ))
}

# Mejor k por Silhouette
best_k <- km_metrics$k[which.max(km_metrics$Silhouette)]
cat(sprintf("  - K optimo por Silhouette: k = %d (Sil = %.4f)\n", best_k, max(km_metrics$Silhouette, na.rm = TRUE)))

# Entrenar modelo final
set.seed(SEED)
km_final <- kmeans(X, centers = best_k, nstart = 25, iter.max = 30)

# Validacion de estabilidad (30 corridas)
sil_samples <- replicate(30, {
  km_s <- kmeans(X, centers = best_k, nstart = 10, iter.max = 30)
  s <- tryCatch({
    sil <- silhouette(km_s$cluster, dist(sample_n(as.data.frame(X), min(2000, nrow(X)))))
    mean(sil[, 3])
  }, error = function(e) NA)
  s
})

km_sil_mean <- mean(sil_samples, na.rm = TRUE)
km_sil_std  <- sd(sil_samples, na.rm = TRUE)

# Calinski-Harabasz y Davies-Bouldin
km_ch <- tryCatch({
  # CH = (SSB/(k-1)) / (SSW/(n-k))
  ssb <- sum(km_final$size * (km_final$centers - colMeans(X))^2)
  ssw <- km_final$tot.withinss
  (ssb / (best_k - 1)) / (ssw / (nrow(X) - best_k))
}, error = function(e) NA)

km_db <- tryCatch({
  clusterSim::index.DB(X, km_final$cluster)$DB
}, error = function(e) NA)

t_km <- as.numeric(difftime(Sys.time(), t_km_start, units = "secs"))
cat(sprintf("  - K-Means (k=%d): Silhouette = %.4f ± %.4f, CH = %.1f, DB = %.3f [%.1fs]\n",
            best_k, km_sil_mean, km_sil_std, km_ch, ifelse(is.na(km_db), 0, km_db), t_km))

# ---- 4. Modelo 2: DBSCAN ----
cat("\n[4/6] Evaluando DBSCAN...\n")
t_db_start <- Sys.time()
library(dbscan)

# Grid search para eps y minPts
eps_vals   <- c(0.5, 1.0, 1.5, 2.0, 2.5, 3.0)
minpts_vals <- c(5, 10, 15, 20)

dbscan_grid <- expand.grid(eps = eps_vals, minPts = minpts_vals)
dbscan_grid$n_clusters <- NA
dbscan_grid$n_noise    <- NA
dbscan_grid$Silhouette <- NA

for (i in seq_len(nrow(dbscan_grid))) {
  eps    <- dbscan_grid$eps[i]
  minPts <- dbscan_grid$minPts[i]

  db <- tryCatch({
    dbscan(X, eps = eps, minPts = minPts)
  }, error = function(e) NULL)

  if (!is.null(db) && length(unique(db$cluster)) >= 2) {
    dbscan_grid$n_clusters[i] <- length(setdiff(unique(db$cluster), 0))
    dbscan_grid$n_noise[i]    <- sum(db$cluster == 0)

    # Silhouette (excluyendo ruido)
    sil_db <- tryCatch({
      idx_valid <- db$cluster != 0
      if (sum(idx_valid) > 10 && length(unique(db$cluster[idx_valid])) >= 2) {
        s <- silhouette(db$cluster[idx_valid], dist(X[idx_valid, ]))
        mean(s[, 3])
      } else NA
    }, error = function(e) NA)
    dbscan_grid$Silhouette[i] <- sil_db
  }
}

# Seleccionar mejor combinacion
dbscan_valid <- dbscan_grid[!is.na(dbscan_grid$Silhouette) &
                             dbscan_grid$n_clusters >= 2 &
                             dbscan_grid$n_noise < nrow(X) * 0.5, ]

if (nrow(dbscan_valid) > 0) {
  best_dbscan <- dbscan_valid[which.max(dbscan_valid$Silhouette), ]
  cat(sprintf("  - Mejor DBSCAN: eps = %.1f, minPts = %d, clusters = %d, ruido = %d (%.1f%%)\n",
              best_dbscan$eps, best_dbscan$minPts, best_dbscan$n_clusters,
              best_dbscan$n_noise, best_dbscan$n_noise / nrow(X) * 100))

  # Entrenar modelo final
  db_final <- dbscan(X, eps = best_dbscan$eps, minPts = best_dbscan$minPts)
  db_sil   <- best_dbscan$Silhouette
  db_noise_pct <- best_dbscan$n_noise / nrow(X) * 100
} else {
  # Fallback a parametros razonables
  db_final <- dbscan(X, eps = 1.5, minPts = 10)
  db_sil <- tryCatch({
    idx <- db_final$cluster != 0
    mean(silhouette(db_final$cluster[idx], dist(X[idx, ]))[, 3])
  }, error = function(e) NA)
  db_noise_pct <- sum(db_final$cluster == 0) / nrow(X) * 100
  cat(sprintf("  - DBSCAN (fallback eps=1.5, minPts=10): clusters=%d, ruido=%.1f%%\n",
              length(setdiff(unique(db_final$cluster), 0)), db_noise_pct))
}

t_db <- as.numeric(difftime(Sys.time(), t_db_start, units = "secs"))
cat(sprintf("  - DBSCAN: Silhouette = %.4f, %d clusters, %.1f%% ruido [%.1fs]\n",
            db_sil, length(setdiff(unique(db_final$cluster), 0)), db_noise_pct, t_db))

# ---- 5. Modelo 3: Gaussian Mixture Models ----
cat("\n[5/6] Evaluando Gaussian Mixture Models...\n")
t_gmm_start <- Sys.time()
library(mclust)

# GMM con BIC para seleccion de k
m_gmm <- tryCatch({
  Mclust(X, G = 3:8, verbose = FALSE)
}, error = function(e) {
  cat("  - mclust fallo, usando densityMclust...\n")
  tryCatch({
    densityMclust(X, G = 3:8, verbose = FALSE)
  }, error = function(e2) NULL)
})

if (!is.null(m_gmm)) {
  gmm_k <- m_gmm$G
  gmm_labels <- m_gmm$classification

  # Metricas
  gmm_sil <- tryCatch({
    s <- silhouette(gmm_labels, dist(sample_n(as.data.frame(X), min(2000, nrow(X)))))
    mean(s[, 3])
  }, error = function(e) NA)

  # Estabilidad (30 corridas muestreadas)
  gmm_sil_samples <- replicate(30, {
    idx_sample <- sample(1:nrow(X), min(1500, nrow(X)))
    gm <- tryCatch({
      densityMclust(X[idx_sample, ], G = 3:8, verbose = FALSE)
    }, error = function(e) NULL)
    if (!is.null(gm)) {
      s <- tryCatch({
        sil <- silhouette(gm$classification, dist(X[idx_sample, ]))
        mean(sil[, 3])
      }, error = function(e) NA)
      s
    } else NA
  })

  gmm_sil_mean <- mean(gmm_sil_samples, na.rm = TRUE)
  gmm_sil_std  <- sd(gmm_sil_samples, na.rm = TRUE)

  cat(sprintf("  - GMM: G=%d, Silhouette=%.4f ± %.4f, Modelo=%s [%.1fs]\n",
              gmm_k, gmm_sil_mean, gmm_sil_std, m_gmm$modelName,
              as.numeric(difftime(Sys.time(), t_gmm_start, units = "secs"))))
} else {
  gmm_k <- NA; gmm_sil <- NA; gmm_sil_mean <- NA; gmm_sil_std <- NA
  cat("  - GMM: No se pudo entrenar.\n")
}

t_gmm <- as.numeric(difftime(Sys.time(), t_gmm_start, units = "secs"))

# ---- 6. Comparar y exportar ----
cat("\n[6/6] Generando resultados comparativos...\n")

# Compilar resultados
resultados <- data.frame(
  Modelo            = c("K-Means", "DBSCAN", "GMM"),
  Silhouette        = c(km_sil_mean, db_sil, gmm_sil_mean),
  Silhouette_SD     = c(km_sil_std, NA, gmm_sil_std),
  N_Clusters        = c(best_k,
                        length(setdiff(unique(db_final$cluster), 0)),
                        gmm_k),
  Pct_Outliers      = c(0, db_noise_pct, 0),
  Tiempo_Segundos   = c(t_km, t_db, t_gmm)
)

guardar_resultados(resultados, "comparacion_segmentacion.csv")

# Grafico comparativo
grafico_comparacion(
  df             = resultados,
  metrica_col    = "Silhouette",
  modelo_col     = "Modelo",
  titulo         = "Comparacion de Modelos - Segmentacion de Clientes",
  subtitulo      = "Silhouette Score (calidad de separacion entre clusters)",
  invertir_mejor = FALSE,
  archivo_salida = "outputs/predicciones/comparacion/02_comparacion_segmentacion.png"
)

# Grafico N° de clusters
p_clusters <- ggplot(resultados, aes(x = Modelo, y = N_Clusters, fill = Modelo)) +
  geom_col(width = 0.5, alpha = 0.9) +
  geom_text(aes(label = N_Clusters), vjust = -0.5, size = 5, fontface = "bold") +
  scale_fill_manual(values = PALETTE_3, guide = "none") +
  labs(
    title    = "Numero de Segmentos por Modelo",
    x        = NULL,
    y        = "Cantidad de Clusters",
    caption  = "Polleria la Infanteria - Comparacion de Modelos"
  ) +
  tema_comparacion

ggsave("outputs/predicciones/comparacion/02_segmentacion_clusters.png", p_clusters,
       width = 7, height = 4.5, dpi = 150, bg = "white")

# Anunciar ganador
anunciar_ganador(resultados, "Modelo", "Silhouette", menor_mejor = FALSE)

cat("\n--- RESULTADOS COMPLETOS ---\n")
print(resultados[, c("Modelo", "Silhouette", "Silhouette_SD", "N_Clusters", "Pct_Outliers")], row.names = FALSE)

# Cerrar conexiones
rm(con_ventas, con_detalle)
gc()

finalizar_script()
