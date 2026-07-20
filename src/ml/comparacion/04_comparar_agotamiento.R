# ============================================================
# 04_COMPARAR_AGOTAMIENTO.R
# Comparacion: Random Forest vs XGBoost vs Regresion Logistica
# Prediccion de agotamiento de insumos en 7 dias
# ============================================================

source("src/ml/comparacion/utils_comparacion.R")
iniciar_script("COMPARACION MODELOS DE AGOTAMIENTO (Clasificacion)")

# ---- 1. Cargar datos ----
cat("\n[1/6] Cargando datos desde MongoDB...\n")
con_insumos    <- conectar_mongo("Insumos")
con_mov_inv    <- conectar_mongo("Movimientos_Inventario")
con_ordenes    <- conectar_mongo("Ordenes_Compra")
con_det_orden  <- conectar_mongo("Detalle_Orden_Compra")

insumos_raw    <- con_insumos$find(query = '{}', fields = '{"_id": 0}')
mov_inv_raw    <- con_mov_inv$find(query = '{}', fields = '{"_id": 0}')
ordenes_raw    <- con_ordenes$find(query = '{}', fields = '{"_id": 0}')
det_orden_raw  <- con_det_orden$find(query = '{}', fields = '{"_id": 0}')

cat(sprintf("  - %d insumos\n", nrow(insumos_raw)))
cat(sprintf("  - %d movimientos inventario\n", nrow(mov_inv_raw)))
cat(sprintf("  - %d ordenes de compra\n", nrow(ordenes_raw)))

# ---- 2. Feature Engineering ----
cat("\n[2/6] Calculando features para agotamiento...\n")

hoy <- as.Date(max(mov_inv_raw$FechaMovimiento, na.rm = TRUE))

# Consumo por insumo
mov_inv <- mov_inv_raw %>%
  mutate(FechaMovimiento = as.Date(FechaMovimiento))

# Consumo ultimos 7 y 30 dias
consumo <- mov_inv %>%
  filter(Tipo == "Salida") %>%
  group_by(Insumo) %>%
  summarise(
    consumo_ult_7d  = sum(Cantidad[FechaMovimiento >= hoy - 7], na.rm = TRUE),
    consumo_ult_30d = sum(Cantidad[FechaMovimiento >= hoy - 30], na.rm = TRUE),
    dias_activo     = n_distinct(FechaMovimiento[FechaMovimiento >= hoy - 30]),
    ultimo_movimiento = max(FechaMovimiento, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    consumo_diario    = consumo_ult_30d / 30,
    dias_ultimo_mov   = as.numeric(hoy - ultimo_movimiento)
  )

# Dias desde ultima compra
ultima_compra <- det_orden_raw %>%
  left_join(ordenes_raw %>% select(FechaPedido),
            by = c("IdOrdenCompra" = "row_number")) %>%
  mutate(FechaPedido = as.Date(FechaPedido)) %>%
  group_by(Insumo) %>%
  summarise(
    dias_desde_ultima_compra = as.numeric(hoy - max(FechaPedido, na.rm = TRUE)),
    costo_unitario_promedio  = mean(PrecioUnitario, na.rm = TRUE),
    .groups = "drop"
  )

# Unir con insumos
datos <- insumos_raw %>%
  left_join(consumo, by = c("Nombre" = "Insumo")) %>%
  left_join(ultima_compra, by = c("Nombre" = "Insumo")) %>%
  mutate(
    StockActual  = as.numeric(StockActual),
    StockMinimo  = as.numeric(StockMinimo),
    StockMaximo  = as.numeric(StockMaximo),
    consumo_diario    = ifelse(is.na(consumo_diario) | consumo_diario == 0, 0.01, consumo_diario),
    consumo_ult_7d    = ifelse(is.na(consumo_ult_7d), 0, consumo_ult_7d),
    consumo_ult_30d   = ifelse(is.na(consumo_ult_30d), consumo_diario * 30, consumo_ult_30d),
    dias_activo       = ifelse(is.na(dias_activo), 0, dias_activo),
    dias_desde_ultima_compra = ifelse(is.na(dias_desde_ultima_compra), 999, dias_desde_ultima_compra),
    costo_unitario_promedio  = ifelse(is.na(costo_unitario_promedio), 0, costo_unitario_promedio),
    ratio_min   = pmax(StockActual / pmax(StockMinimo, 0.01), 0.01),
    ratio_max   = pmax(StockActual / pmax(StockMaximo, 0.01), 0.01),
    tendencia_consumo = ifelse(consumo_diario > 0.001, consumo_ult_7d / (consumo_diario * 7), 1)
  )

# Target: se agota en 7 dias
datos$se_agotara <- ifelse(
  datos$StockActual / pmax(datos$consumo_diario * 7, 0.001) <= 1, 1, 0
)

cat(sprintf("  - Insumos procesados: %d\n", nrow(datos)))
cat(sprintf("  - Se agotan (real): %d (%.1f%%)\n",
            sum(datos$se_agotara), sum(datos$se_agotara) / nrow(datos) * 100))

# Variables predictoras
feature_cols <- c("StockActual", "StockMinimo", "StockMaximo",
                  "consumo_diario", "consumo_ult_7d", "consumo_ult_30d",
                  "dias_activo", "dias_desde_ultima_compra",
                  "ratio_min", "ratio_max", "tendencia_consumo",
                  "costo_unitario_promedio")

cat(sprintf("  - Features: %d\n", length(feature_cols)))

# ---- 3. Generar datos sinteticos ----
cat("\n[3/6] Generando escenarios sinteticos...\n")

# Para cada insumo, generamos 50 escenarios variando el stock
set.seed(SEED)
datos_sinteticos <- data.frame()

for (i in seq_len(nrow(datos))) {
  insumo_base <- datos[i, ]

  # 50 escenarios: stock desde 0 hasta StockMaximo * 1.5
  stocks <- seq(0, pmax(insumo_base$StockMaximo * 1.5, insumo_base$StockMinimo * 3, 100),
                length.out = 50)

  for (s in stocks) {
    nuevo <- insumo_base
    nuevo$StockActual <- s
    nuevo$ratio_min   <- pmax(s / pmax(nuevo$StockMinimo, 0.01), 0.01)
    nuevo$ratio_max   <- pmax(s / pmax(nuevo$StockMaximo, 0.01), 0.01)
    nuevo$se_agotara  <- ifelse(s / pmax(nuevo$consumo_diario * 7, 0.001) <= 1, 1, 0)
    datos_sinteticos <- rbind(datos_sinteticos, nuevo)
  }
}

cat(sprintf("  - %d escenarios sinteticos generados (%d insumos x 50 escenarios)\n",
            nrow(datos_sinteticos), nrow(datos)))
cat(sprintf("  - Clase positiva (se agota): %d (%.1f%%)\n",
            sum(datos_sinteticos$se_agotara),
            sum(datos_sinteticos$se_agotara) / nrow(datos_sinteticos) * 100))

# ---- 4. Preparar train/test ----
cat("\n[4/6] Preparando train/test split...\n")

# Stratified split (80/20)
set.seed(SEED)
pos_idx <- which(datos_sinteticos$se_agotara == 1)
neg_idx <- which(datos_sinteticos$se_agotara == 0)

train_pos <- sample(pos_idx, size = floor(length(pos_idx) * 0.8))
test_pos  <- setdiff(pos_idx, train_pos)
train_neg <- sample(neg_idx, size = floor(length(neg_idx) * 0.8))
test_neg  <- setdiff(neg_idx, train_neg)

train_idx <- c(train_pos, train_neg)
test_idx  <- c(test_pos, test_neg)

X_train <- as.matrix(datos_sinteticos[train_idx, feature_cols])
y_train <- datos_sinteticos$se_agotara[train_idx]
X_test  <- as.matrix(datos_sinteticos[test_idx, feature_cols])
y_test  <- datos_sinteticos$se_agotara[test_idx]

# Imputar NA/Inf en train
X_train[!is.finite(X_train)] <- 0
X_test[!is.finite(X_test)]   <- 0

cat(sprintf("  - Train: %d (%d positivos)\n", length(train_idx), sum(y_train)))
cat(sprintf("  - Test:  %d (%d positivos)\n", length(test_idx), sum(y_test)))

# ---- 5. Entrenar y evaluar modelos ----
cat("\n[5/6] Entrenando modelos...\n")

library(randomForest)
library(xgboost)
library(glmnet)

resultados_modelos <- data.frame()

# --- Random Forest ---
cat("  --- Random Forest ---\n")
t_rf_start <- Sys.time()

rf_cv <- data.frame()
set.seed(SEED)

# 5-fold CV manual
folds <- caret::createFolds(y_train, k = 5, list = TRUE)

for (fold_name in names(folds)) {
  fold_idx <- folds[[fold_name]]
  X_tr <- X_train[-fold_idx, ]
  y_tr <- y_train[-fold_idx]
  X_val <- X_train[fold_idx, ]
  y_val <- y_train[fold_idx]

  rf <- randomForest(
    x = X_tr, y = as.factor(y_tr),
    ntree = 300, importance = FALSE
  )
  pred_rf <- predict(rf, X_val)
  cm <- table(factor(pred_rf, levels = c("0", "1")),
              factor(y_val, levels = c("0", "1")))
  met <- calcular_metricas_clasif(cm)
  rf_cv <- rbind(rf_cv, met)
}

# Modelo final
rf_final <- randomForest(
  x = X_train, y = as.factor(y_train),
  ntree = 500, importance = TRUE
)
pred_rf_test <- predict(rf_final, X_test)
cm_rf <- table(factor(pred_rf_test, levels = c("0", "1")),
               factor(y_test, levels = c("0", "1")))
met_rf <- calcular_metricas_clasif(cm_rf)
t_rf <- as.numeric(difftime(Sys.time(), t_rf_start, units = "secs"))

# Importancia RF
imp_rf <- importance(rf_final)
top_rf <- head(rownames(imp_rf)[order(-imp_rf[, "MeanDecreaseGini"])], 5)

cat(sprintf("  - RF: F1=%.4f ± %.4f, ROC-AUC=NA, Accuracy=%.4f [%.1fs]\n",
            mean(rf_cv$F1), sd(rf_cv$F1), met_rf$Accuracy, t_rf))
cat(sprintf("  - Top variables: %s\n", paste(top_rf, collapse = ", ")))

resultados_modelos <- rbind(resultados_modelos, data.frame(
  Modelo = "Random Forest",
  F1_CV_Mean    = mean(rf_cv$F1),
  F1_CV_SD      = sd(rf_cv$F1),
  Precision_Test = met_rf$Precision,
  Recall_Test    = met_rf$Recall,
  F1_Test        = met_rf$F1,
  Accuracy_Test  = met_rf$Accuracy,
  Tiempo_Segundos = t_rf,
  stringsAsFactors = FALSE
))

# --- XGBoost ---
cat("  --- XGBoost ---\n")
t_xgb_start <- Sys.time()

xgb_cv <- data.frame()
params_xgb <- list(
  objective   = "binary:logistic",
  eval_metric = "error",
  max_depth   = 4,
  eta         = 0.05,
  subsample   = 0.8,
  colsample_bytree = 0.8
)

for (fold_name in names(folds)) {
  fold_idx <- folds[[fold_name]]
  X_tr <- X_train[-fold_idx, ]
  y_tr <- y_train[-fold_idx]
  X_val <- X_train[fold_idx, ]
  y_val <- y_train[fold_idx]

  dtrain <- xgb.DMatrix(X_tr, label = y_tr)
  dval   <- xgb.DMatrix(X_val, label = y_val)

  xgb <- xgb.train(
    params        = params_xgb,
    data          = dtrain,
    nrounds       = 200,
    watchlist     = list(val = dval),
    early_stopping_rounds = 20,
    print_every_n = 0,
    verbose       = 0
  )

  pred_xgb <- predict(xgb, dval)
  pred_class <- ifelse(pred_xgb > 0.5, 1, 0)
  cm <- table(factor(pred_class, levels = c("0", "1")),
              factor(y_val, levels = c("0", "1")))
  met <- calcular_metricas_clasif(cm)
  xgb_cv <- rbind(xgb_cv, met)
}

# Modelo final
dtrain_full <- xgb.DMatrix(X_train, label = y_train)
dtest_full  <- xgb.DMatrix(X_test, label = y_test)

xgb_final <- xgb.train(
  params        = params_xgb,
  data          = dtrain_full,
  nrounds       = 200,
  watchlist     = list(test = dtest_full),
  early_stopping_rounds = 20,
  print_every_n = 0,
  verbose       = 0
)

pred_xgb_test <- predict(xgb_final, dtest_full)
pred_xgb_class <- ifelse(pred_xgb_test > 0.5, 1, 0)
cm_xgb <- table(factor(pred_xgb_class, levels = c("0", "1")),
                factor(y_test, levels = c("0", "1")))
met_xgb <- calcular_metricas_clasif(cm_xgb)

# ROC-AUC aproximado via pROC si esta disponible
roc_auc <- NA
if (requireNamespace("pROC", quietly = TRUE)) {
  roc_obj <- pROC::roc(y_test, pred_xgb_test)
  roc_auc <- as.numeric(roc_obj$auc)
}

t_xgb <- as.numeric(difftime(Sys.time(), t_xgb_start, units = "secs"))
cat(sprintf("  - XGBoost: F1=%.4f ± %.4f, ROC-AUC=%.4f, Accuracy=%.4f [%.1fs]\n",
            mean(xgb_cv$F1), sd(xgb_cv$F1), ifelse(is.na(roc_auc), 0, roc_auc),
            met_xgb$Accuracy, t_xgb))

resultados_modelos <- rbind(resultados_modelos, data.frame(
  Modelo = "XGBoost",
  F1_CV_Mean    = mean(xgb_cv$F1),
  F1_CV_SD      = sd(xgb_cv$F1),
  Precision_Test = met_xgb$Precision,
  Recall_Test    = met_xgb$Recall,
  F1_Test        = met_xgb$F1,
  Accuracy_Test  = met_xgb$Accuracy,
  Tiempo_Segundos = t_xgb,
  stringsAsFactors = FALSE
))

# --- Regresion Logistica ---
cat("  --- Regresion Logistica ---\n")
t_rl_start <- Sys.time()

rl_cv <- data.frame()

for (fold_name in names(folds)) {
  fold_idx <- folds[[fold_name]]
  X_tr <- X_train[-fold_idx, ]
  y_tr <- y_train[-fold_idx]
  X_val <- X_train[fold_idx, ]
  y_val <- y_train[fold_idx]

  rl <- tryCatch({
    glmnet(X_tr, y_tr, family = "binomial", alpha = 0, lambda = 0.01)
  }, error = function(e) glmnet(X_tr, y_tr, family = "binomial", alpha = 0, lambda = 0.1))

  pred_rl <- predict(rl, X_val, type = "response", s = rl$lambda[1])
  pred_class <- ifelse(pred_rl > 0.5, 1, 0)
  cm <- table(factor(pred_class, levels = c("0", "1")),
              factor(y_val, levels = c("0", "1")))
  met <- calcular_metricas_clasif(cm)
  rl_cv <- rbind(rl_cv, met)
}

# Modelo final
rl_final <- glmnet(X_train, y_train, family = "binomial", alpha = 0, lambda = 0.01)
pred_rl_test <- predict(rl_final, X_test, type = "response", s = rl_final$lambda[1])
pred_rl_class <- ifelse(pred_rl_test > 0.5, 1, 0)
cm_rl <- table(factor(pred_rl_class, levels = c("0", "1")),
               factor(y_test, levels = c("0", "1")))
met_rl <- calcular_metricas_clasif(cm_rl)
t_rl <- as.numeric(difftime(Sys.time(), t_rl_start, units = "secs"))
cat(sprintf("  - RegLog: F1=%.4f ± %.4f, Accuracy=%.4f [%.1fs]\n",
            mean(rl_cv$F1), sd(rl_cv$F1), met_rl$Accuracy, t_rl))

resultados_modelos <- rbind(resultados_modelos, data.frame(
  Modelo = "Regresion Logistica",
  F1_CV_Mean    = mean(rl_cv$F1),
  F1_CV_SD      = sd(rl_cv$F1),
  Precision_Test = met_rl$Precision,
  Recall_Test    = met_rl$Recall,
  F1_Test        = met_rl$F1,
  Accuracy_Test  = met_rl$Accuracy,
  Tiempo_Segundos = t_rl,
  stringsAsFactors = FALSE
))

# ---- 6. Comparar y exportar ----
cat("\n[6/6] Generando resultados comparativos...\n")

guardar_resultados(resultados_modelos, "comparacion_agotamiento.csv")

# Grafico F1-Score
grafico_comparacion(
  df             = resultados_modelos,
  metrica_col    = "F1_Test",
  modelo_col     = "Modelo",
  titulo         = "Comparacion de Modelos - Agotamiento de Insumos",
  subtitulo      = "F1-Score en conjunto de prueba",
  invertir_mejor = FALSE,
  archivo_salida = "outputs/predicciones/comparacion/04_comparacion_agotamiento.png"
)

# Grafico multi-metrica
grafico_multi_metrica(
  df             = resultados_modelos,
  modelo_col     = "Modelo",
  metricas       = c("F1_Test", "Precision_Test", "Recall_Test", "Accuracy_Test"),
  titulo         = "Comparacion Completa - Modelos de Agotamiento",
  archivo_salida = "outputs/predicciones/comparacion/04_agotamiento_multi.png"
)

# Anunciar ganador
anunciar_ganador(resultados_modelos, "Modelo", "F1_Test", menor_mejor = FALSE)

cat("\n--- RESULTADOS COMPLETOS ---\n")
print(resultados_modelos[, c("Modelo", "F1_CV_Mean", "F1_CV_SD", "F1_Test", "Recall_Test")],
      row.names = FALSE)

# Cerrar conexiones
rm(con_insumos, con_mov_inv, con_ordenes, con_det_orden)
gc()

finalizar_script()
