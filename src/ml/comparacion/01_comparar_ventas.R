# ============================================================
# 01_COMPARAR_VENTAS.R
# Comparacion: Prophet vs ARIMA vs XGBoost
# Prediccion de ventas diarias/semanales/mensuales
# ============================================================

source("src/ml/comparacion/utils_comparacion.R")
iniciar_script("COMPARACION MODELOS DE VENTAS (Series de Tiempo)")

# ---- 1. Cargar datos ----
cat("\n[1/6] Cargando datos desde MongoDB...\n")
con_ventas <- conectar_mongo("Ventas")

ventas_raw <- con_ventas$find(
  query  = '{}',
  fields = '{"FechaVenta": 1, "Total": 1, "_id": 0}'
)

cat(sprintf("  - %d ventas cargadas\n", nrow(ventas_raw)))

# Agregar a nivel diario
ventas_diarias <- ventas_raw %>%
  mutate(Fecha = as.Date(FechaVenta)) %>%
  group_by(Fecha) %>%
  summarise(
    Total      = sum(Total, na.rm = TRUE),
    n_ventas   = n(),
    .groups    = "drop"
  ) %>%
  arrange(Fecha)

cat(sprintf("  - %d dias con ventas\n", nrow(ventas_diarias)))
cat(sprintf("  - Rango: %s a %s\n", min(ventas_diarias$Fecha), max(ventas_diarias$Fecha)))
cat(sprintf("  - Venta diaria promedio: S/ %.0f\n", mean(ventas_diarias$Total)))

# ---- 2. Preprocesar ----
cat("\n[2/6] Preparando datos para modelado...\n")

# Crear variables calendario
ventas_diarias <- ventas_diarias %>%
  mutate(
    ds           = as.Date(Fecha),
    y            = Total,
    dia_semana   = as.integer(format(Fecha, "%u")),  # 1=Lunes..7=Domingo
    mes          = as.integer(format(Fecha, "%m")),
    semana_anio  = as.integer(format(Fecha, "%U")),
    es_finde     = ifelse(dia_semana >= 6, 1, 0),
    dia_mes      = as.integer(format(Fecha, "%d")),
    es_quincena  = ifelse(dia_mes %in% c(1, 2, 14, 15, 16), 1, 0)
  )

# Split temporal: 60% train, 40% test (holdout fijo)
n_total  <- nrow(ventas_diarias)
n_train  <- floor(n_total * 0.6)
train_df <- ventas_diarias[1:n_train, ]
test_df  <- ventas_diarias[(n_train + 1):n_total, ]

cat(sprintf("  - Train: %d dias (%s a %s)\n", nrow(train_df), min(train_df$Fecha), max(train_df$Fecha)))
cat(sprintf("  - Test:  %d dias (%s a %s)\n", nrow(test_df), min(test_df$Fecha), max(test_df$Fecha)))

# ---- 3. Modelo 1: Prophet ----
cat("\n[3/6] Entrenando Prophet...\n")
t_prophet_start <- Sys.time()

library(prophet)

# Crear dataframe de feriados peruanos 2023-2024
feriados <- data.frame(
  holiday = c(
    "Año Nuevo", "Año Nuevo",
    "Semana Santa", "Semana Santa",
    "Día del Trabajo", "Día del Trabajo",
    "San Pedro y San Pablo", "San Pedro y San Pablo",
    "Fiestas Patrias", "Fiestas Patrias", "Fiestas Patrias", "Fiestas Patrias",
    "Santa Rosa de Lima", "Santa Rosa de Lima",
    "Combate de Angamos", "Combate de Angamos",
    "Todos los Santos", "Todos los Santos",
    "Inmaculada Concepción", "Inmaculada Concepción",
    "Navidad", "Navidad"
  ),
  ds = as.Date(c(
    "2023-01-01", "2024-01-01",
    "2023-04-06", "2024-03-28",
    "2023-05-01", "2024-05-01",
    "2023-06-29", "2024-06-29",
    "2023-07-28", "2023-07-29", "2024-07-28", "2024-07-29",
    "2023-08-30", "2024-08-30",
    "2023-10-08", "2024-10-08",
    "2023-11-01", "2024-11-01",
    "2023-12-08", "2024-12-08",
    "2023-12-25", "2024-12-25"
  )),
  lower_window = 0,
  upper_window = 0
)

m_prophet <- prophet(
  df = train_df[, c("ds", "y")],
  yearly.seasonality  = TRUE,
  weekly.seasonality  = TRUE,
  daily.seasonality   = FALSE,
  changepoint.prior.scale = 0.05,
  holidays = feriados[feriados$ds >= min(train_df$ds) & feriados$ds <= max(test_df$ds), ]
)

future <- make_future_dataframe(m_prophet, periods = nrow(test_df))
forecast_prophet <- predict(m_prophet, future)

# Extraer predicciones para el periodo test
pred_prophet <- tail(forecast_prophet$yhat, nrow(test_df))

metricas_prophet <- calcular_metricas_ts(test_df$y, pred_prophet)
t_prophet <- as.numeric(difftime(Sys.time(), t_prophet_start, units = "secs"))
cat(sprintf("  - Prophet: MAPE = %.2f%%, RMSE = S/ %.0f, R² = %.4f [%.1fs]\n",
            metricas_prophet$MAPE, metricas_prophet$RMSE, metricas_prophet$R2, t_prophet))

# ---- 4. Modelo 2: ARIMA/SARIMA ----
cat("\n[4/6] Entrenando ARIMA/SARIMA...\n")
t_arima_start <- Sys.time()

library(forecast)

# Crear serie de tiempo
ts_train <- ts(train_df$y, frequency = 7)  # Estacionalidad semanal

# auto.arima con SARIMA
m_arima <- auto.arima(
  ts_train,
  seasonal        = TRUE,
  stepwise        = FALSE,
  approximation   = FALSE,
  max.p = 5, max.q = 5, max.P = 2, max.Q = 2
)

cat(sprintf("  - ARIMA seleccionado: %s\n", as.character(m_arima)))

# Predecir
forecast_arima <- forecast(m_arima, h = nrow(test_df))
pred_arima <- as.numeric(forecast_arima$mean)
pred_arima <- pmax(pred_arima, 0)  # No negativos

metricas_arima <- calcular_metricas_ts(test_df$y, pred_arima)
t_arima <- as.numeric(difftime(Sys.time(), t_arima_start, units = "secs"))
cat(sprintf("  - ARIMA: MAPE = %.2f%%, RMSE = S/ %.0f, R² = %.4f [%.1fs]\n",
            metricas_arima$MAPE, metricas_arima$RMSE, metricas_arima$R2, t_arima))

# ---- 5. Modelo 3: XGBoost ----
cat("\n[5/6] Entrenando XGBoost...\n")
t_xgb_start <- Sys.time()

library(xgboost)

# Crear features para XGBoost: lags + rolling windows + calendario
crear_features_xgb <- function(df, max_lag = 7) {
  df_feat <- df %>% select(Fecha, y, dia_semana, mes, semana_anio, es_finde, es_quincena)
  df_feat <- df_feat %>% arrange(Fecha)

  # Lags
  for (lag in 1:max_lag) {
    df_feat[[paste0("lag_", lag)]] <- lag(df_feat$y, n = lag)
  }

  # Rolling means
  df_feat$roll_mean_7  <- zoo::rollmean(df_feat$y, k = 7, fill = NA, align = "right")
  df_feat$roll_mean_14 <- zoo::rollmean(df_feat$y, k = 14, fill = NA, align = "right")
  df_feat$roll_mean_30 <- zoo::rollmean(df_feat$y, k = 30, fill = NA, align = "right")

  df_feat
}

# Crear features para todo el dataset
all_feat <- crear_features_xgb(ventas_diarias)

# Variables predictoras
feature_cols <- c("dia_semana", "mes", "semana_anio", "es_finde", "es_quincena",
                  paste0("lag_", 1:7), "roll_mean_7", "roll_mean_14", "roll_mean_30")

# Preparar train/test
train_feat <- all_feat[1:n_train, ]
test_feat  <- all_feat[(n_train + 1):n_total, ]

# Imputar NAs en train (primeras filas no tienen lags)
train_feat_clean <- train_feat
for (col in feature_cols) {
  if (any(is.na(train_feat_clean[[col]]))) {
    med <- median(train_feat_clean[[col]], na.rm = TRUE)
    train_feat_clean[[col]][is.na(train_feat_clean[[col]])] <- med
  }
}

# Preparar matrices XGBoost
dtrain <- xgb.DMatrix(
  data  = as.matrix(train_feat_clean[, feature_cols]),
  label = train_feat_clean$y
)

# Para test, rellenar NAs con la mediana de train
test_feat_clean <- test_feat
for (col in feature_cols) {
  if (any(is.na(test_feat_clean[[col]]))) {
    med <- median(train_feat_clean[[col]], na.rm = TRUE)
    test_feat_clean[[col]][is.na(test_feat_clean[[col]])] <- med
  }
}

dtest <- xgb.DMatrix(
  data  = as.matrix(test_feat_clean[, feature_cols]),
  label = test_feat_clean$y
)

# Parametros XGBoost
params_xgb <- list(
  objective   = "reg:squarederror",
  eval_metric = "rmse",
  max_depth   = 4,
  eta         = 0.05,
  subsample   = 0.8,
  colsample_bytree = 0.8,
  min_child_weight = 3
)

m_xgb <- xgb.train(
  params        = params_xgb,
  data          = dtrain,
  nrounds       = 500,
  watchlist     = list(train = dtrain, test = dtest),
  early_stopping_rounds = 30,
  print_every_n = 0,
  verbose       = 0
)

pred_xgb <- predict(m_xgb, dtest)
pred_xgb <- pmax(pred_xgb, 0)

metricas_xgb <- calcular_metricas_ts(test_feat_clean$y, pred_xgb)
t_xgb <- as.numeric(difftime(Sys.time(), t_xgb_start, units = "secs"))
cat(sprintf("  - XGBoost: MAPE = %.2f%%, RMSE = S/ %.0f, R² = %.4f [%.1fs, %d rounds]\n",
            metricas_xgb$MAPE, metricas_xgb$RMSE, metricas_xgb$R2, t_xgb, m_xgb$best_iteration))

# Importancia de variables XGBoost
importancia <- xgb.importance(model = m_xgb, feature_names = feature_cols)
cat("  - Top 5 features XGBoost:", paste(head(importancia$Feature, 5), collapse = ", "), "\n")

# ---- 6. Comparar y exportar ----
cat("\n[6/6] Generando resultados comparativos...\n")

# Compilar resultados
resultados <- data.frame(
  Modelo          = c("Prophet", "ARIMA/SARIMA", "XGBoost"),
  MAPE            = c(metricas_prophet$MAPE, metricas_arima$MAPE, metricas_xgb$MAPE),
  RMSE            = c(metricas_prophet$RMSE, metricas_arima$RMSE, metricas_xgb$RMSE),
  MAE             = c(metricas_prophet$MAE, metricas_arima$MAE, metricas_xgb$MAE),
  R2              = c(metricas_prophet$R2, metricas_arima$R2, metricas_xgb$R2),
  Tiempo_Segundos = c(t_prophet, t_arima, t_xgb)
)

guardar_resultados(resultados, "comparacion_ventas.csv")

# Grafico comparativo principal (MAPE)
grafico_comparacion(
  df             = resultados,
  metrica_col    = "MAPE",
  modelo_col     = "Modelo",
  titulo         = "Comparacion de Modelos - Ventas",
  subtitulo      = "MAPE: Mean Absolute Percentage Error",
  invertir_mejor = TRUE,
  archivo_salida = "outputs/predicciones/comparacion/01_comparacion_ventas.png"
)
cat("  - Grafico guardado: outputs/predicciones/comparacion/01_comparacion_ventas.png\n")

# Grafico multi-metrica
grafico_multi_metrica(
  df             = resultados,
  modelo_col     = "Modelo",
  metricas       = c("MAPE", "RMSE", "MAE", "R2"),
  titulo         = "Comparacion Completa - Modelos de Ventas",
  archivo_salida = "outputs/predicciones/comparacion/01_ventas_multi.png"
)
cat("  - Grafico multi-metrica guardado\n")

# Serie temporal: real vs predicciones
df_plot <- data.frame(
  Fecha   = rep(test_df$Fecha, 4),
  Valor   = c(test_df$y, pred_prophet, pred_arima, pred_xgb),
  Modelo  = rep(c("Real", "Prophet", "ARIMA/SARIMA", "XGBoost"), each = nrow(test_df))
)

p_ts <- ggplot(df_plot, aes(x = Fecha, y = Valor, color = Modelo, linetype = Modelo)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Real" = BRAND_DARK, "Prophet" = BRAND_RED,
                                 "ARIMA/SARIMA" = BRAND_GOLD, "XGBoost" = BRAND_ORANGE)) +
  scale_linetype_manual(values = c("Real" = "solid", "Prophet" = "dashed",
                                    "ARIMA/SARIMA" = "dotted", "XGBoost" = "twodash")) +
  labs(
    title    = "Predicciones vs Valores Reales (Test)",
    subtitle = sprintf("%d dias de validacion", nrow(test_df)),
    x        = "Fecha",
    y        = "Ventas (S/)",
    caption  = "Polleria la Infanteria - Comparacion de Modelos"
  ) +
  tema_comparacion

ggsave("outputs/predicciones/comparacion/01_ventas_serie.png", p_ts,
       width = 10, height = 5, dpi = 150, bg = "white")
cat("  - Grafico de serie temporal guardado\n")

# Anunciar ganador
anunciar_ganador(resultados, "Modelo", "MAPE", menor_mejor = TRUE)

# Mostrar tabla completa
cat("\n--- RESULTADOS COMPLETOS ---\n")
print(resultados[, c("Modelo", "MAPE", "RMSE", "R2", "Tiempo_Segundos")], row.names = FALSE)

# Cerrar conexiones
rm(con_ventas)
gc()

finalizar_script()
