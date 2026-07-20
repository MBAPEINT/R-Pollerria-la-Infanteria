# ============================================================
# PREDICCIÓN 2: ¿Se va a agotar este insumo?
# Modelo: Random Forest (clasificación)
# Usa solo SALIDAS de Movimientos_Inventario como consumo real
# ============================================================
library(mongolite)
library(randomForest)
library(dplyr)
library(ggplot2)
library(lubridate)

dir.create("outputs/predicciones", showWarnings = FALSE, recursive = TRUE)
set.seed(123)

# --- 1. Cargar datos ---
insumos <- mongo(collection = "Insumos", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
mov <- mongo(collection = "Movimientos_Inventario", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')

cat("Insumos:", nrow(insumos), "| Movimientos:", nrow(mov), "\n")

# --- 2. Calcular consumo diario REAL (solo salidas) ---
mov$FechaMovimiento <- as.POSIXct(mov$FechaMovimiento, tz = "UTC")
fecha_max <- max(mov$FechaMovimiento, na.rm = TRUE)

consumo_real <- mov %>%
  filter(Tipo == "Salida") %>%
  group_by(Insumo) %>%
  summarise(
    consumo_total = sum(Cantidad, na.rm = TRUE),
    consumo_ult_7d = sum(Cantidad[FechaMovimiento >= fecha_max - days(7)], na.rm = TRUE),
    consumo_ult_30d = sum(Cantidad[FechaMovimiento >= fecha_max - days(30)], na.rm = TRUE),
    dias_activo = n_distinct(as.Date(FechaMovimiento)),
    ultima_salida = max(FechaMovimiento, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. Construir dataset de entrenamiento ---
# Para cada insumo, simulamos escenarios variando el stock
# y calculamos si se agotaría en 7 días al ritmo de consumo actual

df_list <- list()
for (i in 1:nrow(insumos)) {
  ins <- insumos[i, ]
  cons <- consumo_real %>% filter(Insumo == ins$Nombre)

  if (nrow(cons) == 0) next

  consumo_diario <- max(cons$consumo_ult_30d / 30, 0.001)

  # Generar escenarios: stock desde 0 hasta StockMaximo * 1.5
  stocks <- seq(0, ins$StockMaximo * 1.5, length.out = 50)

  for (s in stocks) {
    dias <- s / consumo_diario
    df_list[[length(df_list) + 1]] <- data.frame(
      Nombre = ins$Nombre,
      StockActual = s,
      StockMinimo = ins$StockMinimo,
      StockMaximo = ins$StockMaximo,
      consumo_diario = consumo_diario,
      consumo_ult_7d = cons$consumo_ult_7d,
      consumo_ult_30d = cons$consumo_ult_30d,
      dias_activo = cons$dias_activo,
      ratio_min = s / max(ins$StockMinimo, 0.1),
      ratio_max = s / max(ins$StockMaximo, 0.1),
      se_agotara = ifelse(dias <= 7, 1, 0),
      stringsAsFactors = FALSE
    )
  }
}

df <- bind_rows(df_list)
cat("\nDataset: ", nrow(df), "escenarios |",
    sum(df$se_agotara), "positivos (se agota) |",
    sum(df$se_agotara == 0), "negativos (no se agota)\n")

# --- 4. Entrenar Random Forest ---
features <- c("StockActual", "StockMinimo", "StockMaximo",
              "consumo_diario", "consumo_ult_7d", "consumo_ult_30d",
              "dias_activo", "ratio_min", "ratio_max")

X <- df[, features]
y <- as.factor(df$se_agotara)

rf <- randomForest(x = X, y = y, ntree = 500, importance = TRUE)

cat("\n========== MODELO ==========\n")
print(rf)
cat(sprintf("Precisión OOB: %.1f%%\n", 100 * (1 - rf$err.rate[500, "OOB"])))

# --- 5. Importancia de variables ---
importancia <- importance(rf) %>%
  as.data.frame() %>%
  mutate(variable = rownames(.)) %>%
  arrange(desc(MeanDecreaseGini))

cat("\n¿Qué variables predicen mejor el agotamiento?\n")
for (i in 1:nrow(importancia)) {
  cat(sprintf("  %d. %-20s %.1f\n", i, importancia$variable[i], importancia$MeanDecreaseGini[i]))
}

p_imp <- ggplot(importancia, aes(x = reorder(variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "#E74C3C", width = 0.6) +
  coord_flip() +
  labs(title = "Importancia de variables — Predicción de agotamiento",
       subtitle = "Random Forest | Mean Decrease Gini",
       x = "", y = "Importancia") +
  theme_minimal()
ggsave("outputs/predicciones/04_importancia_agotamiento.png", p_imp, width = 8, height = 5, dpi = 150)

# --- 6. PREDECIR con datos reales ---
insumos_pred <- insumos %>%
  left_join(consumo_real, by = c("Nombre" = "Insumo")) %>%
  mutate(
    across(c(consumo_ult_7d, consumo_ult_30d, consumo_total, dias_activo),
           ~ ifelse(is.na(.), 0, .)),
    consumo_diario = pmax(consumo_ult_30d / 30, 0.001),
    ratio_min = StockActual / pmax(StockMinimo, 0.1),
    ratio_max = StockActual / pmax(StockMaximo, 0.1)
  )

X_real <- insumos_pred[, features]
insumos_pred$riesgo_prob <- predict(rf, X_real, type = "prob")[, "1"]
insumos_pred$dias_hasta_agotar <- insumos_pred$StockActual / insumos_pred$consumo_diario

riesgo <- insumos_pred %>%
  select(Nombre, StockActual, StockMinimo, StockMaximo,
         consumo_diario, dias_hasta_agotar, riesgo_prob) %>%
  mutate(
    riesgo = case_when(
      riesgo_prob >= 0.7 ~ "ALTO",
      riesgo_prob >= 0.4 ~ "MEDIO",
      TRUE ~ "BAJO"
    ),
    dias_hasta_agotar = round(dias_hasta_agotar, 1),
    consumo_diario = round(consumo_diario, 2),
    riesgo_prob = round(riesgo_prob * 100, 0)
  ) %>%
  arrange(desc(riesgo_prob))

cat("\n========== INSUMOS EN RIESGO ==========\n")
for (i in 1:nrow(riesgo)) {
  cat(sprintf("  %-22s Stock:%5.1f | Min:%5.1f | Cons/dia:%5.2f | Dias:%5.1f | Riesgo: %-5s (%s%%)\n",
    riesgo$Nombre[i], riesgo$StockActual[i], riesgo$StockMinimo[i],
    riesgo$consumo_diario[i], riesgo$dias_hasta_agotar[i],
    riesgo$riesgo[i], riesgo$riesgo_prob[i]))
}

# Gráfico
p_riesgo <- riesgo %>%
  head(10) %>%
  ggplot(aes(x = reorder(Nombre, riesgo_prob), y = riesgo_prob, fill = riesgo)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(riesgo_prob, "%")), hjust = -0.3, size = 4, fontface = "bold") +
  scale_fill_manual(values = c("ALTO" = "#E74C3C", "MEDIO" = "#F39C12", "BAJO" = "#F1C40F")) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Top 10 insumos con mayor riesgo de agotamiento",
       subtitle = paste0("Probabilidad en próximos 7 días | Random Forest (",
                         round(100*(1-rf$err.rate[500,"OOB"]),1), "% precisión)"),
       x = "", y = "Probabilidad (%)", fill = "Riesgo") +
  theme_minimal()
ggsave("outputs/predicciones/05_riesgo_agotamiento.png", p_riesgo, width = 10, height = 5, dpi = 150)

write.csv(riesgo, "data/output/riesgo_agotamiento.csv", row.names = FALSE)

cat("\nArchivos guardados:\n")
cat("  - data/output/riesgo_agotamiento.csv\n")
cat("  - outputs/predicciones/04_importancia_agotamiento.png\n")
cat("  - outputs/predicciones/05_riesgo_agotamiento.png\n")
