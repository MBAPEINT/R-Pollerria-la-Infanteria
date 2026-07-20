# ============================================================
# PREDICCIÓN 6: ¿Cuántas unidades de cada producto se venderán?
# Modelo: Random Forest (regresión)
# Pregunta: Para el próximo día/semana, ¿cuántas unidades debo preparar?
# ============================================================
library(mongolite)
library(randomForest)
library(dplyr)
library(ggplot2)
library(lubridate)

dir.create("outputs/predicciones", showWarnings = FALSE, recursive = TRUE)
set.seed(123)

# --- 1. Cargar datos ---
detalle <- mongo(collection = "Detalles_Venta", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
ventas <- mongo(collection = "Ventas", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find(
  '{}', fields = '{"FechaVenta":1, "_id":0}')

cat("Detalles:", nrow(detalle), "| Ventas:", nrow(ventas), "\n")

# --- 2. Construir dataset diario por producto ---
# Unir fecha de venta al detalle
ventas$FechaVenta <- as.POSIXct(ventas$FechaVenta, tz = "UTC")
detalle$FechaVenta <- as.POSIXct(detalle$FechaVenta, tz = "UTC")

# Si Detalles_Venta no tiene FechaVenta directa (derivado del unpivot), usar IdVenta
if (!"FechaVenta" %in% colnames(detalle) || all(is.na(detalle$FechaVenta))) {
  ventas_idx <- ventas %>%
    mutate(id_venta = row_number()) %>%
    select(id_venta, FechaVenta)
  detalle$FechaVenta <- ventas_idx$FechaVenta[match(detalle$IdVenta, ventas_idx$id_venta)]
}

# Agrupar: ventas por producto por día
ventas_diarias <- detalle %>%
  filter(!is.na(Producto) & Producto != "" & !is.na(FechaVenta)) %>%
  mutate(ds = as.Date(FechaVenta)) %>%
  group_by(Producto, ds) %>%
  summarise(
    cant_diaria = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  )

cat("Combinaciones producto-día:", nrow(ventas_diarias), "\n")

# --- 3. Agregar features temporales ---
ventas_diarias <- ventas_diarias %>%
  mutate(
    dia_semana = as.numeric(format(ds, "%u")),  # 1=Lunes, 7=Domingo
    mes = as.numeric(format(ds, "%m")),
    semana_anio = as.numeric(format(ds, "%U")),
    es_finde = ifelse(dia_semana >= 6, 1, 0)    # sábado o domingo
  )

# --- 4. Para cada producto, entrenar su propio modelo ---
productos_unicos <- unique(ventas_diarias$Producto)
cat("\nProductos a predecir:", length(productos_unicos), "\n")

predicciones <- list()

for (prod in productos_unicos) {
  df_prod <- ventas_diarias %>% filter(Producto == prod)

  if (nrow(df_prod) < 30) next  # mínimo 30 días de datos

  # Variables predictoras y target
  df_prod <- df_prod %>% arrange(ds)
  X <- df_prod[, c("dia_semana", "mes", "semana_anio", "es_finde")]
  y <- df_prod$cant_diaria

  # Entrenar Random Forest
  rf <- tryCatch({
    randomForest(x = X, y = y, ntree = 300)
  }, error = function(e) NULL)

  if (is.null(rf)) next

  # Predecir próxima semana (7 días desde la última fecha)
  ultima_fecha <- max(df_prod$ds)
  futuras_fechas <- seq(ultima_fecha + 1, ultima_fecha + 7, by = "day")

  X_futuro <- data.frame(
    dia_semana = as.numeric(format(futuras_fechas, "%u")),
    mes        = as.numeric(format(futuras_fechas, "%m")),
    semana_anio = as.numeric(format(futuras_fechas, "%U")),
    es_finde   = ifelse(as.numeric(format(futuras_fechas, "%u")) >= 6, 1, 0)
  )

  pred <- predict(rf, X_futuro)
  pred <- pmax(pred, 0)  # no puede ser negativo

  predicciones[[prod]] <- data.frame(
    Producto = prod,
    Fecha = futuras_fechas,
    Cantidad_Predicha = round(pred, 0),
    Dia_Semana = weekdays(futuras_fechas),
    stringsAsFactors = FALSE
  )
}

# --- 5. Consolidar predicciones ---
pred_todas <- bind_rows(predicciones)

# Resumen semanal por producto
resumen_semanal <- pred_todas %>%
  group_by(Producto) %>%
  summarise(
    Total_Semana = sum(Cantidad_Predicha),
    Promedio_Diario = round(mean(Cantidad_Predicha), 1),
    Maximo_Dia = max(Cantidad_Predicha),
    .groups = "drop"
  ) %>%
  arrange(desc(Total_Semana))

cat("\n========== PREDICCIÓN DE UNIDADES — PRÓXIMA SEMANA ==========\n")
cat(sprintf("%-32s %6s %8s %6s\n", "Producto", "Total", "Prom/día", "Máx"))
cat(strrep("-", 56), "\n")
for (i in 1:nrow(resumen_semanal)) {
  cat(sprintf("%-32s %6.0f %8.1f %6.0f\n",
    resumen_semanal$Producto[i],
    resumen_semanal$Total_Semana[i],
    resumen_semanal$Promedio_Diario[i],
    resumen_semanal$Maximo_Dia[i]
  ))
}

# --- 6. Detalle diario de top 5 productos ---
cat("\n========== DETALLE DIARIO — TOP 5 PRODUCTOS ==========\n")
top5 <- resumen_semanal$Producto[1:5]
for (prod in top5) {
  cat("\n  ", prod, ":\n")
  det <- pred_todas %>% filter(Producto == prod) %>% arrange(Fecha)
  for (i in 1:nrow(det)) {
    cat(sprintf("    %s (%s): %3.0f unidades\n",
      as.character(det$Fecha[i]), substr(det$Dia_Semana[i], 1, 3),
      det$Cantidad_Predicha[i]))
  }
}

# --- 7. Gráfico ---
p <- resumen_semanal %>%
  head(10) %>%
  ggplot(aes(x = reorder(Producto, Total_Semana), y = Total_Semana)) +
  geom_col(fill = "#3498DB", width = 0.6) +
  geom_text(aes(label = paste0(round(Total_Semana, 0), " unid.")),
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Predicción de unidades — Próxima semana",
       subtitle = paste0("Total estimado de ventas por producto | ", length(productos_unicos), " productos"),
       x = "", y = "Unidades estimadas") +
  theme_minimal()
ggsave("outputs/predicciones/06_prediccion_unidades.png", p, width = 10, height = 5, dpi = 150)

# --- 8. Guardar ---
write.csv(pred_todas, "data/output/ventas_por_producto_diarias.csv", row.names = FALSE)
write.csv(resumen_semanal, "data/output/ventas_por_producto_semana.csv", row.names = FALSE)

cat("\nArchivos guardados:\n")
cat("  - data/output/ventas_por_producto_diarias.csv\n")
cat("  - data/output/ventas_por_producto_semana.csv\n")
cat("  - outputs/predicciones/06_prediccion_unidades.png\n")
