# ============================================================
# PROYECTO 2: ALERTA DE STOCK BAJO
# Monitorea niveles de stock y sugiere cantidades de compra
# ============================================================
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)

dir.create("outputs/proyectos", showWarnings = FALSE, recursive = TRUE)

# --- 1. Cargar datos ---
insumos <- mongo(collection = "Insumos", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
mov     <- mongo(collection = "Movimientos_Inventario", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
mov$FechaMovimiento <- as.POSIXct(mov$FechaMovimiento, tz = "UTC")

# --- 2. Calcular tasa de consumo diario por insumo (solo salidas) ---
fecha_max <- max(mov$FechaMovimiento, na.rm = TRUE)

consumo <- mov %>%
  filter(Tipo == "Salida") %>%
  group_by(Insumo) %>%
  summarise(
    salida_7d = sum(Cantidad[FechaMovimiento >= fecha_max - days(7)], na.rm = TRUE),
    salida_30d = sum(Cantidad[FechaMovimiento >= fecha_max - days(30)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(consumo_diario = salida_30d / 30)

# --- 3. Unir con stock actual y calcular métricas ---
alerta <- insumos %>%
  left_join(consumo, by = c("Nombre" = "Insumo")) %>%
  mutate(
    across(starts_with("salida"), ~ ifelse(is.na(.), 0, .)),
    consumo_diario = ifelse(is.na(consumo_diario) | consumo_diario == 0, 0.001, consumo_diario),
    dias_restantes = StockActual / consumo_diario,
    porcentaje_stock = round(StockActual / pmax(StockMaximo, 1) * 100, 1),

    # Clasificar nivel de alerta
    nivel = case_when(
      StockActual <= StockMinimo ~ "CRÍTICO",
      dias_restantes <= 3 ~ "URGENTE",
      dias_restantes <= 7 ~ "ALERTA",
      dias_restantes <= 14 ~ "PRECAUCIÓN",
      TRUE ~ "NORMAL"
    ),

    # Sugerir cantidad de compra: reabastecer hasta StockMaximo
    cantidad_sugerida = pmax(0, StockMaximo - StockActual),
    # Ajustar por el consumo durante el tiempo de entrega (asumimos 2 días)
    compra_recomendada = pmax(0, StockMaximo - StockActual + (consumo_diario * 2))
  ) %>%
  arrange(desc(dias_restantes <= 0), dias_restantes)

# --- 4. Mostrar resultados ---
cat("============================================================\n")
cat("  ALERTA DE STOCK —", as.character(Sys.time()), "\n")
cat("============================================================\n\n")

# Críticos
criticos <- alerta %>% filter(nivel == "CRÍTICO")
if (nrow(criticos) > 0) {
  cat(" CRÍTICO — Stock bajo el mínimo. ¡COMPRAR YA!\n")
  cat(" --------------------------------------------------------\n")
  for (i in 1:nrow(criticos)) {
    cat(sprintf("  %-20s Stock: %6.1f | Mín: %6.1f | Comprar: %6.1f\n",
      criticos$Nombre[i], criticos$StockActual[i],
      criticos$StockMinimo[i], criticos$compra_recomendada[i]))
  }
  cat("\n")
}

# Urgentes
urgentes <- alerta %>% filter(nivel == "URGENTE")
if (nrow(urgentes) > 0) {
  cat(" URGENTE — Menos de 3 días de stock. Pedir hoy.\n")
  cat(" --------------------------------------------------------\n")
  for (i in 1:nrow(urgentes)) {
    cat(sprintf("  %-20s Stock: %6.1f | Días: %5.1f | Comprar: %6.1f\n",
      urgentes$Nombre[i], urgentes$StockActual[i],
      urgentes$dias_restantes[i], urgentes$compra_recomendada[i]))
  }
  cat("\n")
}

# Alerta
alerta_items <- alerta %>% filter(nivel == "ALERTA")
if (nrow(alerta_items) > 0) {
  cat(" ALERTA — Agotamiento en 3-7 días. Planificar pedido.\n")
  cat(" --------------------------------------------------------\n")
  for (i in 1:nrow(alerta_items)) {
    cat(sprintf("  %-20s Stock: %6.1f | Días: %5.1f | Comprar: %6.1f\n",
      alerta_items$Nombre[i], alerta_items$StockActual[i],
      alerta_items$dias_restantes[i], alerta_items$compra_recomendada[i]))
  }
  cat("\n")
}

# Precaución
precaucion <- alerta %>% filter(nivel == "PRECAUCIÓN")
if (nrow(precaucion) > 0) {
  cat(" PRECAUCIÓN — 7-14 días de stock. Monitorear.\n")
  cat(" --------------------------------------------------------\n")
  for (i in 1:nrow(precaucion)) {
    cat(sprintf("  %-20s Stock: %6.1f | Días: %5.1f\n",
      precaucion$Nombre[i], precaucion$StockActual[i],
      precaucion$dias_restantes[i]))
  }
  cat("\n")
}

# Normales
normales <- alerta %>% filter(nivel == "NORMAL")
cat(sprintf(" NORMAL — %d insumos con stock suficiente.\n\n", nrow(normales)))

# --- 5. Resumen de compras sugeridas ---
total_compra <- sum(alerta$compra_recomendada[alerta$nivel %in% c("CRÍTICO", "URGENTE", "ALERTA")])
cat(sprintf(">>> MONTO ESTIMADO DE COMPRAS URGENTES: %.1f unidades en total\n\n", total_compra))

# --- 6. Gráfico ---
p <- alerta %>%
  mutate(Nombre = factor(Nombre, levels = rev(Nombre))) %>%
  ggplot(aes(x = Nombre, y = StockActual, fill = nivel)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_point(aes(y = StockMinimo), shape = 23, size = 3, fill = "red", stroke = 1) +
  geom_point(aes(y = StockMaximo), shape = 24, size = 2, fill = "green", alpha = 0.5) +
  scale_fill_manual(values = c(
    "CRÍTICO" = "#E74C3C", "URGENTE" = "#E67E22",
    "ALERTA" = "#F39C12", "PRECAUCIÓN" = "#F1C40F", "NORMAL" = "#2ECC71"
  )) +
  coord_flip() +
  labs(title = "Estado de stock por insumo",
       subtitle = "◊ Stock mínimo | △ Stock máximo | Color = nivel de alerta",
       x = "", y = "Unidades", fill = "Alerta") +
  theme_minimal()
ggsave("outputs/proyectos/02_alerta_stock.png", p, width = 10, height = 6, dpi = 150)

# --- 7. Guardar CSV ---
write.csv(alerta, "data/output/alerta_stock.csv", row.names = FALSE)

cat("Archivos guardados:\n")
cat("  - data/output/alerta_stock.csv (detalle completo)\n")
cat("  - outputs/proyectos/02_alerta_stock.png (gráfico)\n")
