# ============================================================
# PROYECTO 7: DETECCIÓN DE ANOMALÍAS EN VENTAS
# Método: Análisis multidimensional de anomalías
# Detecta: errores de captura, ventas sospechosas, patrones raros
# ============================================================
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)

dir.create("outputs/proyectos", showWarnings = FALSE, recursive = TRUE)

# --- 1. Cargar datos ---
ventas  <- mongo(collection = "Ventas", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
detalle <- mongo(collection = "Detalles_Venta", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')

ventas$FechaVenta <- as.POSIXct(ventas$FechaVenta, tz = "UTC")

# --- 2. Calcular características por venta ---
ventas <- ventas %>%
  mutate(
    hora = as.numeric(format(FechaVenta, "%H")),
    dia_semana = as.numeric(format(FechaVenta, "%u")),
    mes = as.numeric(format(FechaVenta, "%m"))
  )

# Agregar cantidad de productos por venta
n_prod <- detalle %>%
  group_by(IdVenta) %>%
  summarise(
    cant_productos = n(),
    cant_total_items = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  )

ventas <- ventas %>%
  mutate(IdVenta = row_number()) %>%
  left_join(n_prod, by = "IdVenta") %>%
  mutate(
    cant_productos = ifelse(is.na(cant_productos), 1, cant_productos),
    cant_total_items = ifelse(is.na(cant_total_items), 1, cant_total_items)
  )

# --- 3. Detectar anomalías por dimensión ---

# Dimensión 1: Total de venta muy alto (> 3 desviaciones de la mediana diaria)
umbral_total <- median(ventas$Total, na.rm = TRUE) +
  5 * sd(ventas$Total, na.rm = TRUE)
ventas$anom_total <- ventas$Total > umbral_total

# Dimensión 2: Hora fuera de lo normal
# Excluir medianoche (00:00) porque es el default cuando no se registró hora
ventas$anom_hora <- ventas$hora >= 1 & ventas$hora <= 5

# Dimensión 3: Ticket atípico para el día de la semana
ticket_por_dia <- ventas %>%
  group_by(dia_semana) %>%
  summarise(
    media_ticket = mean(Total, na.rm = TRUE),
    sd_ticket = sd(Total, na.rm = TRUE),
    .groups = "drop"
  )
ventas <- ventas %>%
  left_join(ticket_por_dia, by = "dia_semana") %>%
  mutate(anom_ticket_dia = abs(Total - media_ticket) > 4 * sd_ticket)

# Dimensión 4: Demasiados productos para el ticket (ej: 10 productos por S/ 20)
ventas$ratio <- ventas$cant_productos / pmax(ventas$Total, 0.01)
umbral_ratio <- median(ventas$ratio, na.rm = TRUE) +
  4 * sd(ventas$ratio, na.rm = TRUE)
ventas$anom_ratio <- ventas$ratio > umbral_ratio

# Dimensión 5: Método de pago atípico para el monto
pago_montos <- ventas %>%
  group_by(MetodoPago) %>%
  summarise(
    media_metodo = mean(Total, na.rm = TRUE),
    sd_metodo = sd(Total, na.rm = TRUE),
    .groups = "drop"
  )
ventas <- ventas %>%
  left_join(pago_montos, by = "MetodoPago") %>%
  mutate(anom_pago = abs(Total - media_metodo) > 4 * sd_metodo)

# --- 4. Score de anomalía: suma de banderas ---
ventas <- ventas %>%
  mutate(
    score_anomalia = anom_total + anom_hora + anom_ticket_dia + anom_ratio + anom_pago,
    es_anomalia = score_anomalia >= 2  # al menos 2 dimensiones anómalas
  )

# --- 5. Resultados ---
anomalas <- ventas %>% filter(es_anomalia)
cat("============================================================\n")
cat("  DETECCIÓN DE ANOMALÍAS —", as.character(Sys.time()), "\n")
cat("============================================================\n\n")
cat(sprintf("Total de ventas analizadas: %d\n", nrow(ventas)))
cat(sprintf("Anomalías detectadas: %d (%.2f%%)\n\n",
  nrow(anomalas), 100 * nrow(anomalas) / nrow(ventas)))

if (nrow(anomalas) > 0) {
  cat("========== VENTAS ANÓMALAS DETECTADAS ==========\n\n")
  for (i in 1:min(20, nrow(anomalas))) {
    a <- anomalas[i, ]
    razones <- c()
    if (a$anom_total) razones <- c(razones, "Monto muy alto")
    if (a$anom_hora) razones <- c(razones, "Hora inusual")
    if (a$anom_ticket_dia) razones <- c(razones, "Ticket atípico para el día")
    if (a$anom_ratio) razones <- c(razones, "Ratio productos/monto extraño")
    if (a$anom_pago) razones <- c(razones, "Monto atípico para método de pago")

    cat(sprintf("Venta #%d | %s | Total: S/ %.2f | %d productos | Score: %d\n",
      a$IdVenta, format(a$FechaVenta, "%Y-%m-%d %H:%M"),
      a$Total, a$cant_productos, a$score_anomalia))
    cat(sprintf("  Razones: %s\n\n", paste(razones, collapse = ", ")))
  }
}

# --- 6. Resumen por tipo de anomalía ---
cat("========== FRECUENCIA POR TIPO DE ANOMALÍA ==========\n")
cat(sprintf("  Monto muy alto:                 %d ventas\n", sum(ventas$anom_total)))
cat(sprintf("  Hora inusual (madrugada):       %d ventas\n", sum(ventas$anom_hora)))
cat(sprintf("  Ticket atípico para el día:     %d ventas\n", sum(ventas$anom_ticket_dia)))
cat(sprintf("  Ratio productos/monto extraño:  %d ventas\n", sum(ventas$anom_ratio)))
cat(sprintf("  Monto atípico método de pago:   %d ventas\n", sum(ventas$anom_pago)))

# --- 7. Gráficos ---
# Histograma de scores
p1 <- ventas %>%
  count(score_anomalia) %>%
  ggplot(aes(x = factor(score_anomalia), y = n, fill = score_anomalia >= 2)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(n, "\n(", round(n/sum(n)*100, 1), "%)")),
            vjust = -0.2, size = 3.5) +
  scale_fill_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Distribución del score de anomalía",
       subtitle = paste0("Anómalas (score ≥ 2): ", nrow(anomalas), " de ", nrow(ventas)),
       x = "Score de anomalía (0=normal, 5=muy anómalo)", y = "Cantidad de ventas") +
  theme_minimal()
ggsave("outputs/proyectos/07_anomalias_score.png", p1, width = 8, height = 5, dpi = 150)

# Dispersión: Total vs cantidad de productos, anomalías en rojo
p2 <- ventas %>%
  ggplot(aes(x = cant_productos, y = Total, color = es_anomalia, size = score_anomalia)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB")) +
  scale_size_continuous(range = c(0.3, 3)) +
  labs(title = "Anomalías: Total de venta vs Cantidad de productos",
       subtitle = "Rojo = anómalo | Tamaño = severidad",
       x = "Cantidad de productos", y = "Total (S/)",
       color = "Anomalía", size = "Score") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("outputs/proyectos/07_anomalias_dispersion.png", p2, width = 10, height = 6, dpi = 150)

# --- 8. Guardar ---
write.csv(anomalas, "data/output/anomalias_detectadas.csv", row.names = FALSE)

cat("\nArchivos guardados:\n")
cat("  - data/output/anomalias_detectadas.csv (", nrow(anomalas), " ventas anómalas)\n", sep = "")
cat("  - outputs/proyectos/07_anomalias_score.png\n")
cat("  - outputs/proyectos/07_anomalias_dispersion.png\n")
