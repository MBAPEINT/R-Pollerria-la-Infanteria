# ============================================================
# PREDICCIÓN 1: Ventas semanales, mensuales y anuales
# Modelo: Prophet (Facebook)
# ============================================================
library(mongolite)
library(prophet)
library(ggplot2)
library(lubridate)
library(dplyr)

dir.create("outputs/predicciones", showWarnings = FALSE, recursive = TRUE)

# --- 1. Cargar datos desde MongoDB ---
con <- mongo(collection = "Ventas", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))
df <- con$find('{}', fields = '{"FechaVenta":1, "Total":1, "_id":0}')

cat("Ventas cargadas:", nrow(df), "\n")

# --- 2. Preparar datos para Prophet (ds, y) ---
df$FechaVenta <- as.POSIXct(df$FechaVenta, tz = "UTC")
head(df$FechaVenta)

# Agrupar por día
ventas_diarias <- df %>%
  mutate(ds = as.Date(FechaVenta)) %>%
  group_by(ds) %>%
  summarise(y = sum(Total, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(ds))

cat("Días con ventas:", nrow(ventas_diarias), "\n")
cat("Rango de fechas:", as.character(min(ventas_diarias$ds)), "a", as.character(max(ventas_diarias$ds)), "\n")
cat("Venta diaria promedio: S/", round(mean(ventas_diarias$y), 2), "\n")
cat("Venta diaria máxima:  S/", round(max(ventas_diarias$y), 2), "\n")

# --- 3. Entrenar Prophet ---
m <- prophet(
  ventas_diarias,
  yearly.seasonality = TRUE,
  weekly.seasonality = TRUE,
  daily.seasonality = FALSE,
  changepoint.prior.scale = 0.05
)

# --- 4. Predicciones ---

# 4a. PRÓXIMA SEMANA (7 días)
futuro_semana <- make_future_dataframe(m, periods = 7, freq = "day")
pred_semana <- predict(m, futuro_semana)
pred_semana_ultimos <- tail(pred_semana, 7)

cat("\n========== PREDICCIÓN PRÓXIMA SEMANA ==========\n")
for (i in 1:7) {
  cat(sprintf("  %s: S/ %s\n",
    as.character(pred_semana_ultimos$ds[i]),
    format(round(pred_semana_ultimos$yhat[i], 0), big.mark = ",")))
}
cat(sprintf("  TOTAL semana: S/ %s\n\n",
  format(round(sum(pred_semana_ultimos$yhat), 0), big.mark = ",")))

# 4b. PRÓXIMO MES (30 días)
futuro_mes <- make_future_dataframe(m, periods = 30, freq = "day")
pred_mes <- predict(m, futuro_mes)
pred_mes_ultimos <- tail(pred_mes, 30)

cat("========== PREDICCIÓN PRÓXIMO MES ==========\n")
ventas_semanales <- data.frame()
for (s in 1:4) {
  ini <- (s-1)*7 + 1
  fin <- min(s*7, 30)
  subtotal <- sum(pred_mes_ultimos$yhat[ini:fin])
  cat(sprintf("  Semana %d: S/ %s\n", s, format(round(subtotal, 0), big.mark = ",")))
}
cat(sprintf("  TOTAL mes: S/ %s\n\n",
  format(round(sum(pred_mes_ultimos$yhat), 0), big.mark = ",")))

# 4c. PRÓXIMO AÑO (365 días)
futuro_anio <- make_future_dataframe(m, periods = 365, freq = "day")
pred_anio <- predict(m, futuro_anio)
pred_anio_ultimos <- tail(pred_anio, 365)

cat("========== PREDICCIÓN PRÓXIMO AÑO ==========\n")
for (m in 1:12) {
  mes_pred <- pred_anio_ultimos %>%
    filter(format(ds, "%m") == sprintf("%02d", m))
  if (nrow(mes_pred) > 0) {
    cat(sprintf("  Mes %02d: S/ %s (%d dias)\n", m,
      format(round(sum(mes_pred$yhat), 0), big.mark = ","), nrow(mes_pred)))
  }
}
cat(sprintf("  TOTAL año: S/ %s\n",
  format(round(sum(pred_anio_ultimos$yhat), 0), big.mark = ",")))

# --- 5. Gráficos ---

# Gráfico 1: Histórico + predicción a 60 días
pred_plot <- pred_mes
pred_plot$tipo <- ifelse(pred_plot$ds <= max(ventas_diarias$ds), "Histórico", "Predicción")

p1 <- ggplot(pred_plot, aes(x = ds, y = yhat, color = tipo)) +
  geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.15, fill = "#3498DB") +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = c("Histórico" = "#2C3E50", "Predicción" = "#3498DB")) +
  labs(title = "Predicción de ventas diarias",
       subtitle = paste0(nrow(ventas_diarias), " días históricos + 30 días pronóstico"),
       x = "Fecha", y = "Ventas diarias (S/)", color = "") +
  theme_minimal() + theme(legend.position = "bottom")
ggsave("outputs/predicciones/01_prediccion_mes.png", p1, width = 12, height = 5, dpi = 150)

# Gráfico 2: Componentes (tendencia + estacionalidad semanal)
comps <- data.frame(
  ds = pred_anio$ds,
  Tendencia = pred_anio$trend,
  Estacionalidad_Semanal = pred_anio$weekly,
  Estacionalidad_Anual = pred_anio$yearly
)

p2_trend <- ggplot(comps, aes(x = ds, y = Tendencia)) +
  geom_line(color = "#2C3E50", linewidth = 0.8) +
  labs(title = "Tendencia de ventas", x = "", y = "S/") + theme_minimal()

p2_week <- comps %>%
  mutate(dia_sem = weekdays(ds)) %>%
  group_by(dia_sem) %>%
  summarise(efecto = mean(Estacionalidad_Semanal), .groups = "drop") %>%
  mutate(dia_sem = factor(dia_sem, levels = c("lunes","martes","miércoles","jueves","viernes","sábado","domingo"))) %>%
  ggplot(aes(x = dia_sem, y = efecto)) +
  geom_col(fill = "#3498DB", width = 0.6) +
  labs(title = "Efecto día de la semana", x = "", y = "S/") + theme_minimal()

p2_year <- comps %>%
  mutate(mes = as.numeric(format(ds, "%m"))) %>%
  group_by(mes) %>%
  summarise(efecto = mean(Estacionalidad_Anual), .groups = "drop") %>%
  ggplot(aes(x = mes, y = efecto)) +
  geom_col(fill = "#E74C3C", width = 0.6) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(title = "Efecto mensual", x = "", y = "S/") + theme_minimal()

library(gridExtra)
p2 <- gridExtra::grid.arrange(p2_trend, p2_week, p2_year, ncol = 1)
ggsave("outputs/predicciones/02_componentes_prophet.png", p2, width = 10, height = 10, dpi = 150)

# Gráfico 3: Zoom a la próxima semana
ultima_fecha <- max(ventas_diarias$ds)
semana_pred <- pred_mes %>%
  filter(ds > ultima_fecha, ds <= ultima_fecha + 7)

p3 <- ggplot(semana_pred, aes(x = ds, y = yhat)) +
  geom_col(fill = "#3498DB", width = 0.7) +
  geom_text(aes(label = paste0("S/", format(round(yhat, 0), big.mark = ","))),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Predicción de ventas — Próxima semana",
       subtitle = "Cada barra = ventas estimadas del día",
       x = "", y = "Ventas estimadas (S/)") +
  theme_minimal()
ggsave("outputs/predicciones/03_prediccion_semana.png", p3, width = 10, height = 5, dpi = 150)

# --- 6. Guardar predicciones en CSV ---
write.csv(pred_anio[, c("ds", "yhat", "yhat_lower", "yhat_upper")],
          "data/output/ventas_prediccion_anual.csv", row.names = FALSE)

cat("\nArchivos guardados en data/output/ y outputs/predicciones/\n")
cat("  - data/output/ventas_prediccion_anual.csv\n")
cat("  - outputs/predicciones/01_prediccion_mes.png\n")
cat("  - outputs/predicciones/02_componentes_prophet.png\n")
cat("  - outputs/predicciones/03_prediccion_semana.png\n")
