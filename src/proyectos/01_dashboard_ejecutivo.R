# ============================================================
# PROYECTO 1: DASHBOARD EJECUTIVO
# Genera gráficos PNG + reporte HTML con KPIs del negocio
# ============================================================
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

dir.create("outputs/proyectos", showWarnings = FALSE, recursive = TRUE)

# --- 1. Cargar datos ---
ventas   <- mongo(collection = "Ventas", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
detalle  <- mongo(collection = "Detalles_Venta", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
insumos  <- mongo(collection = "Insumos", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
clientes <- mongo(collection = "Clientes", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')

ventas$FechaVenta <- as.Date(as.POSIXct(ventas$FechaVenta, tz = "UTC"))
hoy <- max(ventas$FechaVenta, na.rm = TRUE)
ayer <- hoy - 1
inicio_semana <- hoy - 6
inicio_mes <- floor_date(hoy, "month")
inicio_30d <- hoy - 30

# Tema común
tema <- theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold", size = 12),
        panel.grid.minor = element_blank())

# --- 2. CALCULAR KPIs ---
ventas_hoy <- ventas %>% filter(FechaVenta == hoy)
total_hoy <- sum(ventas_hoy$Total, na.rm = TRUE)
n_ventas_hoy <- nrow(ventas_hoy)
ticket_hoy <- ifelse(n_ventas_hoy > 0, round(total_hoy / n_ventas_hoy, 2), 0)

ventas_ayer <- ventas %>% filter(FechaVenta == ayer)
total_ayer <- sum(ventas_ayer$Total, na.rm = TRUE)
var_diaria <- ifelse(total_ayer > 0, round((total_hoy - total_ayer) / total_ayer * 100, 1), 0)

ventas_semana <- ventas %>% filter(FechaVenta >= inicio_semana)
total_semana <- sum(ventas_semana$Total, na.rm = TRUE)
n_ventas_semana <- nrow(ventas_semana)
ticket_semana <- round(mean(ventas_semana$Total, na.rm = TRUE), 2)

ventas_mes <- ventas %>% filter(FechaVenta >= inicio_mes)
total_mes <- sum(ventas_mes$Total, na.rm = TRUE)

clientes_activos <- ventas %>%
  filter(FechaVenta >= inicio_30d) %>%
  summarise(n = n_distinct(Cliente)) %>% pull()

insumos_bajo <- insumos %>% filter(StockActual <= StockMinimo)

# --- 3. GRÁFICOS ---
# Gráfico 1: Ventas últimos 7 días
p1 <- ventas %>%
  filter(FechaVenta >= inicio_semana) %>%
  group_by(FechaVenta) %>%
  summarise(Total = sum(Total, na.rm = TRUE), Ventas = n(), .groups = "drop") %>%
  ggplot(aes(x = FechaVenta, y = Total)) +
  geom_col(fill = "#3498DB", width = 0.7) +
  geom_text(aes(label = paste0("S/", format(round(Total/1000, 1)), "k")),
            vjust = -0.5, size = 3.5, fontface = "bold") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Ventas — Últimos 7 días", x = "", y = "S/") + tema
ggsave("outputs/proyectos/dash_ventas_semana.png", p1, width = 8, height = 4, dpi = 130)

# Gráfico 2: Evolución mensual últimos 12 meses
ultimos_12 <- ventas %>%
  filter(FechaVenta >= hoy - 365) %>%
  mutate(mes = floor_date(FechaVenta, "month")) %>%
  group_by(mes) %>%
  summarise(Total = sum(Total, na.rm = TRUE), Ventas = n(), .groups = "drop")

p2 <- ggplot(ultimos_12, aes(x = mes, y = Total)) +
  geom_area(fill = "#3498DB", alpha = 0.2) +
  geom_line(color = "#3498DB", linewidth = 1) +
  geom_point(color = "#2C3E50", size = 2) +
  scale_y_continuous(labels = function(x) paste0("S/", format(round(x/1000, 0), big.mark = ","), "k")) +
  labs(title = "Evolución mensual — Último año", x = "", y = "") + tema +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("outputs/proyectos/dash_evolucion_mensual.png", p2, width = 8, height = 4, dpi = 130)

# Gráfico 3: Top 10 productos de la semana
p3 <- detalle %>%
  filter(as.Date(FechaVenta) >= inicio_semana) %>%
  group_by(Producto) %>%
  summarise(Cantidad = sum(Cantidad, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Cantidad)) %>% head(10) %>%
  ggplot(aes(x = reorder(Producto, Cantidad), y = Cantidad)) +
  geom_col(fill = "#2ECC71", width = 0.6) +
  geom_text(aes(label = Cantidad), hjust = -0.3, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Top 10 productos — Semana", x = "", y = "Unidades vendidas") + tema
ggsave("outputs/proyectos/dash_top_productos.png", p3, width = 8, height = 4, dpi = 130)

# Gráfico 4: Insumos bajo mínimo
p4 <- insumos %>%
  mutate(alerta = ifelse(StockActual <= StockMinimo, "BAJO MÍNIMO", "OK")) %>%
  ggplot(aes(x = reorder(Nombre, StockActual), y = StockActual, fill = alerta)) +
  geom_col(width = 0.6) +
  geom_point(aes(y = StockMinimo), shape = 23, size = 3, fill = "red") +
  coord_flip() +
  scale_fill_manual(values = c("BAJO MÍNIMO" = "#E74C3C", "OK" = "#3498DB")) +
  labs(title = "Stock actual vs mínimo (◊)", x = "", y = "Unidades", fill = "") +
  tema + theme(legend.position = "bottom")
ggsave("outputs/proyectos/dash_stock.png", p4, width = 8, height = 4, dpi = 130)

# --- 4. Generar HTML ---
html <- 'outputs/proyectos/dashboard_ejecutivo.html'

cat('<!DOCTYPE html>
<html lang="es">
<head>
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1.0">
<title>Dashboard — Pollería la Infantería</title>
<style>
  *{margin:0;padding:0;box-sizing:border-box;font-family:"Segoe UI",Arial,sans-serif}
  body{background:#f0f2f5;padding:20px;color:#2C3E50}
  .header{background:linear-gradient(135deg,#1a1a2e,#16213e);color:white;padding:25px 30px;border-radius:12px;margin-bottom:20px}
  .header h1{font-size:26px}.header p{opacity:.8;margin-top:5px;font-size:14px}
  .kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:15px;margin-bottom:20px}
  .kpi{background:white;padding:20px;border-radius:10px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
  .kpi .label{font-size:11px;color:#7f8c8d;text-transform:uppercase;letter-spacing:1px}
  .kpi .value{font-size:30px;font-weight:700;margin:5px 0}
  .kpi .sub{font-size:12px;color:#95a5a6}
  .up{color:#27ae60}.down{color:#e74c3c}
  .chart-grid{display:grid;grid-template-columns:1fr 1fr;gap:15px;margin-bottom:20px}
  .card{background:white;padding:15px;border-radius:10px;box-shadow:0 1px 3px rgba(0,0,0,.08)}
  .card h3{font-size:15px;margin-bottom:10px}
  .card img{width:100%}
  .alert{grid-template-columns:1fr 1fr;display:grid;gap:15px;margin-bottom:20px}
  .red-bar{border-left:4px solid #e74c3c}.green-bar{border-left:4px solid #27ae60}
  .alert-item{padding:6px 0;border-bottom:1px solid #eee;font-size:13px}
  .footer{text-align:center;padding:20px;color:#95a5a6;font-size:12px}
</style>
</head>
<body>
<div class="header">
  <h1>Pollería la Infantería</h1>
  <p>Dashboard generado: ', format(Sys.time(), "%d/%m/%Y %H:%M"),
  ' | Último día con datos: ', as.character(hoy),
  ' | ', format(nrow(clientes), big.mark=","), ' clientes registrados</p>
</div>

<div class="kpi-grid">
  <div class="kpi">
    <div class="label">Ventas último día</div>
    <div class="value">S/', format(round(total_hoy,0),big.mark=","), '</div>
    <div class="sub ', ifelse(var_diaria>=0,"up","down"), '">',
    ifelse(var_diaria>=0,"▲","▼"), ' ', abs(var_diaria), '% vs día anterior | ',
    n_ventas_hoy, ' transacciones</div>
  </div>
  <div class="kpi">
    <div class="label">Ticket promedio (semana)</div>
    <div class="value">S/', ticket_semana, '</div>
    <div class="sub">Semana: ', format(n_ventas_semana, big.mark=","), ' ventas</div>
  </div>
  <div class="kpi">
    <div class="label">Ventas del mes</div>
    <div class="value">S/', format(round(total_mes/1000,1),big.mark=","), 'k</div>
    <div class="sub">Total acumulado del mes en curso</div>
  </div>
  <div class="kpi">
    <div class="label">Clientes activos (30d)</div>
    <div class="value">', format(clientes_activos,big.mark=","), '</div>
    <div class="sub">', round(clientes_activos/nrow(clientes)*100,1), '% del total de registrados</div>
  </div>
</div>

<div class="chart-grid">
  <div class="card"><h3>Ventas últimos 7 días</h3><img src="dash_ventas_semana.png" alt="ventas"></div>
  <div class="card"><h3>Evolución mensual — Último año</h3><img src="dash_evolucion_mensual.png" alt="evolucion"></div>
</div>

<div class="chart-grid">
  <div class="card"><h3>Top 10 productos — Esta semana</h3><img src="dash_top_productos.png" alt="productos"></div>
  <div class="card"><h3>Estado de stock</h3><img src="dash_stock.png" alt="stock"></div>
</div>

<div class="alert">
  <div class="card ', ifelse(nrow(insumos_bajo)>0,"red-bar","green-bar"), '">
    <h3>', ifelse(nrow(insumos_bajo)>0,"⚠ INSUMOS BAJO STOCK MÍNIMO","✓ Todos los insumos OK"), '</h3>',
    file = html)

if (nrow(insumos_bajo) > 0) {
  for (i in 1:nrow(insumos_bajo)) {
    cat(sprintf('<div class="alert-item"><b>%s</b>: Stock %.1f (mín %.1f, máx %.1f)</div>',
      insumos_bajo$Nombre[i], insumos_bajo$StockActual[i],
      insumos_bajo$StockMinimo[i], insumos_bajo$StockMaximo[i]),
      file = html, append = TRUE)
  }
}

cat('
  </div>
  <div class="card">
    <h3>Resumen de la semana</h3>
    <div style="padding:10px;font-size:14px;">
      <p>Total semana: <b>S/', format(round(total_semana,0),big.mark=","), '</b></p>
      <p>Ticket promedio: <b>S/', ticket_semana, '</b></p>
      <p>Transacciones: <b>', format(n_ventas_semana,big.mark=","), '</b></p>
      <p>Clientes activos (30d): <b>', format(clientes_activos,big.mark=","), '</b></p>
      <p>Insumos en alerta: <b>', nrow(insumos_bajo), ' de ', nrow(insumos), '</b></p>
    </div>
  </div>
</div>

<div class="footer">Pollería la Infantería — Pipeline ETL R + MongoDB — Dashboard automático</div>
</body></html>',
  file = html, append = TRUE)

cat("\nDashboard generado: outputs/proyectos/dashboard_ejecutivo.html\n")
cat("Ábrelo en tu navegador.\n")
