# ============================================================
# consultas.R
# 5 consultas básicas + 5 consultas avanzadas
# Lee datos limpios (datos_outliers.xlsx) — valores en escala original
# La hoja Ventas está en formato ancho (Producto1..7),
# se hace unpivot en R para A2 y A5.
# ============================================================
library(readxl)
library(dplyr)
library(tidyr)

INPUT <- "data/processed/datos_outliers.xlsx"
hojas <- setdiff(excel_sheets(INPUT), "Resumen")
datos <- list()
for (h in hojas) datos[[h]] <- read_excel(INPUT, sheet = h)

cat("============================================================\n")
cat("  CONSULTAS — Pollería la Infantería\n")
cat("  Datos: ", INPUT, "\n")
cat("============================================================\n\n")

# ============================================================
# FUNCIÓN AUXILIAR: Unpivot de Producto1..7 en Ventas
# Convierte columnas ProductoN_Nombre/Cantidad/PrecioUnitario/Subtotal
# en filas normalizadas (una fila por producto dentro de cada venta)
# ============================================================
unpivot_ventas <- function(df) {
  df %>%
    # Renombrar columnas que colisionan con los nombres del unpivot
    rename(VentaSubtotal = Subtotal, VentaTotal = Total) %>%
    # Crear ID temporal de venta
    mutate(id_venta = row_number()) %>%
    # Pivotar los 4 atributos de cada posición de producto
    pivot_longer(
      cols = starts_with("Producto"),
      names_to = c("pos", ".value"),
      names_pattern = "Producto(\\d+)_(.*)"
    ) %>%
    # Filtrar filas sin producto (NA en Nombre)
    filter(!is.na(Nombre) & Nombre != "NO_ESPECIFICADO") %>%
    # Renombrar para claridad
    rename(
      Producto = Nombre,
      Cantidad = Cantidad,
      PrecioUnitario = PrecioUnitario,
      SubtotalProd = Subtotal
    ) %>%
    select(-pos)
}


# ============================================================
# PUNTO a) — 5 CONSULTAS BÁSICAS
# ============================================================

cat("────────── 5 CONSULTAS BÁSICAS ──────────\n\n")

# B1. Buscar cliente por correo
#     Requerimiento: atención al cliente (verificar identidad)
b1 <- datos$Clientes %>%
  filter(Correo == "elena.mendoza2861@gmail.com") %>%
  select(PrimerNombre, ApellidoPaterno, Correo, Telefono)
cat("B1. Cliente por correo:\n")
print(b1)
cat("\n")

# B2. Productos de una categoría
#     Requerimiento: catálogo segmentado para menú digital
b2 <- datos$Productos %>%
  filter(Categoria == "BEBIDAS") %>%
  select(NombreProducto, Precio, Categoria)
cat("B2. Productos categoría BEBIDAS:\n")
print(b2)
cat("\n")

# B3. Ventas de un vendedor específico (por nombre)
#     Requerimiento: seguimiento de desempeño individual
b3 <- datos$Ventas %>%
  filter(Personal == "Carla Beatriz Sánchez Torres") %>%
  select(FechaVenta, Total, Cliente, MetodoPago) %>%
  head(10)
cat("B3. Ventas del vendedor 'Carla Beatriz Sánchez Torres' (primeras 10):\n")
print(b3)
cat(sprintf("   Total ventas encontradas: %d\n",
    nrow(datos$Ventas %>% filter(Personal == "Carla Beatriz Sánchez Torres"))))
cat("\n")

# B4. Ventas de una fecha específica
#     Requerimiento: cierre de caja diario / auditoría
b4 <- datos$Ventas %>%
  filter(FechaVenta >= as.POSIXct("2023-05-02", tz = "UTC"),
         FechaVenta <  as.POSIXct("2023-05-03", tz = "UTC")) %>%
  select(FechaVenta, Total, Cliente, MetodoPago)
cat("B4. Ventas del 2023-05-02:\n")
cat(sprintf("   Total ventas: %d | Monto total: S/ %.2f\n",
    nrow(b4), sum(b4$Total)))
cat("\n")

# B5. Insumos bajo stock mínimo
#     Requerimiento: alerta de reposición urgente
b5 <- datos$Insumos %>%
  filter(StockActual <= StockMinimo) %>%
  select(Nombre, StockActual, StockMinimo, UnidadMedida) %>%
  mutate(Diferencia = StockActual - StockMinimo)
cat("B5. Insumos bajo stock mínimo:\n")
if (nrow(b5) > 0) { print(b5)
} else { cat("   ✓ Todos los insumos están sobre el stock mínimo.\n") }
cat("\n")


# ============================================================
# PUNTO b) — 5 CONSULTAS AVANZADAS
# ============================================================

cat("────────── 5 CONSULTAS AVANZADAS ──────────\n\n")

# A1. Ventas por día de la semana
#     Requerimiento: planificación de personal y stock
dias <- c("Lunes", "Martes", "Miércoles", "Jueves", "Viernes", "Sábado", "Domingo")
a1 <- datos$Ventas %>%
  mutate(dia_semana = dias[as.numeric(format(FechaVenta, "%u"))]) %>%
  group_by(dia_semana) %>%
  summarise(
    monto_total = sum(Total),
    num_ventas = n(),
    ticket_promedio = round(mean(Total), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(monto_total))
cat("A1. Ventas por día de la semana:\n")
print(a1)
cat("\n")

# A2. Top 5 productos más vendidos (cantidad)
#     Requerimiento: gestión de inventario / reposición prioritaria
#     → Se hace unpivot de Producto1..7 en Ventas
a2 <- unpivot_ventas(datos$Ventas) %>%
  group_by(Producto) %>%
  summarise(
    cantidad_total = sum(Cantidad),
    ventas = n(),
    ingreso_total = sum(SubtotalProd),
    .groups = "drop"
  ) %>%
  arrange(desc(cantidad_total)) %>%
  head(5)
cat("A2. Top 5 productos más vendidos (cantidad):\n")
print(a2)
cat("\n")

# A3. Ticket promedio por método de pago
#     Requerimiento: estrategia de medios de pago
a3 <- datos$Ventas %>%
  group_by(MetodoPago) %>%
  summarise(
    ticket_promedio = round(mean(Total), 2),
    num_ventas = n(),
    monto_total = sum(Total),
    .groups = "drop"
  ) %>%
  arrange(desc(ticket_promedio))
cat("A3. Ticket promedio por método de pago:\n")
print(a3)
cat("\n")

# A4. Top 10 clientes por gasto acumulado
#     Requerimiento: programa de fidelización / marketing dirigido
a4 <- datos$Ventas %>%
  group_by(Cliente) %>%
  summarise(
    gasto_total = sum(Total),
    num_compras = n(),
    ticket_promedio = round(mean(Total), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(gasto_total)) %>%
  head(10)
cat("A4. Top 10 clientes por gasto acumulado:\n")
print(a4)
cat("\n")

# A5. Ingresos por categoría de producto y mes
#     Requerimiento: planificación estacional de compras/stock
#     → Unpivot + join con Productos para obtener Categoria
meses <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
           "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
a5 <- unpivot_ventas(datos$Ventas) %>%
  mutate(mes = as.numeric(format(FechaVenta, "%m"))) %>%
  left_join(
    datos$Productos %>% select(NombreProducto, Categoria),
    by = c("Producto" = "NombreProducto")
  ) %>%
  group_by(Categoria, mes) %>%
  summarise(
    ingresos = sum(SubtotalProd),
    num_lineas = n(),
    .groups = "drop"
  ) %>%
  mutate(mes_nombre = meses[mes]) %>%
  arrange(mes, desc(ingresos)) %>%
  select(Categoria, mes_nombre, ingresos, num_lineas)
cat("A5. Ingresos por categoría de producto y mes:\n")
print(a5)
cat("\n")


# ============================================================
# PUNTO c) — PATRONES DE NEGOCIO
# ============================================================

cat("────────── PATRONES DE NEGOCIO ──────────\n\n")

# Patrón 1: Día de la semana con mayor volumen de ventas
cat("PATRÓN 1 — Día de mayor volumen:\n")
mejor_dia <- a1[1, ]
cat(sprintf("  %s concentra S/ %.2f (%d ventas, ticket S/ %.2f)\n",
    mejor_dia$dia_semana, mejor_dia$monto_total,
    mejor_dia$num_ventas, mejor_dia$ticket_promedio))
cat(sprintf("  Recomendación: reforzar personal y stock el %s.\n\n",
    mejor_dia$dia_semana))

# Patrón 2: Método de pago con menor ticket promedio
cat("PATRÓN 2 — Método de pago con menor ticket:\n")
peor_metodo <- a3 %>% arrange(ticket_promedio) %>% slice(1)
cat(sprintf("  %s tiene ticket S/ %.2f (%d transacciones)\n",
    peor_metodo$MetodoPago, peor_metodo$ticket_promedio, peor_metodo$num_ventas))
cat("  Recomendación: mantener todos los métodos; diferencia es mínima.\n\n")

# Patrón 3: Concentración de ingresos (regla 80/20)
cat("PATRÓN 3 — Concentración de ingresos en clientes top:\n")
gasto_total <- datos$Ventas %>%
  group_by(Cliente) %>%
  summarise(gasto = sum(Total), .groups = "drop") %>%
  arrange(desc(gasto))
top20_n <- ceiling(nrow(gasto_total) * 0.2)
pct_top20 <- round(100 * sum(gasto_total$gasto[1:top20_n]) / sum(gasto_total$gasto), 1)
cat(sprintf("  %d clientes únicos | Top 20%% (%d) genera %.1f%% de ingresos\n",
    nrow(gasto_total), top20_n, pct_top20))
if (pct_top20 >= 80) {
  cat("  ⚠ Se CUMPLE regla 80/20. Riesgo: dependencia de pocos clientes.\n")
} else {
  cat("  ✓ NO se cumple regla 80/20 estricta. Base de clientes diversificada.\n")
}
cat("  Recomendación: programa VIP para top 20% + frecuencia para bottom 80%.\n")
