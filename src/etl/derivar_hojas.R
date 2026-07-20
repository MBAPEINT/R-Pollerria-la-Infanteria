# ============================================================
# derivar_hojas.R
# Funciones para derivar hojas de detalle desde hojas principales
# cuando el Excel está en formato crudo (ancho) en vez de
# normalizado (largo).
#
# Uso: source("derivar_hojas.R") desde main.R
# ============================================================
library(dplyr)
library(tidyr)

# ------------------------------------------------------------
# 1. Derivar Detalles_Venta desde Ventas (Producto1..7)
#    Columnas crudas: ProductoN_Nombre, ProductoN_Cantidad,
#                     ProductoN_PrecioUnitario, ProductoN_Subtotal
#    → Columnas derivadas: Producto, Cantidad, PrecioUnitario,
#      Subtotal, FechaVenta, Cliente, Personal, MetodoPago, Total
# ------------------------------------------------------------
unpivot_ventas_detalle <- function(df) {
  df %>%
    # Evitar colisión: las columnas Subtotal/Total de Ventas se renombran
    rename(VentaSubtotal = Subtotal, VentaTotal = Total) %>%
    # ID único por venta para trazabilidad
    mutate(IdVenta = row_number()) %>%
    pivot_longer(
      cols = starts_with("Producto"),
      names_to = c("pos", ".value"),
      names_pattern = "Producto(\\d+)_(.*)"
    ) %>%
    # Solo filas que realmente tienen un producto
    filter(!is.na(Nombre) & Nombre != "NO_ESPECIFICADO") %>%
    rename(
      Producto = Nombre,
      Cantidad = Cantidad,
      PrecioUnitario = PrecioUnitario,
      Subtotal = Subtotal
    ) %>%
    select(-pos)
}

# ------------------------------------------------------------
# 2. Derivar Detalle_Orden_Compra desde Ordenes_Compra (Insumo1..5)
#    Columnas crudas: InsumoN_Nombre, InsumoN_Cantidad,
#                     InsumoN_PrecioUnitario, InsumoN_Subtotal
#    → Columnas derivadas: Insumo, Cantidad, PrecioUnitario,
#      Subtotal, IdOrdenCompra, Proveedor, FechaPedido
# ------------------------------------------------------------
unpivot_ordenes_compra_detalle <- function(df) {
  df %>%
    # ID único por orden de compra
    mutate(IdOrdenCompra = row_number()) %>%
    pivot_longer(
      cols = starts_with("Insumo"),
      names_to = c("pos", ".value"),
      names_pattern = "Insumo(\\d+)_(.*)"
    ) %>%
    # Solo filas con insumo real
    filter(!is.na(Nombre) & Nombre != "") %>%
    rename(
      Insumo = Nombre,
      Cantidad = Cantidad,
      PrecioUnitario = PrecioUnitario,
      Subtotal = Subtotal
    ) %>%
    select(-pos)
}

# ------------------------------------------------------------
# 3. Derivar Detalle_Insumo desde Productos (Insumo1..7)
#    Columnas crudas: InsumoN_Nombre, InsumoN_Cantidad
#    → Columnas derivadas: IdProducto, Insumo, Cantidad
# ------------------------------------------------------------
unpivot_productos_insumos <- function(df) {
  df %>%
    rename(IdProducto = NombreProducto) %>%
    pivot_longer(
      cols = starts_with("Insumo"),
      names_to = c("pos", ".value"),
      names_pattern = "Insumo(\\d+)_(.*)"
    ) %>%
    filter(!is.na(Nombre) & Nombre != "") %>%
    rename(
      Insumo = Nombre,
      Cantidad = Cantidad
    ) %>%
    select(-pos)
}

# ------------------------------------------------------------
# 4. Derivar Unidades_Medida desde Insumos
#    Extrae los valores únicos de UnidadMedida y asigna un ID
#    → Columnas: IdUnidadMedida, Nombre (o UnidadMedida)
# ------------------------------------------------------------
extraer_unidades_medida <- function(df) {
  df %>%
    distinct(UnidadMedida) %>%
    filter(!is.na(UnidadMedida) & UnidadMedida != "") %>%
    mutate(IdUnidadMedida = row_number()) %>%
    rename(Nombre = UnidadMedida) %>%
    select(IdUnidadMedida, Nombre)
}
