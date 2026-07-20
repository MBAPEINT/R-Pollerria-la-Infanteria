# ============================================================
# main.R
# Proceso ETL completo: Excel -> Transformación -> MongoDB
# ============================================================
library(dplyr)
library(readxl)
library(mongolite)

# Importamos tus funciones del archivo limpieza_datos.R
source("src/etl/limpieza_datos.R")
source("src/etl/derivar_hojas.R")

# Helper: convierte fechas a POSIXct UTC para que mongolite las serialice
# como ISODate real en MongoDB (as.Date() las mandaría como string)
a_mongo_fecha <- function(x) {
  as.POSIXct(as.character(x), tz = "UTC")
}

# Definimos la ruta del Excel y los datos de conexión a MongoDB
ruta_excel <- "data/raw/data_2023.xlsx"
mongo_url <- Sys.getenv("MONGO_URL", "mongodb://localhost:27017")
nombre_bd <- "Polleria"

# Detectar qué hojas existen en el Excel (para decidir si leer o derivar)
hojas_existentes <- excel_sheets(ruta_excel)

print("--- INICIANDO PROCESO ETL COMPLETO ---")

# ============================================================
# 2. PROCESAR HOJA: CLIENTES
# ============================================================
print("Procesando Clientes...")
df_clientes <- read_excel(ruta_excel, sheet = "Clientes")

# (Al ser datos de texto como nombres y correos, los subimos directamente)
db_clientes <- mongo(collection = "Clientes", db = nombre_bd, url = mongo_url)
db_clientes$drop() # Limpia la colección por si ya existía
db_clientes$insert(df_clientes)


# ============================================================
# 3. PROCESAR HOJA: PRODUCTO
# ============================================================
print("Procesando Productos...")
df_producto <- read_excel(ruta_excel, sheet = "Productos")

# CORRECCIÓN: NombreProducto es único por fila (1 producto = 1 grupo de
# tamaño 1), así que el IQR nunca detectaría nada agrupando por ahí.
# Derivamos una Categoria para tener varios productos por grupo y que
# la comparación estadística tenga sentido.
df_producto <- df_producto %>%
    mutate(Categoria = case_when(
        grepl("pollo|parrilla|chuleta|churrasco|chorizo", tolower(NombreProducto)) ~ "PLATOS_PRINCIPALES",
        grepl("papas|ensalada", tolower(NombreProducto)) ~ "ACOMPAÑAMIENTOS",
        grepl("frozen|limonada|chicha|jugo|inca kola|gaseosa", tolower(NombreProducto)) ~ "BEBIDAS",
        TRUE ~ "OTROS"
    ))

df_producto_transformado <- df_producto %>%
    tratar_outliers_IQR(columna = "Precio", columnas_grupo = "Categoria", minimo_valido = 0.1)

db_producto <- mongo(collection = "Productos", db = nombre_bd, url = mongo_url)
db_producto$drop()
db_producto$insert(df_producto_transformado)


# ============================================================
# 4. PROCESAR HOJA: VENTAS
# ============================================================
print("Procesando Ventas...")
df_ventas <- read_excel(ruta_excel, sheet = "Ventas")

# Convertir FechaVenta a POSIXct UTC para que Mongo la almacene como ISODate
df_ventas$FechaVenta <- a_mongo_fecha(df_ventas$FechaVenta)

# Nota: cada función lee siempre la columna original 'Total', no se
# encadenan entre sí (tratar_outliers_IQR no usa Total_imputada, etc.).
# Es el comportamiento esperado del diseño: cada técnica genera su
# propia columna de auditoría de forma independiente.
df_ventas_transformado <- df_ventas %>%
    tratar_valores_nulos(columna = "Total", columnas_grupo = "MetodoPago") %>%
    tratar_outliers_IQR(columna = "Total", columnas_grupo = "MetodoPago") %>%
    estandarizar_variable(columna = "Total", columnas_grupo = "MetodoPago")

db_ventas <- mongo(collection = "Ventas", db = nombre_bd, url = mongo_url)
db_ventas$drop()
db_ventas$insert(df_ventas_transformado)


# ============================================================
# 5. PROCESAR HOJA: DETALLE_VENTA
# ============================================================
print("Procesando Detalles de Venta...")
if ("Detalles_Venta" %in% hojas_existentes) {
    df_detalle <- read_excel(ruta_excel, sheet = "Detalles_Venta")
    grupo_detalle <- "IdProducto"
} else {
    # Derivar desde Ventas (formato ancho con Producto1..7)
    df_detalle <- unpivot_ventas_detalle(df_ventas)
    grupo_detalle <- "Producto"
}

# CORRECCIÓN: Cantidad son unidades (no debe quedar con decimales) y
# nadie vende 0 o menos unidades, así que agregamos es_entero y
# minimo_valido para que la regla de negocio tenga efecto real.
df_detalle_transformado <- df_detalle %>%
    tratar_outliers_IQR(
        columna = "Cantidad", columnas_grupo = grupo_detalle,
        minimo_valido = 1, es_entero = TRUE
    )

# Si además quieres tratar nulos en Cantidad/PrecioUnitario (como en el
# diseño original de la tabla), descomenta estas dos líneas:
# df_detalle_transformado <- df_detalle_transformado %>%
#     tratar_valores_nulos(columna = "Cantidad", columnas_grupo = "IdProducto", es_entero = TRUE) %>%
#     tratar_valores_nulos(columna = "PrecioUnitario", columnas_grupo = "IdProducto")

db_detalle <- mongo(collection = "Detalles_Venta", db = nombre_bd, url = mongo_url)
db_detalle$drop()
db_detalle$insert(df_detalle_transformado)


# ============================================================
# 6. PROCESAR HOJA: Personal
# ============================================================
print("Procesando Personal...")
df_personal <- read_excel(ruta_excel, sheet = "Personal")

# Convertir FechaIngreso a POSIXct UTC para Mongo (ISODate real)
df_personal$FechaIngreso <- a_mongo_fecha(df_personal$FechaIngreso)

db_personal <- mongo(collection = "Personal", db = nombre_bd, url = mongo_url)
db_personal$drop()
db_personal$insert(df_personal)


# ============================================================
# 7. PROCESAR HOJA: Insumos
# ============================================================
print("Procesando Insumos...")
df_insumos <- read_excel(ruta_excel, sheet = "Insumos")

df_insumos_transformado <- df_insumos %>%
    tratar_valores_nulos(columna = "StockActual", columnas_grupo = "UnidadMedida") %>%
    tratar_outliers_IQR(columna = "StockActual", columnas_grupo = "UnidadMedida",
                        minimo_valido = 0, es_entero = TRUE) %>%
    tratar_outliers_IQR(columna = "StockMinimo", columnas_grupo = "UnidadMedida",
                        minimo_valido = 0) %>%
    tratar_outliers_IQR(columna = "StockMaximo", columnas_grupo = "UnidadMedida",
                        minimo_valido = 0)

db_insumos <- mongo(collection = "Insumos", db = nombre_bd, url = mongo_url)
db_insumos$drop()
db_insumos$insert(df_insumos_transformado)


# ============================================================
# 8. PROCESAR HOJA: Detalle_Insumo
# ============================================================
print("Procesando Detalle_Insumo...")
if ("Detalle_Insumo" %in% hojas_existentes) {
    df_detalle_insumo <- read_excel(ruta_excel, sheet = "Detalle_Insumo")
} else {
    # Derivar desde Productos (formato ancho con Insumo1..7)
    df_detalle_insumo <- unpivot_productos_insumos(df_producto)
}

df_detalle_insumo_transformado <- df_detalle_insumo %>%
    tratar_outliers_IQR(columna = "Cantidad", columnas_grupo = "IdProducto",
                        minimo_valido = 0.001)

db_detalle_insumo <- mongo(collection = "Detalle_Insumo", db = nombre_bd, url = mongo_url)
db_detalle_insumo$drop()
db_detalle_insumo$insert(df_detalle_insumo_transformado)


# ============================================================
# 9. PROCESAR HOJA: Proveedores
# ============================================================
print("Procesando Proveedores...")
df_proveedores <- read_excel(ruta_excel, sheet = "Proveedores")

db_proveedores <- mongo(collection = "Proveedores", db = nombre_bd, url = mongo_url)
db_proveedores$drop()
db_proveedores$insert(df_proveedores)


# ============================================================
# 10. PROCESAR HOJA: Pagos
# ============================================================
print("Procesando Pagos...")
df_pagos <- read_excel(ruta_excel, sheet = "Pagos")

# Limpiar columnas monetarias: vienen como character con prefijo "s/."
# y algunos valores corruptos (ej: "s/.52.47" → NA)
df_pagos$MontoRecibido <- as.numeric(gsub("^s/\\.", "", df_pagos$MontoRecibido))
df_pagos$MontoPagado   <- as.numeric(gsub("^s/\\.", "", df_pagos$MontoPagado))

df_pagos_transformado <- df_pagos %>%
    tratar_valores_nulos(columna = "MontoRecibido", columnas_grupo = "MetodoPago") %>%
    tratar_outliers_IQR(columna = "MontoRecibido", columnas_grupo = "MetodoPago",
                        minimo_valido = 0) %>%
    tratar_valores_nulos(columna = "MontoPagado", columnas_grupo = "MetodoPago") %>%
    tratar_outliers_IQR(columna = "MontoPagado", columnas_grupo = "MetodoPago",
                        minimo_valido = 0)

db_pagos <- mongo(collection = "Pagos", db = nombre_bd, url = mongo_url)
db_pagos$drop()
db_pagos$insert(df_pagos_transformado)


# ============================================================
# 11. PROCESAR HOJA: Metodos_Pago
# ============================================================
print("Procesando Metodos_Pago...")
df_metodos_pago <- read_excel(ruta_excel, sheet = "Metodos_Pago")

db_metodos_pago <- mongo(collection = "Metodos_Pago", db = nombre_bd, url = mongo_url)
db_metodos_pago$drop()
db_metodos_pago$insert(df_metodos_pago)


# ============================================================
# 12. PROCESAR HOJA: Unidades_Medida
# ============================================================
print("Procesando Unidades_Medida...")
if ("Unidades_Medida" %in% hojas_existentes) {
    df_unidades_medida <- read_excel(ruta_excel, sheet = "Unidades_Medida")
} else {
    # Extraer valores únicos desde Insumos
    df_unidades_medida <- extraer_unidades_medida(df_insumos)
}

db_unidades_medida <- mongo(collection = "Unidades_Medida", db = nombre_bd, url = mongo_url)
db_unidades_medida$drop()
db_unidades_medida$insert(df_unidades_medida)


# ============================================================
# 13. PROCESAR HOJA: Movimientos_Inventario
# ============================================================
print("Procesando Movimientos_Inventario...")
df_movimientos_inv <- read_excel(ruta_excel, sheet = "Movimientos_Inventario")

# Convertir FechaMovimiento a POSIXct UTC para Mongo (ISODate real)
df_movimientos_inv$FechaMovimiento <- a_mongo_fecha(df_movimientos_inv$FechaMovimiento)

df_movimientos_inv_transformado <- df_movimientos_inv %>%
    tratar_outliers_IQR(columna = "Cantidad", columnas_grupo = "Insumo",
                        minimo_valido = 0.001)

db_movimientos_inv <- mongo(collection = "Movimientos_Inventario", db = nombre_bd, url = mongo_url)
db_movimientos_inv$drop()
db_movimientos_inv$insert(df_movimientos_inv_transformado)


# ============================================================
# 14. PROCESAR HOJA: Ordenes_Compra
# ============================================================
print("Procesando Ordenes_Compra...")
df_ordenes_compra <- read_excel(ruta_excel, sheet = "Ordenes_Compra")

# Convertir fechas a POSIXct UTC para Mongo (ISODate real)
df_ordenes_compra$FechaPedido  <- a_mongo_fecha(df_ordenes_compra$FechaPedido)
df_ordenes_compra$FechaEntrega <- a_mongo_fecha(df_ordenes_compra$FechaEntrega)

# Derivar CantidadItems si no existe (formato crudo: Insumo1..5)
if (!"CantidadItems" %in% colnames(df_ordenes_compra)) {
    columnas_insumo_nombre <- grep("^Insumo\\d+_Nombre$", colnames(df_ordenes_compra), value = TRUE)
    df_ordenes_compra$CantidadItems <- apply(
        df_ordenes_compra[, columnas_insumo_nombre, drop = FALSE], 1,
        function(fila) sum(!is.na(fila) & fila != "")
    )
}

df_ordenes_compra_transformado <- df_ordenes_compra %>%
    tratar_valores_nulos(columna = "CostoTotal", columnas_grupo = "Estado") %>%
    tratar_outliers_IQR(columna = "CostoTotal", columnas_grupo = "Estado",
                        minimo_valido = 0) %>%
    tratar_outliers_IQR(columna = "CantidadItems", columnas_grupo = "Estado",
                        minimo_valido = 1, es_entero = TRUE)

db_ordenes_compra <- mongo(collection = "Ordenes_Compra", db = nombre_bd, url = mongo_url)
db_ordenes_compra$drop()
db_ordenes_compra$insert(df_ordenes_compra_transformado)


# ============================================================
# 15. PROCESAR HOJA: Detalle_Orden_Compra
# ============================================================
print("Procesando Detalle_Orden_Compra...")
if ("Detalle_Orden_Compra" %in% hojas_existentes) {
    df_detalle_oc <- read_excel(ruta_excel, sheet = "Detalle_Orden_Compra")
} else {
    # Derivar desde Ordenes_Compra (formato ancho con Insumo1..5)
    df_detalle_oc <- unpivot_ordenes_compra_detalle(df_ordenes_compra)
}

df_detalle_oc_transformado <- df_detalle_oc %>%
    tratar_outliers_IQR(columna = "Cantidad", columnas_grupo = "IdOrdenCompra",
                        minimo_valido = 0.001) %>%
    tratar_valores_nulos(columna = "PrecioUnitario", columnas_grupo = "IdOrdenCompra") %>%
    tratar_outliers_IQR(columna = "PrecioUnitario", columnas_grupo = "IdOrdenCompra",
                        minimo_valido = 0) %>%
    tratar_outliers_IQR(columna = "Subtotal", columnas_grupo = "IdOrdenCompra",
                        minimo_valido = 0)

db_detalle_oc <- mongo(collection = "Detalle_Orden_Compra", db = nombre_bd, url = mongo_url)
db_detalle_oc$drop()
db_detalle_oc$insert(df_detalle_oc_transformado)


print("--- ¡PROCESO ETL FINALIZADO CON ÉXITO! ---")
print("Las 14 colecciones han sido cargadas en la base de datos 'Polleria'.")
