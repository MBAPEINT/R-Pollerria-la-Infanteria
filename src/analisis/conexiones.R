# ============================================================
# conexiones.R
# Crea las conexiones de LECTURA a las 14 colecciones de MongoDB
# que usa consultas.R. Ejecutar este script ANTES de consultas.R.
#
# Requisito previo: haber corrido main.R al menos una vez para
# que las colecciones ya existan y tengan datos en Mongo.
# ============================================================
library(mongolite)

mongo_url <- Sys.getenv("MONGO_URL", "mongodb://localhost:27017")
nombre_bd <- "Polleria"

# --- Colecciones originales (4) ---
con_clientes  <- mongo(collection = "Clientes",        db = nombre_bd, url = mongo_url)
con_productos <- mongo(collection = "Productos",       db = nombre_bd, url = mongo_url)
con_ventas    <- mongo(collection = "Ventas",          db = nombre_bd, url = mongo_url)
con_detalle   <- mongo(collection = "Detalles_Venta",  db = nombre_bd, url = mongo_url)

# --- Colecciones nuevas (10) ---
con_personal              <- mongo(collection = "Personal",              db = nombre_bd, url = mongo_url)
con_insumos               <- mongo(collection = "Insumos",               db = nombre_bd, url = mongo_url)
con_detalle_insumo        <- mongo(collection = "Detalle_Insumo",        db = nombre_bd, url = mongo_url)
con_proveedores           <- mongo(collection = "Proveedores",           db = nombre_bd, url = mongo_url)
con_pagos                 <- mongo(collection = "Pagos",                 db = nombre_bd, url = mongo_url)
con_metodos_pago          <- mongo(collection = "Metodos_Pago",          db = nombre_bd, url = mongo_url)
con_unidades_medida       <- mongo(collection = "Unidades_Medida",       db = nombre_bd, url = mongo_url)
con_movimientos_inv       <- mongo(collection = "Movimientos_Inventario", db = nombre_bd, url = mongo_url)
con_ordenes_compra        <- mongo(collection = "Ordenes_Compra",        db = nombre_bd, url = mongo_url)
con_detalle_orden_compra  <- mongo(collection = "Detalle_Orden_Compra",  db = nombre_bd, url = mongo_url)

print("--- Conexiones a MongoDB listas (14 colecciones) ---")
print("Originales: con_clientes, con_productos, con_ventas, con_detalle")
print("Nuevas: con_personal, con_insumos, con_detalle_insumo, con_proveedores, con_pagos, con_metodos_pago, con_unidades_medida, con_movimientos_inv, con_ordenes_compra, con_detalle_orden_compra")
