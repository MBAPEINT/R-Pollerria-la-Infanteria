# ============================================================
# PREDICCIÓN 4: Productos que se compran juntos
# Modelo: Apriori (Market Basket Analysis)
# Pregunta: ¿Qué combos de productos se venden juntos?
# ============================================================
library(mongolite)
library(arules)
library(dplyr)

dir.create("outputs/predicciones", showWarnings = FALSE, recursive = TRUE)

# --- 1. Cargar detalles de venta desde MongoDB ---
con <- mongo(collection = "Detalles_Venta", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))
detalle <- con$find('{}', fields = '{"IdVenta":1, "Producto":1, "_id":0}')
cat("Líneas de detalle cargadas:", nrow(detalle), "\n")

# --- 2. Preparar datos para Apriori (formato "transacciones") ---
# Cada IdVenta es una "canasta", los Productos son los "items"
# Agrupar por venta y crear lista de productos por venta
transacciones <- detalle %>%
  filter(!is.na(Producto) & Producto != "") %>%
  group_by(IdVenta) %>%
  summarise(
    productos = list(Producto),
    n_productos = n(),
    .groups = "drop"
  ) %>%
  filter(n_productos >= 2)  # solo ventas con 2+ productos

cat("Ventas con 2+ productos:", nrow(transacciones), "\n")

# Convertir a formato "transactions" de arules
trans_list <- as(transacciones$productos, "transactions")
cat("Transacciones:", length(trans_list), "| Productos únicos:", length(itemLabels(trans_list)), "\n")

# --- 3. Aplicar Apriori ---
reglas <- apriori(
  trans_list,
  parameter = list(
    supp = 0.01,    # soporte mínimo: aparece en al menos 1% de ventas
    conf = 0.30,    # confianza mínima: 30% de las veces que se compra A, también B
    minlen = 2,     # mínimo 2 productos por regla
    maxlen = 3      # máximo 3 (combos de 2 o 3 productos)
  ),
  control = list(verbose = FALSE)
)

cat("\n========== REGLAS ENCONTRADAS:", length(reglas), "==========\n")

# Ordenar por lift (qué tan fuerte es la asociación)
reglas_ordenadas <- sort(reglas, by = "lift", decreasing = TRUE)

# --- 4. Mostrar resultados ---
cat("\nTOP 15 COMBOS DESCUBIERTOS:\n")
cat("(Soporte=% de ventas | Confianza=probabilidad | Lift=fuerza de asociación)\n\n")

df_reglas <- data.frame(
  Antecedente = labels(lhs(reglas_ordenadas)),
  Consecuente = labels(rhs(reglas_ordenadas)),
  Soporte      = round(quality(reglas_ordenadas)$support * 100, 2),
  Confianza    = round(quality(reglas_ordenadas)$confidence * 100, 1),
  Lift         = round(quality(reglas_ordenadas)$lift, 2)
)

for (i in 1:min(15, nrow(df_reglas))) {
  cat(sprintf("  %2d. %-30s → %-25s  sop:%5.2f%%  conf:%5.1f%%  lift:%6.2f\n",
    i,
    gsub("[{}]", "", df_reglas$Antecedente[i]),
    gsub("[{}]", "", df_reglas$Consecuente[i]),
    df_reglas$Soporte[i],
    df_reglas$Confianza[i],
    df_reglas$Lift[i]
  ))
}

# --- 5. Extraer combos accionables ---
cat("\n========== COMBOS RECOMENDADOS PARA EL NEGOCIO ==========\n")

# Solo reglas con lift > 1.5 (asociación real, no casualidad)
combos <- df_reglas %>%
  filter(Lift > 1.5, Soporte > 1) %>%
  head(10)

for (i in 1:nrow(combos)) {
  ant <- gsub("[{}]", "", combos$Antecedente[i])
  cons <- gsub("[{}]", "", combos$Consecuente[i])
  cat(sprintf("\n  COMBO #%d: %s + %s\n", i, ant, cons))
  cat(sprintf("  Aparece en el %.1f%% de las ventas (~%d ventas)\n",
    combos$Soporte[i], round(combos$Soporte[i] * length(trans_list) / 100)))
  cat(sprintf("  Cuando compran %s, el %.0f%% también compra %s\n",
    ant, combos$Confianza[i], cons))
  cat(sprintf("  Lift: %.1fx (se compran juntos %.1f veces más de lo esperado)\n",
    combos$Lift[i], combos$Lift[i]))
}

# --- 6. También: ¿qué productos son más "combinables"? ---
freq_items <- itemFrequency(trans_list, type = "absolute")
top_items <- sort(freq_items, decreasing = TRUE)

cat("\n========== PRODUCTOS MÁS VENDIDOS ==========\n")
for (i in 1:min(10, length(top_items))) {
  cat(sprintf("  %2d. %-30s %d ventas (%.1f%%)\n",
    i, names(top_items)[i], top_items[i],
    100 * top_items[i] / length(trans_list)))
}

# --- 7. Guardar ---
write.csv(df_reglas, "data/output/reglas_asociacion.csv", row.names = FALSE)

cat("\nArchivos guardados:\n")
cat("  - data/output/reglas_asociacion.csv (", nrow(df_reglas), "reglas)\n")
