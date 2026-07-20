# ============================================================
# nulos.R — PIPELINE STEP 2
# Imputa SOLO nulos REALES (no estructurales).
#
# REGLA: Si una columna pertenece a un grupo repetitivo
#   Producto[2-7]_*  → "no hay N-ésimo producto" (estructural)
#   Insumo[2-7]_*    → "no hay N-ésimo insumo"   (estructural)
#   ...4 ...7        → columnas fantasma         (estructural)
#
# Estos NUNCA se imputan. Solo se imputan nulos reales:
#   - Nombres faltantes (SegundoNombre, ApellidoMaterno)
#   - Pocos numéricos huérfanos (ej: 8 NAs en Producto1_PrecioUnitario)
#   - Insumo1_* (el primer insumo DEBE existir si el producto existe)
# ============================================================
library(readxl)
library(dplyr)
library(openxlsx)

INPUT  <- "data/processed/datos_casting.xlsx"
OUTPUT <- "data/processed/datos_nulos.xlsx"
UMBRAL <- 80  # safety net para columnas no-estructurales

GRUPOS <- list(
  "Productos"              = "Categoria",
  "Insumos"                = "UnidadMedida",
  "Ventas"                 = "TipoComprobante",
  "Movimientos_Inventario" = "Tipo",
  "Ordenes_Compra"         = "Estado",
  "Pagos"                  = "MetodoPago"
)

# --- Detecta si una columna es estructural por su nombre ---
es_estructural <- function(nombre_col) {
  # Slots de producto/insumo en posición 2+: no hay N-ésimo elemento
  if (grepl("^(Producto|Insumo)[2-9]_", nombre_col)) return(TRUE)
  # Columnas fantasma (Pagos ...4 a ...7)
  if (grepl("^\\.\\.\\.[4-9]$", nombre_col)) return(TRUE)
  return(FALSE)
}

hojas <- setdiff(excel_sheets(INPUT), "Resumen")
datos <- list()
for (h in hojas) datos[[h]] <- read_excel(INPUT, sheet = h)

reporte <- data.frame(
  Hoja = character(), Columna = character(), Tipo = character(),
  Nulos_Antes = integer(), Nulos_Despues = integer(),
  Metodo = character(), stringsAsFactors = FALSE
)

cat("============================================================\n")
cat("  STEP 2: NULOS — Imputación SOLO de faltantes reales\n")
cat("  Columnas Producto[2-7]/Insumo[2-7] → estructurales (no se tocan)\n")
cat("============================================================\n\n")

for (h in hojas) {
  cat("Procesando:", h, "...\n")
  df <- datos[[h]]
  gcol <- GRUPOS[[h]]
  nf <- nrow(df)

  for (col in names(df)) {
    nna <- sum(is.na(df[[col]]))
    if (nna == 0) next

    pct <- round(100 * nna / nf, 1)
    tc <- class(df[[col]])[1]

    # --- Regla 1: Estructural por NOMBRE (slots opcionales) ---
    if (es_estructural(col)) {
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col, Tipo = tc,
        Nulos_Antes = nna, Nulos_Despues = nna,
        Metodo = paste0("ESTRUCTURAL (slot opcional, ", pct, "%)"),
        stringsAsFactors = FALSE))
      cat(sprintf("  [ESTRUCT]  %-30s %5d NAs (%5.1f%%) → slot opcional, se conserva\n",
                  col, nna, pct))
      next
    }

    # --- Regla 2: Estructural por UMBRAL (>80% vacío, safety net) ---
    if (pct > UMBRAL) {
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col, Tipo = tc,
        Nulos_Antes = nna, Nulos_Despues = nna,
        Metodo = paste0("ESTRUCTURAL (umbral ", pct, "%)"), stringsAsFactors = FALSE))
      cat(sprintf("  [SKIP]     %-30s %5d NAs (%5.1f%%) → >80%%, estructural\n", col, nna, pct))
      next
    }

    # --- Regla 3: Fechas no se imputan ---
    if (tc == "Date" || inherits(df[[col]], "POSIXct")) {
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col, Tipo = "Date/POSIXct",
        Nulos_Antes = nna, Nulos_Despues = nna,
        Metodo = "SIN CAMBIO (fecha, no imputable)", stringsAsFactors = FALSE))
      cat(sprintf("  [SKIP]     %-30s %5d NAs → fecha, no imputable\n", col, nna))
      next
    }

    # --- IMPUTAR: solo nulos REALES ---
    if (tc %in% c("numeric", "integer")) {
      if (!is.null(gcol) && gcol %in% names(df)) {
        med_g <- df %>% filter(!is.na(.data[[col]])) %>%
          group_by(across(all_of(gcol))) %>%
          summarise(m = median(.data[[col]], na.rm = TRUE), .groups = "drop")
        for (i in 1:nrow(med_g)) {
          filas <- which(is.na(df[[col]]) & df[[gcol]] == med_g[[gcol]][i])
          df[filas, col] <- med_g$m[i]
        }
        metodo <- paste0("mediana por ", gcol)
      } else {
        df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
        metodo <- "mediana global"
      }
    } else if (tc == "character") {
      df[[col]][is.na(df[[col]])] <- "NO_ESPECIFICADO"
      metodo <- "'NO_ESPECIFICADO'"
    } else {
      metodo <- "sin acción"
    }

    nd <- sum(is.na(df[[col]]))
    reporte <- rbind(reporte, data.frame(
      Hoja = h, Columna = col, Tipo = tc,
      Nulos_Antes = nna, Nulos_Despues = nd,
      Metodo = metodo, stringsAsFactors = FALSE))
    cat(sprintf("  [IMPUTAR]  %-30s %5d → %-5d NAs  (%s)\n", col, nna, nd, metodo))
  }
  datos[[h]] <- df
}

# --- Guardar ---
wb <- createWorkbook()
addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", reporte)
hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#E74C3C",
                   textDecoration = "bold", halign = "center")
addStyle(wb, "Resumen", hdr, rows = 1, cols = 1:6, gridExpand = TRUE)
setColWidths(wb, "Resumen", cols = 1:6, widths = c(22, 30, 14, 12, 14, 55))
for (h in hojas) {
  addWorksheet(wb, h)
  writeData(wb, h, datos[[h]])
  addStyle(wb, h, hdr, rows = 1, cols = 1:ncol(datos[[h]]), gridExpand = TRUE)
}
saveWorkbook(wb, OUTPUT, overwrite = TRUE)

cat(sprintf("\nNULOS COMPLETO → %s\n", OUTPUT))
cat(sprintf("  Imputaciones reales: %d\n", sum(reporte$Nulos_Antes != reporte$Nulos_Despues)))
cat(sprintf("  Estructurales (slots opcionales): %d\n", sum(grepl("ESTRUCTURAL", reporte$Metodo))))
cat(sprintf("  Otros sin cambio: %d\n", sum(reporte$Nulos_Antes == reporte$Nulos_Despues & !grepl("ESTRUCTURAL", reporte$Metodo))))
