# ============================================================
# casting.R — PIPELINE STEP 1
# Corrige tipos de dato:
#   fechas string → POSIXct UTC
#   montos "s/.XX" → numeric
#   logical 100% NA → tipo adecuado
#   celdas numéricas corruptas ('989yt','8o8','gf','787.e5e') → NA numeric
# ============================================================
library(readxl)
library(dplyr)
library(openxlsx)

INPUT  <- "data/raw/data_2023.xlsx"
OUTPUT <- "data/processed/datos_casting.xlsx"

cat("============================================================\n")
cat("  STEP 1: CASTING — Corrección de tipos de dato\n")
cat("============================================================\n\n")

es_fecha_iso <- function(x) {
  x_ok <- x[!is.na(x)]
  if (length(x_ok) == 0) return(FALSE)
  mean(grepl("^\\d{4}-\\d{2}-\\d{2}(T\\d{2}:\\d{2}:\\d{2}(\\.\\d{3})?Z?)?$", x_ok)) > 0.9
}

tiene_prefijo_moneda <- function(x) {
  x_ok <- x[!is.na(x)]
  if (length(x_ok) == 0) return(FALSE)
  sum(grepl("^\\s*[sS]/\\.?", x_ok)) > 0
}

limpiar_moneda <- function(x) {
  xc <- gsub("^\\s*[sS]/\\.?\\s*", "", x)
  suppressWarnings(as.numeric(xc))
}

# Limpia celdas numéricas corruptas (ej: '989yt'→989, '8o8'→808, 'gf'→NA)
limpiar_num_corrupto <- function(x) {
  if (!is.character(x)) return(x)
  # Quitar caracteres no numéricos excepto '.' y '-'
  xc <- gsub("[^0-9.\\-]", "", x)
  xc[xc == ""] <- NA_character_
  # Si tiene múltiples puntos, solo conserva el primero
  xc <- gsub("^([0-9]*\\.?[0-9]*).*", "\\1", xc)
  as.numeric(xc)
}

# Cargar hojas
hojas <- excel_sheets(INPUT)
datos <- list()
for (h in hojas) {
  datos[[h]] <- read_excel(INPUT, sheet = h)
}

reporte <- data.frame(
  Hoja = character(), Columna = character(),
  Tipo_Original = character(), Tipo_Nuevo = character(),
  Ejemplo_Antes = character(), Ejemplo_Despues = character(),
  Filas_Afectadas = integer(),
  stringsAsFactors = FALSE
)

# ============================================================
# 1. Fechas: string → POSIXct UTC
# ============================================================
columnas_fecha <- list(
  "Personal"               = "FechaIngreso",
  "Ventas"                 = "FechaVenta",
  "Movimientos_Inventario" = "FechaMovimiento",
  "Ordenes_Compra"         = c("FechaPedido", "FechaEntrega")
)

for (h in names(columnas_fecha)) {
  for (col in columnas_fecha[[h]]) {
    if (col %in% names(datos[[h]])) {
      vals <- datos[[h]][[col]]
      antes <- head(vals[!is.na(vals)], 2)
      datos[[h]][[col]] <- as.POSIXct(as.character(vals), tz = "UTC")
      despues <- as.character(head(datos[[h]][[col]][!is.na(datos[[h]][[col]])], 2))
      n_ok <- sum(!is.na(vals))
      n_na <- sum(is.na(vals))

      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col,
        Tipo_Original = "character",
        Tipo_Nuevo = "POSIXct UTC",
        Ejemplo_Antes = paste(antes, collapse = " | "),
        Ejemplo_Despues = paste(despues, collapse = " | "),
        Filas_Afectadas = n_ok,
        stringsAsFactors = FALSE
      ))
      msg <- if(n_na > 0) sprintf("(%d NAs→NA)", n_na) else ""
      cat(sprintf("  [DATE]  %-22s %-28s  texto → fecha   %d filas %s\n",
                  h, col, n_ok, msg))
    }
  }
}

# ============================================================
# 2. Montos "s/.XX": string → numeric
# ============================================================
for (h in names(datos)) {
  for (col in names(datos[[h]])) {
    vals <- datos[[h]][[col]]
    if (is.character(vals) && tiene_prefijo_moneda(vals)) {
      idx <- grep("^\\s*[sS]/\\.?", vals)
      antes <- head(vals[idx], 3)
      datos[[h]][[col]] <- limpiar_moneda(vals)
      despues <- head(datos[[h]][[col]][idx], 3)
      malos <- vals[grepl("\\.[0-9]+\\.[0-9]+", vals)]
      if (length(malos) > 0) {
        cat(sprintf("  ⚠ %d valores con doble punto en %s/%s\n",
                    length(malos), h, col))
      }
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col,
        Tipo_Original = "character (S/.)",
        Tipo_Nuevo = "numeric",
        Ejemplo_Antes = paste(antes, collapse = " | "),
        Ejemplo_Despues = paste(despues, collapse = " | "),
        Filas_Afectadas = length(idx),
        stringsAsFactors = FALSE
      ))
      cat(sprintf("  [S/.]   %-22s %-28s  S/. → num     %d valores\n",
                  h, col, length(idx)))
    }
  }
}

# ============================================================
# 3. Celdas numéricas corruptas: detectar y limpiar
#    (Ordenes_Compra: '989yt','8o8','787.e5e','gf','qw','6y','e56e')
# ============================================================
for (h in names(datos)) {
  df <- datos[[h]]
  for (col in names(df)) {
    vals <- df[[col]]
    if (!is.character(vals)) next

    # Detectar: columna es character pero debería ser numérica
    # (tiene números + basura mezclados)
    vals_ok <- vals[!is.na(vals)]
    if (length(vals_ok) == 0) next

    # Contar cuántos son numéricos puros
    n_numericos <- sum(grepl("^-?\\d+\\.?\\d*$", vals_ok))
    n_texto <- sum(!grepl("^-?\\d+\\.?\\d*$", vals_ok) & vals_ok != "")

    # Si hay texto mezclado con números en una columna que claramente debería ser numérica
    if (n_texto > 0 && n_numericos > n_texto &&
        grepl("(Cantidad|Precio|Subtotal|Costo|Stock|Monto|Total)", col, ignore.case = TRUE)) {

      antes <- vals_ok[!grepl("^-?\\d+\\.?\\d*$", vals_ok)]
      df[[col]] <- limpiar_num_corrupto(vals)
      nuevos_nas <- sum(is.na(df[[col]])) - sum(is.na(vals))

      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col,
        Tipo_Original = "character (corrupto)",
        Tipo_Nuevo = "numeric (limpiado)",
        Ejemplo_Antes = paste(head(antes, 5), collapse = " | "),
        Ejemplo_Despues = paste(head(df[[col]][match(head(antes, 5), vals)], 5), collapse = " | "),
        Filas_Afectadas = n_texto,
        stringsAsFactors = FALSE
      ))
      cat(sprintf("  [FIX]   %-22s %-28s  %d valores corruptos → num (%d → NA)\n",
                  h, col, n_texto, nuevos_nas))
    }
  }
  datos[[h]] <- df
}

# ============================================================
# 4. Logical 100% NA → tipo adecuado
# ============================================================
for (h in hojas) {
  df <- datos[[h]]
  for (col in names(df)) {
    if (is.logical(df[[col]]) && all(is.na(df[[col]]))) {
      if (grepl("Nombre|Tipo|Estado|Comprobante|Metodo|Proveedor|Personal|Cliente", col, ignore.case = TRUE)) {
        datos[[h]][[col]] <- as.character(df[[col]])
        dest <- "character"
      } else {
        datos[[h]][[col]] <- as.numeric(df[[col]])
        dest <- "numeric"
      }
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col,
        Tipo_Original = "logical (100% NA)",
        Tipo_Nuevo = dest,
        Ejemplo_Antes = "(todo NA logical)",
        Ejemplo_Despues = paste("(todo NA", dest, ")"),
        Filas_Afectadas = nrow(df),
        stringsAsFactors = FALSE
      ))
      cat(sprintf("  [LOGIC] %-22s %-28s  logical NA → %s\n", h, col, dest))
    }
  }
}

# ============================================================
# Guardar
# ============================================================
wb <- createWorkbook()
addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", reporte)
hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#2ECC71",
                   textDecoration = "bold", halign = "center")
addStyle(wb, "Resumen", hdr, rows = 1, cols = 1:ncol(reporte), gridExpand = TRUE)
setColWidths(wb, "Resumen", cols = 1:7, widths = c(22, 28, 26, 22, 55, 55, 14))
for (h in hojas) {
  addWorksheet(wb, h)
  writeData(wb, h, datos[[h]])
  addStyle(wb, h, hdr, rows = 1, cols = 1:ncol(datos[[h]]), gridExpand = TRUE)
}
saveWorkbook(wb, OUTPUT, overwrite = TRUE)

cat("\n============================================================\n")
cat(sprintf("  CASTING COMPLETO → %s (%d conversiones)\n", OUTPUT, nrow(reporte)))
cat("============================================================\n")
