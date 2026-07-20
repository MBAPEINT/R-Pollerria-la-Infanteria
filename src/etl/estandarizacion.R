# ============================================================
# estandarizacion.R — PIPELINE STEP 4 (FINAL)
# Z-Score: (x - μ) / σ  → todas las numéricas en escala unificada
# Incluye diccionario de reversión en hoja "Como_Revertir"
# ============================================================
library(readxl)
library(dplyr)
library(openxlsx)

INPUT  <- "data/processed/datos_outliers.xlsx"
OUTPUT <- "data/processed/datos_ml_normalizado.xlsx"

NO_ESTANDARIZAR <- c(
  "DNI", "Telefono", "RUC", "Correo",
  "IdCliente", "IdProducto", "IdVenta", "IdPersonal",
  "IdMetodoPago", "IdInsumo", "IdProveedor", "IdOrdenCompra",
  "CantidadItems"
)

hojas <- setdiff(excel_sheets(INPUT), "Resumen")
datos <- list()
for (h in hojas) datos[[h]] <- read_excel(INPUT, sheet = h)

reporte <- data.frame(
  Hoja = character(), Columna = character(),
  Media_Orig = numeric(), SD_Orig = numeric(),
  Min_Orig = numeric(), Max_Orig = numeric(),
  Min_Z = numeric(), Max_Z = numeric(),
  stringsAsFactors = FALSE
)

cat("============================================================\n")
cat("  STEP 4: ESTANDARIZACIÓN — Z-Score (μ=0, σ=1)\n")
cat("============================================================\n\n")

for (h in hojas) {
  cat("Procesando:", h, "...\n")
  df <- datos[[h]]
  cols_num <- names(df)[sapply(df, is.numeric) & !names(df) %in% NO_ESTANDARIZAR]
  if (length(cols_num) == 0) { cat("  → Sin numéricas\n"); next }

  for (col in cols_num) {
    vals <- df[[col]]; vals_ok <- vals[!is.na(vals)]
    if (length(vals_ok) < 5) next
    mu <- mean(vals_ok); sigma <- sd(vals_ok)
    if (sigma == 0 || is.na(sigma)) {
      df[[col]] <- vals - mu
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col, Media_Orig = round(mu, 4), SD_Orig = 0,
        Min_Orig = round(min(vals_ok), 4), Max_Orig = round(max(vals_ok), 4),
        Min_Z = 0, Max_Z = 0, stringsAsFactors = FALSE))
      cat(sprintf("  [CENTRAR] %-30s μ=%.2f  σ=0\n", col, mu))
    } else {
      z <- (vals - mu) / sigma; df[[col]] <- z
      z_ok <- z[!is.na(z)]
      reporte <- rbind(reporte, data.frame(
        Hoja = h, Columna = col, Media_Orig = round(mu, 4), SD_Orig = round(sigma, 4),
        Min_Orig = round(min(vals_ok), 4), Max_Orig = round(max(vals_ok), 4),
        Min_Z = round(min(z_ok), 3), Max_Z = round(max(z_ok), 3),
        stringsAsFactors = FALSE))
      cat(sprintf("  [Z] %-30s μ=%-8.2f σ=%-7.2f → z ∈ [%.2f, %.2f]\n",
                  col, mu, sigma, min(z_ok), max(z_ok)))
    }
  }
  datos[[h]] <- df
}

wb <- createWorkbook()
addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", reporte)
hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#9B59B6",
                   textDecoration = "bold", halign = "center")
addStyle(wb, "Resumen", hdr, rows = 1, cols = 1:8, gridExpand = TRUE)

# Diccionario de reversión
addWorksheet(wb, "Como_Revertir")
dict <- reporte[, c("Hoja", "Columna", "Media_Orig", "SD_Orig")]
dict$Formula <- paste0("(valor_z × ", round(reporte$SD_Orig, 4), ") + ", round(reporte$Media_Orig, 4))
dict$Formula[reporte$SD_Orig == 0] <- paste0("valor_centrado + ", round(reporte$Media_Orig[reporte$SD_Orig == 0], 4))
writeData(wb, "Como_Revertir", dict)
addStyle(wb, "Como_Revertir", hdr, rows = 1, cols = 1:5, gridExpand = TRUE)
setColWidths(wb, "Como_Revertir", cols = 1:5, widths = c(22, 28, 12, 12, 55))

setColWidths(wb, "Resumen", cols = 1:8, widths = c(22, 28, 12, 12, 12, 12, 10, 10))
for (h in hojas) {
  addWorksheet(wb, h)
  writeData(wb, h, datos[[h]])
  addStyle(wb, h, hdr, rows = 1, cols = 1:ncol(datos[[h]]), gridExpand = TRUE)
}
saveWorkbook(wb, OUTPUT, overwrite = TRUE)

cat(sprintf("\nESTANDARIZACIÓN COMPLETA → %s  (%d columnas en escala Z)\n",
            OUTPUT, nrow(reporte)))
cat("  ⚠ Este es PREPROCESAMIENTO OPCIONAL para machine learning.\n")
cat("  Los datos limpios en escala original están en: datos_outliers.xlsx\n")
cat("  Hoja 'Como_Revertir' contiene fórmulas para deshacer el z-score\n")
