# ============================================================
# outliers.R — PIPELINE STEP 3
# Winsorización IQR × 3.0: valores extremos → cap en límites
# Límite inferior acotado a ≥0 (precios, cantidades, stock no son negativos)
# ============================================================
library(readxl)
library(dplyr)
library(openxlsx)

INPUT  <- "data/processed/datos_nulos.xlsx"
OUTPUT <- "data/processed/datos_outliers.xlsx"
FACTOR <- 3.0

EXCLUIR <- c("DNI", "Telefono", "RUC", "Correo",
             "IdCliente", "IdProducto", "IdVenta", "IdPersonal",
             "IdMetodoPago", "IdInsumo", "IdProveedor", "IdOrdenCompra")

GRUPOS <- list(
  "Productos" = "Categoria", "Insumos" = "UnidadMedida",
  "Ventas" = "TipoComprobante", "Movimientos_Inventario" = "Tipo",
  "Ordenes_Compra" = "Estado", "Pagos" = "MetodoPago"
)

hojas <- setdiff(excel_sheets(INPUT), "Resumen")
datos <- list()
for (h in hojas) datos[[h]] <- read_excel(INPUT, sheet = h)

reporte <- data.frame(
  Hoja = character(), Columna = character(), Grupo = character(),
  Lim_Inferior = numeric(), Lim_Superior = numeric(),
  Valores_Cap = integer(), Pct_Afectado = numeric(),
  stringsAsFactors = FALSE
)

cat("============================================================\n")
cat("  STEP 3: OUTLIERS — Winsorización IQR ×", FACTOR, "\n")
cat("============================================================\n\n")

for (h in hojas) {
  cat("Procesando:", h, "...\n")
  df <- datos[[h]]
  gcol <- GRUPOS[[h]]
  nf <- nrow(df)

  cols_num <- names(df)[sapply(df, is.numeric) & !names(df) %in% EXCLUIR]
  if (length(cols_num) == 0) { cat("  → Sin numéricas\n"); next }

  for (col in cols_num) {
    vals <- df[[col]]
    vals_ok <- vals[!is.na(vals)]
    if (length(vals_ok) < 10) next

    if (!is.null(gcol) && gcol %in% names(df)) {
      for (g in unique(df[[gcol]])) {
        fg <- which(df[[gcol]] == g)
        vg <- vals[fg]; vg_ok <- vg[!is.na(vg)]
        if (length(vg_ok) < 10) next
        q1 <- quantile(vg_ok, 0.25); q3 <- quantile(vg_ok, 0.75)
        iqr <- q3 - q1; if (iqr == 0) next
        li <- q1 - FACTOR * iqr; li <- max(0, li)  # no-negativo
        ls <- q3 + FACTOR * iqr
        nb <- sum(vg < li, na.rm = TRUE); na <- sum(vg > ls, na.rm = TRUE)
        if (nb + na > 0) {
          df[fg[which(vg < li)], col] <- li
          df[fg[which(vg > ls)], col] <- ls
          pct <- round(100 * (nb + na) / length(fg), 2)
          reporte <- rbind(reporte, data.frame(
            Hoja = h, Columna = col, Grupo = paste0(gcol, "=", g),
            Lim_Inferior = round(li, 2), Lim_Superior = round(ls, 2),
            Valores_Cap = nb + na, Pct_Afectado = pct, stringsAsFactors = FALSE))
          cat(sprintf("  [CAP] %-28s [%s] %4d vals → [%.2f, %.2f]\n",
                      col, g, nb + na, li, ls))
        }
      }
    } else {
      q1 <- quantile(vals_ok, 0.25); q3 <- quantile(vals_ok, 0.75)
      iqr <- q3 - q1; if (iqr == 0) next
      li <- q1 - FACTOR * iqr; ls <- q3 + FACTOR * iqr
      nb <- sum(vals < li, na.rm = TRUE); na <- sum(vals > ls, na.rm = TRUE)
      if (nb + na > 0) {
        df[which(vals < li), col] <- li
        df[which(vals > ls), col] <- ls
        pct <- round(100 * (nb + na) / nf, 2)
        reporte <- rbind(reporte, data.frame(
          Hoja = h, Columna = col, Grupo = "(global)",
          Lim_Inferior = round(li, 2), Lim_Superior = round(ls, 2),
          Valores_Cap = nb + na, Pct_Afectado = pct, stringsAsFactors = FALSE))
        cat(sprintf("  [CAP] %-28s [global] %4d vals → [%.2f, %.2f]\n",
                    col, nb + na, li, ls))
      }
    }
  }
  datos[[h]] <- df
}

reporte <- reporte %>% arrange(desc(Valores_Cap))

wb <- createWorkbook()
addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", reporte)
hdr <- createStyle(fontColour = "#FFFFFF", fgFill = "#E67E22",
                   textDecoration = "bold", halign = "center")
addStyle(wb, "Resumen", hdr, rows = 1, cols = 1:7, gridExpand = TRUE)
setColWidths(wb, "Resumen", cols = 1:7, widths = c(22, 28, 22, 14, 14, 12, 12))
for (h in hojas) {
  addWorksheet(wb, h)
  writeData(wb, h, datos[[h]])
  addStyle(wb, h, hdr, rows = 1, cols = 1:ncol(datos[[h]]), gridExpand = TRUE)
}
saveWorkbook(wb, OUTPUT, overwrite = TRUE)

cat(sprintf("\nOUTLIERS COMPLETO → %s  (%d grupos, %d valores winsorizados)\n",
            OUTPUT, nrow(reporte), sum(reporte$Valores_Cap)))
