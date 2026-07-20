# ============================================================
# limpieza_datos.R
# Librería de funciones de limpieza/transformación.
# Este archivo SOLO define funciones — no lee el Excel,
# no ejecuta nada. Se carga desde main.R con source().
# ============================================================

library(dplyr)

# ------------------------------------------------------------
# 1. Tratamiento de nulos: imputa con la mediana del grupo
# ------------------------------------------------------------
tratar_valores_nulos <- function(datos, columna, columnas_grupo,
                                 es_entero = FALSE, estrategia = "mediana") {
  col_imputada <- paste0(columna, "_imputada")
  col_es_nulo <- paste0(columna, "_es_nulo_imputado")
  col_motivo <- paste0(columna, "_motivo_imputacion")

  datos <- datos %>%
    group_by(across(all_of(columnas_grupo))) %>%
    mutate(
      !!col_es_nulo := is.na(.data[[columna]]),
      !!col_imputada := ifelse(
        is.na(.data[[columna]]),
        median(.data[[columna]], na.rm = TRUE),
        .data[[columna]]
      ),
      !!col_motivo := ifelse(is.na(.data[[columna]]), "mediana_grupo", NA_character_)
    ) %>%
    ungroup()

  if (es_entero) {
    datos[[col_imputada]] <- round(datos[[col_imputada]])
  }

  return(datos)
}

# ------------------------------------------------------------
# 2. Tratamiento de outliers con IQR, con regla de negocio opcional
# ------------------------------------------------------------
tratar_outliers_IQR <- function(datos, columna, columnas_grupo,
                                minimo_valido = NULL, es_entero = FALSE,
                                factor = 1.5) {
  col_limpia <- paste0(columna, "_limpia")
  col_outlier <- paste0(columna, "_es_outlier")
  col_motivo <- paste0(columna, "_motivo_outlier")

  # CORRECCIÓN: si minimo_valido es NULL, `.data[[columna]] < NULL` da un
  # vector de tamaño 0 (no FALSE), lo que rompe case_when() al intentar
  # combinarlo con columnas de otro tamaño. Usamos -Inf como "sin regla
  # de negocio", que siempre compara de forma segura y da FALSE.
  if (is.null(minimo_valido)) minimo_valido <- -Inf

  datos <- datos %>%
    group_by(across(all_of(columnas_grupo))) %>%
    mutate(
      .Q1 = quantile(.data[[columna]], 0.25, na.rm = TRUE),
      .Q3 = quantile(.data[[columna]], 0.75, na.rm = TRUE),
      .IQR = .Q3 - .Q1,
      .lim_inf = .Q1 - factor * .IQR,
      .lim_sup = .Q3 + factor * .IQR,
      !!col_outlier := case_when(
        .data[[columna]] < minimo_valido ~ TRUE,
        .data[[columna]] < .lim_inf ~ TRUE,
        .data[[columna]] > .lim_sup ~ TRUE,
        TRUE ~ FALSE
      ),
      !!col_motivo := case_when(
        .data[[columna]] < minimo_valido ~ "REGLA_NEGOCIO",
        .data[[columna]] < .lim_inf ~ "IQR_INFERIOR",
        .data[[columna]] > .lim_sup ~ "IQR_SUPERIOR",
        TRUE ~ NA_character_
      ),
      !!col_limpia := .data[[columna]]
    ) %>%
    ungroup() %>%
    select(-.Q1, -.Q3, -.IQR, -.lim_inf, -.lim_sup)

  if (es_entero) {
    datos[[col_limpia]] <- round(datos[[col_limpia]])
  }

  return(datos)
}

# ------------------------------------------------------------
# 3. Estandarización z-score dentro de cada grupo
# ------------------------------------------------------------
estandarizar_variable <- function(datos, columna, columnas_grupo, metodo = "zscore") {
  col_estandarizada <- paste0(columna, "_estandarizada")

  datos <- datos %>%
    group_by(across(all_of(columnas_grupo))) %>%
    mutate(
      !!col_estandarizada := round(
        (.data[[columna]] - mean(.data[[columna]], na.rm = TRUE)) /
          sd(.data[[columna]], na.rm = TRUE),
        4
      )
    ) %>%
    ungroup()

  return(datos)
}

# ------------------------------------------------------------
# 4. Orquestador: aplica las 3 técnicas a cada fila de una
#    tabla de configuración (hoja, columna, grupo). No lee
#    el Excel ni conoce archivos — solo recibe datos en memoria.
# ------------------------------------------------------------
transformar_todas_hojas <- function(hojas, config) {
  for (i in seq_len(nrow(config))) {
    nombre_hoja <- config$hoja[i]
    columna <- config$columna[i]
    grupo <- if (is.na(config$columna_grupo[i])) character(0) else config$columna_grupo[i]
    es_entero <- config$es_entero[i]
    min_valido <- if (is.na(config$minimo_valido[i])) NULL else config$minimo_valido[i]

    datos <- hojas[[nombre_hoja]]
    datos[[columna]] <- suppressWarnings(as.numeric(datos[[columna]]))

    datos <- tratar_valores_nulos(datos, columna, grupo, es_entero = es_entero)
    datos <- tratar_outliers_IQR(datos, columna, grupo,
      minimo_valido = min_valido, es_entero = es_entero
    )
    datos <- estandarizar_variable(datos, columna, grupo)

    hojas[[nombre_hoja]] <- datos

    message(sprintf(
      "Procesado: %s / %s (grupo: %s)",
      nombre_hoja, columna,
      if (length(grupo) == 0) "sin grupo" else grupo
    ))
  }

  return(hojas)
}
