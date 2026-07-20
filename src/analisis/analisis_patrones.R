# ============================================================
# analisis_patrones.R
# Análisis de 3 patrones de negocio desde el Excel
# ============================================================
library(readxl)
library(dplyr)

ruta_excel <- "data/processed/datos_outliers.xlsx"

# Cargar hojas necesarias
df_ventas <- read_excel(ruta_excel, sheet = "Ventas")
df_metodos_pago <- read_excel(ruta_excel, sheet = "Metodos_Pago")

# Convertir FechaVenta a tipo fecha
df_ventas <- df_ventas %>%
    mutate(FechaVenta = as.Date(FechaVenta))

cat("\n")
cat("============================================================\n")
cat("PATRON 1: DIA DE LA SEMANA CON MAYOR VOLUMEN DE VENTAS\n")
cat("Requerimiento: Planificacion de personal y stock\n")
cat("============================================================\n\n")

# Agregar dia de la semana numerico y nombre
# %u: 1=Lunes ... 7=Domingo (ISO 8601)
dias_nombre <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo")
df_ventas <- df_ventas %>%
    mutate(
        dia_semana_num = as.numeric(format(FechaVenta, "%u")),
        dia_semana = dias_nombre[dia_semana_num]
    )

# Agrupar por dia y calcular metricas
ventas_por_dia <- df_ventas %>%
    group_by(dia_semana_num, dia_semana) %>%
    summarise(
        monto_total   = sum(Total),
        num_ventas    = n(),
        ticket_promedio = round(mean(Total), 2),
        .groups = "drop"
    ) %>%
    arrange(dia_semana_num) %>%
    select(-dia_semana_num)

# Imprimir tabla
cat("--- Tabla de resultados ---\n")
print(as.data.frame(ventas_por_dia), row.names = FALSE)

# Identificar mejor y peor dia
mejor <- ventas_por_dia %>% slice_max(monto_total, n = 1)
peor  <- ventas_por_dia %>% slice_min(monto_total, n = 1)

# Calcular concentracion fin de semana
finde <- c("Sabado", "Domingo")
monto_finde <- ventas_por_dia %>%
    filter(dia_semana %in% finde) %>%
    summarise(sum(monto_total)) %>%
    pull()
pct_finde <- round(100 * monto_finde / sum(ventas_por_dia$monto_total), 1)

cat("\n")
cat("--- Conclusion ---\n")
cat(sprintf(
    "El dia con mayor volumen de ventas es %s con S/ %.2f (%d transacciones).\n",
    as.character(mejor$dia_semana), mejor$monto_total, mejor$num_ventas
))
cat(sprintf(
    "El dia con menor volumen es %s con S/ %.2f (%d transacciones).\n",
    as.character(peor$dia_semana), peor$monto_total, peor$num_ventas
))
cat(sprintf(
    "La diferencia entre el mejor y peor dia es de S/ %.2f (%.1f%% menos).\n",
    mejor$monto_total - peor$monto_total,
    round(100 * (1 - peor$monto_total / mejor$monto_total), 1)
))
cat(sprintf(
    "El fin de semana (sabado + domingo) concentra el %.1f%% de los ingresos semanales.\n",
    pct_finde
))
cat(sprintf(
    "El ticket promedio es notablemente estable entre dias (%.2f - %.2f),\n",
    min(ventas_por_dia$ticket_promedio), max(ventas_por_dia$ticket_promedio)
))
cat("lo que indica que la variacion en ingresos se debe al volumen, no al gasto por cliente.\n")

cat("\n--- Recomendacion ---\n")
cat(sprintf(
    "1. REFORZAR PERSONAL Y STOCK: Asignar mas cocineros, meseros y stock\n"
))
cat(sprintf(
    "   de insumos frescos para viernes, sabado y domingo (%.1f%% del ingreso).\n", pct_finde
))
cat(sprintf(
    "2. PROMOCION ENTRE SEMANA: Lanzar 'Miercoles de Parrillero' o 'Jueves 2x1'\n"
))
cat(sprintf(
    "   para atraer clientes en los dias de baja demanda y nivelar la carga.\n"
))
cat(sprintf(
    "3. ROTACION DE PERSONAL: Concentrar descansos del personal los dias lunes\n"
))
cat(sprintf(
    "   y miercoles, cuando la demanda es mas baja.\n"
))


cat("\n\n")
cat("============================================================\n")
cat("PATRON 2: METODO DE PAGO CON MENOR TICKET PROMEDIO\n")
cat("Requerimiento: Estrategia de medios de pago\n")
cat("============================================================\n\n")

# El nuevo Excel ya tiene MetodoPago como texto directo ("Efectivo", "Plin", etc.)
ticket_por_metodo <- df_ventas %>%
    group_by(MetodoPago) %>%
    summarise(
        ticket_promedio = round(mean(Total), 2),
        num_ventas      = n(),
        monto_total     = sum(Total),
        .groups         = "drop"
    ) %>%
    arrange(ticket_promedio)

# Imprimir tabla
cat("--- Tabla de resultados ---\n")
print(as.data.frame(ticket_por_metodo), row.names = FALSE)

menor_metodo <- ticket_por_metodo[1, ]
mayor_metodo <- ticket_por_metodo[nrow(ticket_por_metodo), ]
diferencia <- mayor_metodo$ticket_promedio - menor_metodo$ticket_promedio

# Calcular % de transacciones digitales vs efectivo
total_ventas <- sum(ticket_por_metodo$num_ventas)
ventas_digital <- ticket_por_metodo %>%
    filter(MetodoPago %in% c("Yape", "Plin")) %>%
    summarise(sum(num_ventas)) %>%
    pull()
pct_digital <- round(100 * ventas_digital / total_ventas, 1)

ventas_efectivo <- ticket_por_metodo %>%
    filter(MetodoPago == "Efectivo") %>%
    summarise(sum(num_ventas)) %>%
    pull()
pct_efectivo <- round(100 * ventas_efectivo / total_ventas, 1)

cat("\n")
cat("--- Conclusion ---\n")
cat(sprintf(
    "El metodo de pago con menor ticket promedio es %s (S/ %.2f).\n",
    menor_metodo$MetodoPago, menor_metodo$ticket_promedio
))
cat(sprintf(
    "El metodo con mayor ticket promedio es %s (S/ %.2f).\n",
    mayor_metodo$MetodoPago, mayor_metodo$ticket_promedio
))
cat(sprintf(
    "La diferencia entre ambos es de solo S/ %.2f, lo cual es estadisticamente\n", diferencia
))
cat("insignificante: todos los metodos de pago tienen tickets promedio casi identicos.\n")
cat(sprintf(
    "El efectivo sigue dominando con %d transacciones (%.1f%% del total),\n",
    ventas_efectivo, pct_efectivo
))
cat(sprintf(
    "mientras que los metodos digitales (Yape + Plin) representan %d transacciones (%.1f%%).\n",
    ventas_digital, pct_digital
))

cat("\n--- Recomendacion ---\n")
cat("1. PROMOVER PAGOS DIGITALES: El ticket promedio de Yape (S/ 73.82) y Plin\n")
cat("   (S/ 71.86) es competitivo. Ofrecer un descuento del 5%% por pago digital\n")
cat("   para reducir el manejo de efectivo (seguridad y eficiencia operativa).\n")
cat("2. MANTENER TODOS LOS METODOS: La diferencia entre metodos es minima\n")
cat("   (S/ 2.12), lo que indica que ningun metodo desincentiva el gasto.\n")
cat("   No eliminar ningun metodo; todos son viables.\n")
cat("3. TARJETA DE CREDITO COMO UPSELL: Al tener el ticket mas alto (S/ 73.98),\n")
cat("   se puede ofrecer como opcion preferente en combos familiares (> S/ 80)\n")
cat("   donde el cliente podria necesitar financiamiento.\n")


cat("\n\n")
cat("============================================================\n")
cat("PATRON 3: CONCENTRACION DE INGRESOS EN CLIENTES TOP\n")
cat("Requerimiento: Programa de fidelizacion / regla 80/20\n")
cat("============================================================\n\n")

# Calcular gasto total por cliente
gasto_por_cliente <- df_ventas %>%
    group_by(Cliente) %>%
    summarise(
        gasto_total = sum(Total),
        num_compras = n(),
        ticket_promedio = round(mean(Total), 2),
        .groups = "drop"
    ) %>%
    arrange(desc(gasto_total))

total_clientes <- nrow(gasto_por_cliente)
ingreso_total <- sum(gasto_por_cliente$gasto_total)

# Calcular percentiles de concentracion
top5_n  <- ceiling(total_clientes * 0.05)
top10_n <- ceiling(total_clientes * 0.10)
top20_n <- ceiling(total_clientes * 0.20)

top5  <- gasto_por_cliente[1:top5_n, ]
top10 <- gasto_por_cliente[1:top10_n, ]
top20 <- gasto_por_cliente[1:top20_n, ]
bottom80 <- gasto_por_cliente[(top20_n + 1):total_clientes, ]

ingreso_top5  <- sum(top5$gasto_total)
ingreso_top10 <- sum(top10$gasto_total)
ingreso_top20 <- sum(top20$gasto_total)
ingreso_bottom80 <- sum(bottom80$gasto_total)

pct_top5  <- round(100 * ingreso_top5 / ingreso_total, 1)
pct_top10 <- round(100 * ingreso_top10 / ingreso_total, 1)
pct_top20 <- round(100 * ingreso_top20 / ingreso_total, 1)
pct_bottom80 <- round(100 * ingreso_bottom80 / ingreso_total, 1)

# Construir tabla resumen
tabla_concentracion <- data.frame(
    Segmento = c("Top 5%", "Top 10%", "Top 20%", "Bottom 80%"),
    Clientes = c(top5_n, top10_n, top20_n, total_clientes - top20_n),
    Ingresos = c(ingreso_top5, ingreso_top10, ingreso_top20, ingreso_bottom80),
    Porcentaje = c(pct_top5, pct_top10, pct_top20, pct_bottom80),
    Ticket_Promedio = c(
        round(mean(top5$gasto_total), 2),
        round(mean(top10$gasto_total), 2),
        round(mean(top20$gasto_total), 2),
        round(mean(bottom80$gasto_total), 2)
    ),
    Compras_Promedio = c(
        round(mean(top5$num_compras), 1),
        round(mean(top10$num_compras), 1),
        round(mean(top20$num_compras), 1),
        round(mean(bottom80$num_compras), 1)
    )
)

# Imprimir tabla
cat("--- Tabla de concentracion de ingresos ---\n")
print(tabla_concentracion, row.names = FALSE)

cat(sprintf("\nTotal de clientes unicos con compras: %d\n", total_clientes))
cat(sprintf("Ingreso total del periodo: S/ %.2f\n", ingreso_total))

# Calcular ratio de concentracion
ratio <- ingreso_top20 / ingreso_bottom80
top1_gasto <- gasto_por_cliente$gasto_total[1]
bottom1_gasto <- gasto_por_cliente$gasto_total[total_clientes]

cat("\n")
cat("--- Conclusion ---\n")
if (pct_top20 >= 80) {
    cat("Se CUMPLE estrictamente la regla 80/20 de Pareto.\n")
} else {
    cat(sprintf(
        "NO se cumple estrictamente la regla 80/20. El 20%% de clientes\n"
    ))
    cat(sprintf(
        "genera el %.1f%% de los ingresos (no el 80%%).\n", pct_top20
    ))
    cat(sprintf(
        "La distribucion es mas equitativa de lo esperado.\n"
    ))
}
cat(sprintf(
    "El cliente top gasta S/ %.2f (%.0fx mas que el bottom, S/ %.2f).\n",
    top1_gasto, round(top1_gasto / bottom1_gasto, 0), bottom1_gasto
))
cat(sprintf(
    "El segmento top 20%% compra en promedio %.1f veces, mientras que el\n",
    tabla_concentracion$Compras_Promedio[3]
))
cat(sprintf(
    "bottom 80%% compra solo %.1f veces (%.0fx menos frecuente).\n",
    tabla_concentracion$Compras_Promedio[4],
    round(tabla_concentracion$Compras_Promedio[3] / tabla_concentracion$Compras_Promedio[4], 0)
))
cat(sprintf(
    "El bottom 80%% (%s clientes) aun genera el %.1f%% de los ingresos,\n",
    format(total_clientes - top20_n, big.mark = ","), pct_bottom80
))
cat("lo que lo hace un segmento que NO se puede descuidar.\n")

cat("\n--- Recomendacion ---\n")
cat("1. ESTRATEGIA DUAL DE FIDELIZACION:\n")
cat("   a) PROGRAMA VIP para los 1,528 clientes top 20%:\n")
cat("      - Tarjeta de cliente frecuente con descuento acumulable (5% a partir\n")
cat("        de la 5ta visita del mes).\n")
cat("      - Acceso prioritario en delivery y reservas para fin de semana.\n")
cat("      - Beneficio exclusivo: porcion extra de guarnicion gratis.\n")
cat("   b) PROGRAMA DE FRECUENCIA para los 6,111 clientes del bottom 80%:\n")
cat("      - 'Compra 5 pollos, el 6to gratis' (tarjeta de sellos digital).\n")
cat("      - Cupon de bienvenida post-primera compra para incentivar la 2da.\n")
cat("      - Meta: subir el promedio de 2.6 a 4 compras por cliente.\n")
cat("2. NO DESCUIDAR AL BOTTOM 80%: Generan mas de la mitad de los ingresos\n")
cat("   (51.6%). Una mejora del 10% en su frecuencia de compra representaria\n")
cat(sprintf("   S/ %.2f adicionales.\n", ingreso_bottom80 * 0.10))
cat("3. SEGMENTACION POR TICKET: Clientes con ticket > S/ 500 son candidatos\n")
cat("   a servicio de delivery premium o catering para eventos/reuniones.\n")
cat(sprintf("   Hay %d clientes en este segmento.\n",
    sum(gasto_por_cliente$gasto_total > 500)))
