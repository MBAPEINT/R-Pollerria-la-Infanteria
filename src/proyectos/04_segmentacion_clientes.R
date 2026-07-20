# ============================================================
# PROYECTO 4: SEGMENTACIÓN DE CLIENTES
# Modelo: K-Means sobre datos de clientes
# Objetivo: Agrupar clientes en VIP, Frecuentes, Ocasionales, Nuevos, Perdidos
# ============================================================
library(mongolite)
library(dplyr)
library(ggplot2)
library(lubridate)

dir.create("outputs/proyectos", showWarnings = FALSE, recursive = TRUE)
set.seed(123)

# --- 1. Cargar datos ---
ventas   <- mongo(collection = "Ventas", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')
clientes <- mongo(collection = "Clientes", db = "Polleria", url = Sys.getenv("MONGO_URL", "mongodb://localhost:27017"))$find('{}')

ventas$FechaVenta <- as.Date(as.POSIXct(ventas$FechaVenta, tz = "UTC"))
hoy <- max(ventas$FechaVenta, na.rm = TRUE)

# --- 2. Calcular métricas por cliente ---
rfm <- ventas %>%
  group_by(Cliente) %>%
  summarise(
    recencia      = as.numeric(hoy - max(FechaVenta)),        # días desde última compra
    frecuencia    = n(),                                        # número de compras
    gasto_total   = sum(Total, na.rm = TRUE),                  # dinero total gastado
    ticket_promedio = mean(Total, na.rm = TRUE),               # gasto por visita
    primera_compra = as.numeric(hoy - min(FechaVenta)),        # antigüedad
    .groups = "drop"
  ) %>%
  filter(gasto_total > 0)

cat("Clientes con compras:", nrow(rfm), "\n")

# --- 3. Estandarizar para K-Means ---
rfm_z <- rfm %>%
  mutate(
    recencia_z      = scale(recencia),
    frecuencia_z    = scale(frecuencia),
    gasto_z         = scale(gasto_total),
    ticket_z        = scale(ticket_promedio),
    antiguedad_z    = scale(primera_compra)
  )

# Seleccionar features para clustering
X <- rfm_z %>% select(recencia_z, frecuencia_z, gasto_z, ticket_z) %>% as.matrix()
X[is.na(X)] <- 0

# --- 4. K-Means: encontrar número óptimo de clusters ---
# Método del codo
wss <- sapply(1:10, function(k) {
  tryCatch(kmeans(X, k, nstart = 25, iter.max = 20)$tot.withinss, error = function(e) NA)
})

# Elegir k=5 segmentos (VIP, Frecuente, Ocasional, Nuevo, Perdido)
k <- 5
km <- kmeans(X, centers = k, nstart = 25, iter.max = 20)

rfm$segmento_num <- km$cluster

# --- 5. Nombrar los segmentos según sus características ---
perfil <- rfm %>%
  group_by(segmento_num) %>%
  summarise(
    n = n(),
    gasto_medio = mean(gasto_total),
    frecuencia_media = mean(frecuencia),
    recencia_media = mean(recencia),
    ticket_medio = mean(ticket_promedio),
    .groups = "drop"
  ) %>%
  arrange(desc(gasto_medio))

# Asignar nombres basados en características reales
# Ordenar segmentos por gasto_medio descendente
perfil <- perfil %>% arrange(desc(gasto_medio))
perfil$segmento <- c("VIP", "FRECUENTE", "OCASIONAL", "NUEVO", "PERDIDO")[1:nrow(perfil)]

# Mapear a los clientes
rfm <- rfm %>% left_join(perfil[, c("segmento_num", "segmento")], by = "segmento_num")

cat("\nSegmentos encontrados:\n")
for (i in 1:nrow(perfil)) {
  cat(sprintf("  %-12s %5d clientes (%5.1f%%) | Gasto medio: S/ %7.0f | Frecuencia: %4.1f\n",
    perfil$segmento[i], perfil$n[i], 100*perfil$n[i]/nrow(rfm),
    perfil$gasto_medio[i], perfil$frecuencia_media[i]))
}

# --- 6. Sugerir acción por segmento ---
cat("\n========== ESTRATEGIA POR SEGMENTO ==========\n")
cat("  VIP:        Programa de fidelización premium, atención personalizada\n")
cat("  FRECUENTE:  Incentivar mayor ticket con combos y upsell\n")
cat("  OCASIONAL:  Promociones para aumentar frecuencia de visita\n")
cat("  NUEVO:      Bienvenida + descuento primera compra + seguimiento\n")
cat("  PERDIDO:    Campaña de reconquista con descuento agresivo\n")

# --- 7. Gráficos ---
# Gráfico 1: Segmentos en espacio recencia vs gasto
p1 <- rfm %>%
  ggplot(aes(x = recencia, y = gasto_total, color = segmento, size = frecuencia)) +
  geom_point(alpha = 0.6) +
  scale_color_manual(values = c("VIP"="#E74C3C","FRECUENTE"="#3498DB","OCASIONAL"="#2ECC71",
                                "NUEVO"="#F39C12","PERDIDO"="#95A5A6")) +
  scale_size_continuous(range = c(0.5, 4)) +
  labs(title = "Segmentación de clientes",
       subtitle = paste0(nrow(rfm), " clientes agrupados en ", k, " segmentos"),
       x = "Días desde última compra (recencia)", y = "Gasto total (S/)",
       color = "Segmento", size = "Frecuencia") +
  theme_minimal()
ggsave("outputs/proyectos/04_segmentos_clientes.png", p1, width = 10, height = 6, dpi = 150)

# Gráfico 2: Tamaño de cada segmento
p2 <- perfil %>%
  ggplot(aes(x = reorder(segmento, n), y = n, fill = segmento)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = paste0(n, " (", round(n/sum(n)*100, 1), "%)")),
            hjust = -0.1, size = 4) +
  scale_fill_manual(values = c("VIP"="#E74C3C","FRECUENTE"="#3498DB","OCASIONAL"="#2ECC71",
                               "NUEVO"="#F39C12","PERDIDO"="#95A5A6"), guide = "none") +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25))) +
  labs(title = "Distribución de clientes por segmento",
       x = "", y = "Número de clientes") +
  theme_minimal()
ggsave("outputs/proyectos/04_distribucion_segmentos.png", p2, width = 8, height = 4, dpi = 150)

# --- 8. Guardar ---
write.csv(rfm, "data/output/segmentacion_clientes.csv", row.names = FALSE)

cat("\nArchivos guardados:\n")
cat("  - data/output/segmentacion_clientes.csv (todos los clientes con su segmento)\n")
cat("  - outputs/proyectos/04_segmentos_clientes.png\n")
cat("  - outputs/proyectos/04_distribucion_segmentos.png\n")
