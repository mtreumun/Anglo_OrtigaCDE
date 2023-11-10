#install.packages("sf")
#install.packages("ggplot2")
#install.packages("gridextra")
#install.packages("classInt")
#install.packages("gridExtra")


# Cargar las librerías
library(sf)
library(ggplot2)
library(gridExtra)
library(classInt)
library(gridExtra)

# Leer el archivo shapefile
shapefile_path <- "E:/ANGLO_Ortiga CDE/CAPAS/Unidades_vegetación_Ortiga_Ajuste_Dron_noviembre_2023_ME_MT_20231108_v1.shp"
shp <- st_read(shapefile_path)

# Convertir a numérico si no lo son ya y manejar NAs
shp$MEAN <- as.numeric(shp$MEAN)
shp$STD <- as.numeric(shp$STD)
shp$AREA <- as.numeric(shp$AREA)
shp <- shp[shp$AREA != 0, ]

# Asumiendo que deseas excluir los NAs, actualiza el dataset para excluirlos
shp <- shp[!is.na(shp$MEAN) & !is.na(shp$STD) & !is.na(shp$AREA), ]

# Recalcular la normalización por área y por STD
shp$MEAN_per_area <- shp$MEAN / shp$AREA
shp$MEAN_normalized_STD <- (shp$MEAN - mean(shp$MEAN)) / shp$STD

# Calcular el rango y definir los intervalos
range_MEAN <- max(shp$MEAN, na.rm = TRUE) - min(shp$MEAN, na.rm = TRUE)
range_MEAN_per_area <- max(shp$MEAN_per_area, na.rm = TRUE) - min(shp$MEAN_per_area, na.rm = TRUE)
range_MEAN_normalized_STD <- max(shp$MEAN_normalized_STD, na.rm = TRUE) - min(shp$MEAN_normalized_STD, na.rm = TRUE)

interval_MEAN <- range_MEAN / 10
interval_MEAN_per_area <- range_MEAN_per_area / 10
interval_MEAN_normalized_STD <- range_MEAN_normalized_STD / 10

# Definir los intervalos para las categorías
breaks_MEAN <- seq(min(shp$MEAN, na.rm = TRUE), max(shp$MEAN, na.rm = TRUE), by = interval_MEAN)
breaks_MEAN_per_area <- seq(min(shp$MEAN_per_area, na.rm = TRUE), max(shp$MEAN_per_area, na.rm = TRUE), by = interval_MEAN_per_area)
breaks_MEAN_normalized_STD <- seq(min(shp$MEAN_normalized_STD, na.rm = TRUE), max(shp$MEAN_normalized_STD, na.rm = TRUE), by = interval_MEAN_normalized_STD)

# Crear factores basados en intervalos geométricos con etiquetas numéricas
shp$MEAN_cat <- cut(shp$MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = FALSE)
shp$MEAN_per_area_cat <- cut(shp$MEAN_per_area, breaks = breaks_MEAN_per_area, include.lowest = TRUE, labels = FALSE)
shp$MEAN_normalized_STD_cat <- cut(shp$MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = FALSE)

# Función para crear etiquetas de intervalos
create_interval_labels <- function(breaks) {
  intervals <- length(breaks) - 1
  labels <- vector("list", intervals)
  for (i in 1:intervals) {
    labels[[i]] <- paste(format(round(breaks[i], 2)), "-", format(round(breaks[i + 1], 2)))
  }
  return(labels)
}

# Crear etiquetas de intervalos para cada conjunto de datos
labels_MEAN <- create_interval_labels(breaks_MEAN)
labels_MEAN_per_area <- create_interval_labels(breaks_MEAN_per_area)
labels_MEAN_normalized_STD <- create_interval_labels(breaks_MEAN_normalized_STD)

# Crear mapas temáticos con etiquetas de intervalos
#plot_shp_MEAN_cat <- ggplot(data = shp) +
#geom_sf(aes(fill = cut(MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = labels_MEAN)), color = NA) +
#scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN)), labels = labels_MEAN) +
#theme_minimal() +
#labs(fill = "Rango", title = "Mapa de MEAN Categorizado") +
#theme(legend.position = "bottom")

#plot_shp_MEAN_per_area_cat <- ggplot(data = shp) +
#geom_sf(aes(fill = cut(MEAN_per_area, breaks = breaks_MEAN_per_area, include.lowest = TRUE, labels = labels_MEAN_per_area)), color = NA) +
#scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_per_area)), labels = labels_MEAN_per_area) +
#theme_minimal() +
#labs(fill = "Rango", title = "Mapa de MEAN por Área Categorizado") +
#theme(legend.position = "bottom")

#plot_shp_MEAN_normalized_STD_cat <- ggplot(data = shp) +
#geom_sf(aes(fill = cut(MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = labels_MEAN_normalized_STD)), color = NA) +
#scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_normalized_STD)), labels = labels_MEAN_normalized_STD) +
#theme_minimal() +
#labs(fill = "Rango", title = "Mapa de MEAN Normalizado por STD Categorizado") +
#theme(legend.position = "bottom")

# Mostrar los mapas
#print(plot_shp_MEAN_cat)
#print(plot_shp_MEAN_per_area_cat)
#print(plot_shp_MEAN_normalized_STD_cat)

# Histograma para MEAN
#hist_shp_MEAN <- ggplot(data = shp, aes(x = MEAN)) +
  #geom_histogram(bins = 30, fill = "blue", color = "black") +
  #theme_minimal() +
  #ggtitle("Histograma NDVI")

# Histograma para MEAN_per_area
#hist_shp_MEAN_per_area <- ggplot(data = shp, aes(x = MEAN_per_area)) +
  #geom_histogram(bins = 30, fill = "blue", color = "black") +
  #theme_minimal() +
  #ggtitle("Histograma de MEAN por Área")

# Histograma para MEAN_normalized_STD
#hist_shp_MEAN_normalized_STD <- ggplot(data = shp, aes(x = MEAN_normalized_STD)) +
  #geom_histogram(bins = 30, fill = "blue", color = "black") +
  #theme_minimal() +
  #ggtitle("Histograma de MEAN Normalizado por STD")

# Mostrar mapas y histogramas juntos
#grid.arrange(plot_shp_MEAN_cat, hist_shp_MEAN, ncol = 2)
#grid.arrange(plot_shp_MEAN_per_area_cat, hist_shp_MEAN_per_area, ncol = 2)
#grid.arrange(plot_shp_MEAN_normalized_STD_cat, hist_shp_MEAN_normalized_STD, ncol = 2)

# Crear mapas temáticos con etiquetas de intervalos
plot_shp_MEAN_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = labels_MEAN)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN)), labels = labels_MEAN) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 5, angle = 90), # Reducir tamaño y rotar las etiquetas de la leyenda
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
    axis.text.y = element_text(size = 6, angle = 90, hjust = 1, vjust = 0.5),
    axis.ticks = element_blank(),
    legend.key.size = unit(1, 'lines'), # Ajustar tamaño de las cajas de leyenda para dar más espacio entre etiquetas
    legend.spacing.x = unit(0.1, 'cm'), # Espaciado entre las leyendas
    legend.margin = margin(t = 0, b = 0, unit = 'pt') # Reducir el margen alrededor de la leyenda
  ) +
  labs(fill = "", title = "Mapa NDVI")

# Mejorar el histograma para MEAN
hist_shp_MEAN <- ggplot(data = shp, aes(x = MEAN)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 6, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y para evitar repetición
    axis.title.x = element_blank(), # Ocultar el título del eje x
    axis.title.y = element_blank(), # Ocultar el título del eje y
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI")

# Combinar el mapa mejorado y el histograma en una sola visualización
grid.arrange(plot_shp_MEAN_cat, hist_shp_MEAN, ncol = 2, widths = c(3, 2))

# Mejorar el estilo del mapa para MEAN per Area
plot_shp_MEAN_per_area_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_per_area, breaks = breaks_MEAN_per_area, include.lowest = TRUE, labels = labels_MEAN_per_area)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_per_area)), labels = labels_MEAN_per_area) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 5, angle = 90),
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_blank(), # Ocultar las etiquetas del eje x para evitar repetición
    axis.text.y = element_blank(), # Ocultar las etiquetas del eje y para evitar repetición
    axis.ticks = element_blank()
  ) +
  labs(fill = "", title = "Mapa NDVI Norm. Área")

# Mejorar el histograma para MEAN per Area
hist_shp_MEAN_per_area <- ggplot(data = shp, aes(x = MEAN_per_area)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 6, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y para evitar repetición
    axis.title.x = element_blank(), # Ocultar el título del eje x
    axis.title.y = element_blank(), # Ocultar el título del eje y
  ) +
  ggtitle("Histograma NDVI Norm. Área")

# Combinar el mapa y el histograma para MEAN per Area
grid.arrange(plot_shp_MEAN_per_area_cat, hist_shp_MEAN_per_area, ncol = 2, widths = c(3, 2))

# Mejorar el estilo del mapa para MEAN Normalizado por STD
plot_shp_MEAN_normalized_STD_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = labels_MEAN_normalized_STD)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_normalized_STD)), labels = labels_MEAN_normalized_STD) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text = element_text(size = 5, angle = 90),
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    plot.margin = margin(5, 5, 5, 5),
    axis.text.x = element_blank(), # Ocultar las etiquetas del eje x para evitar repetición
    axis.text.y = element_blank(), # Ocultar las etiquetas del eje y para evitar repetición
    axis.ticks = element_blank()
  ) +
  labs(fill = "", title = "Mapa NDVI Norm. por STD")

# Mejorar el histograma para MEAN Normalizado por STD
hist_shp_MEAN_normalized_STD <- ggplot(data = shp, aes(x = MEAN_normalized_STD)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 6, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y para evitar repetición
    axis.title.x = element_blank(), # Ocultar el título del eje x
    axis.title.y = element_blank(), # Ocultar el título del eje y
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI Norm. STD ")

# Combinar el mapa y el histograma para MEAN Normalizado por STD
grid.arrange(plot_shp_MEAN_normalized_STD_cat, hist_shp_MEAN_normalized_STD, ncol = 2, widths = c(3, 2))
