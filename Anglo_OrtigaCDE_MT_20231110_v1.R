# Cargar las librerías
library(sf)
library(ggplot2)
library(gridExtra)
library(classInt)

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
plot_shp_MEAN_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = labels_MEAN)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN)), labels = labels_MEAN) +
  theme_minimal() +
  labs(fill = "Rango", title = "Mapa de MEAN Categorizado") +
  theme(legend.position = "bottom")

plot_shp_MEAN_per_area_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_per_area, breaks = breaks_MEAN_per_area, include.lowest = TRUE, labels = labels_MEAN_per_area)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_per_area)), labels = labels_MEAN_per_area) +
  theme_minimal() +
  labs(fill = "Rango", title = "Mapa de MEAN por Área Categorizado") +
  theme(legend.position = "bottom")

plot_shp_MEAN_normalized_STD_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = labels_MEAN_normalized_STD)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_normalized_STD)), labels = labels_MEAN_normalized_STD) +
  theme_minimal() +
  labs(fill = "Rango", title = "Mapa de MEAN Normalizado por STD Categorizado") +
  theme(legend.position = "bottom")

# Mostrar los mapas
print(plot_shp_MEAN_cat)
print(plot_shp_MEAN_per_area_cat)
print(plot_shp_MEAN_normalized_STD_cat)
