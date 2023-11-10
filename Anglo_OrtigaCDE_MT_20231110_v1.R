# Instalar paquetes necesarios
# install.packages("sf")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("classInt")

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

# Calcular intervalos para las categorías nuevamente
num_categories <- 10
breaks_MEAN <- classIntervals(shp$MEAN, n = num_categories, style = "quantile")$brks
breaks_MEAN_per_area <- classIntervals(shp$MEAN_per_area, n = num_categories, style = "quantile")$brks
breaks_MEAN_normalized_STD <- classIntervals(shp$MEAN_normalized_STD, n = num_categories, style = "quantile")$brks

# Etiquetas para las categorías
category_labels <- c("Excepcionalmente bajo", "Extremadamente bajo", "Muy bajo", "Bajo",
                     "Moderadamente bajo", "Medio", "Moderadamente alto", "Alto",
                     "Muy alto", "Extremadamente alto")

# Crear factores basados en intervalos geométricos
shp$MEAN_cat <- cut(shp$MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = category_labels)
shp$MEAN_per_area_cat <- cut(shp$MEAN_per_area, breaks = breaks_MEAN_per_area, include.lowest = TRUE, labels = category_labels)
shp$MEAN_normalized_STD_cat <- cut(shp$MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = category_labels)

# Crear mapas temáticos basados en las categorías con etiquetas descriptivas
plot_shp_MEAN_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = MEAN_cat), color = NA) + 
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(num_categories), labels = category_labels) +
  theme_minimal() +
  labs(fill = "Categoría", title = "Mapa de MEAN Categorizado") +
  theme(legend.position = "bottom")

plot_shp_MEAN_per_area_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = MEAN_per_area_cat), color = NA) + 
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(num_categories), labels = category_labels) +
  theme_minimal() +
  labs(fill = "Categoría", title = "Mapa de MEAN por Área Categorizado") +
  theme(legend.position = "bottom")

plot_shp_MEAN_normalized_STD_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = MEAN_normalized_STD_cat), color = NA) + 
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(num_categories), labels = category_labels) +
  theme_minimal() +
  labs(fill = "Categoría", title = "Mapa de MEAN Normalizado por STD Categorizado") +
  theme(legend.position = "bottom")

# Mostrar los mapas
print(plot_shp_MEAN_cat)
print(plot_shp_MEAN_per_area_cat)
print(plot_shp_MEAN_normalized_STD_cat)
