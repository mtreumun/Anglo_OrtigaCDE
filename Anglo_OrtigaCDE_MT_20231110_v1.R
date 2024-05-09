#Codigo desarrollado por Matias Treumun
#Abril 2024

#Instalacion de paquetes requeridos
#install.packages("sf")
#install.packages("ggplot2")
#install.packages("gridextra")
#install.packages("classInt")
#install.packages("gridExtra")
#install.packages("FNN")
#install.packages("philentropy")
#install.packages("Rcpp")
#install.packages("transport")

# Carga las librerías
library(sf)
library(ggplot2)
library(gridExtra)
library(classInt)
library(gridExtra)
library(FNN)
library(philentropy)
library(dplyr)
library(Rcpp)
library(transport)

# Lee el archivo shapefile
shapefile_path <- "E:/ANGLO_Ortiga CDE/CAPAS/20240423/UnidadesVegetacionOrtiga_ActualizacionMayo24_FR_20240423_v1_MT_20240430_v1.shp"
shp <- st_read(shapefile_path)

# Convertir a numérico y NAs
shp$MEAN <- as.numeric(shp$MEAN)
shp$STD <- as.numeric(shp$STD)
shp$AREA <- as.numeric(shp$AREA)
shp <- shp[shp$AREA != 0, ]

# Excluir los NAs, actualiza el dataset
shp <- shp[!is.na(shp$MEAN) & !is.na(shp$STD) & !is.na(shp$AREA), ]

# Recalcula la normalización por desviacion estandar
shp$MEAN_normalized_STD <- shp$MEAN / shp$STD

# Calcular el rango y definir los intervalos
range_MEAN <- max(shp$MEAN, na.rm = TRUE) - min(shp$MEAN, na.rm = TRUE)
range_MEAN_normalized_STD <- max(shp$MEAN_normalized_STD, na.rm = TRUE) - min(shp$MEAN_normalized_STD, na.rm = TRUE)

interval_MEAN <- range_MEAN / 10
interval_MEAN_normalized_STD <- range_MEAN_normalized_STD / 10

# Definir los intervalos para las categorías (equal intervals)
breaks_MEAN <- seq(min(shp$MEAN, na.rm = TRUE), max(shp$MEAN, na.rm = TRUE), by = interval_MEAN)
breaks_MEAN_normalized_STD <- seq(min(shp$MEAN_normalized_STD, na.rm = TRUE), max(shp$MEAN_normalized_STD, na.rm = TRUE), by = interval_MEAN_normalized_STD)

#breaks_MEAN <- quantile(shp$MEAN, probs = seq(0.1, 1, by = 0.1), na.rm = TRUE)
#breaks_MEAN_normalized_STD <- quantile(shp$MEAN_normalized_STD, probs = seq(0.1, 1, by = 0.1), na.rm = TRUE)
# Ajustar los cuantiles si es necesario para incluir todos los datos
#if (min(shp$MEAN, na.rm = TRUE) < min(breaks_MEAN)) {
#  breaks_MEAN[1] <- min(shp$MEAN, na.rm = TRUE)
#}
#if (max(shp$MEAN, na.rm = TRUE) > max(breaks_MEAN)) {
#  breaks_MEAN[length(quantiles_MEAN)] <- max(shp$MEAN, na.rm = TRUE)
#}
#if (min(shp$MEAN_normalized_STD, na.rm = TRUE) < min(breaks_MEAN_normalized_STD)) {
#  breaks_MEAN_normalized_STD[1] <- min(shp$MEAN_normalized_STD, na.rm = TRUE)
#}
#if (max(shp$MEAN_normalized_STD, na.rm = TRUE) > max(breaks_MEAN_normalized_STD)) {
#  breaks_MEAN_normalized_STD[length(quantiles_MEAN_normalized_STD)] <- max(shp$MEAN_normalized_STD, na.rm = TRUE)
#}

# Crear factores basados en equal intervals con etiquetas numéricas
shp$MEAN_cat <- cut(shp$MEAN, breaks = breaks_MEAN, include.lowest = TRUE, labels = FALSE)
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
labels_MEAN_normalized_STD <- create_interval_labels(breaks_MEAN_normalized_STD)

# Mapa MEAN 2024
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

# Histograma para MEAN
hist_shp_MEAN <- ggplot(data = shp, aes(x = MEAN)) +
  geom_histogram(bins=10, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 9, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y
    #axis.title.x = element_blank(), # Ocultar el título del eje x
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI")+
  xlab("Valor NDVI") + # Cambiar la etiqueta del eje x +
  ylab("Número de unidades")   # Cambiar la etiqueta del eje y

# Combinar mapa e histograma en una sola visualización
grid.arrange(plot_shp_MEAN_cat, hist_shp_MEAN, ncol = 2, widths = c(3, 2))

# Mapa de MEAN normalizada por desviacion estandar
plot_shp_MEAN_normalized_STD_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_normalized_STD, breaks = breaks_MEAN_normalized_STD, include.lowest = TRUE, labels = labels_MEAN_normalized_STD)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_normalized_STD)), labels = labels_MEAN_normalized_STD) +
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
  labs(fill = "", title = "Mapa NDVI Norm. por STD")

# Histograma para MEAN normalizada por desviacion estandar
hist_shp_MEAN_normalized_STD <- ggplot(data = shp, aes(x = MEAN_normalized_STD)) +
  geom_histogram(bins=10, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 9, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y
    #axis.title.x = element_blank(), # Ocultar el título del eje x
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI Norm. con la desv. estándar")+
  xlab("Valor NDVI/STD") + # Cambiar la etiqueta del eje x +
  ylab("Número de unidades")   # Cambiar la etiqueta del eje y

# Combina el mapa e histograma normalizados para MEAN
grid.arrange(plot_shp_MEAN_normalized_STD_cat, hist_shp_MEAN_normalized_STD, ncol = 2, widths = c(3, 2))

####################################################################
#MEAN_1 año 2023 (anterior)

shp$MEAN_1 <- as.numeric(shp$MEAN_1)
shp$STD <- as.numeric(shp$STD)
shp$AREA <- as.numeric(shp$AREA)
shp <- shp[shp$AREA != 0, ]

# Excluir los NAs, actualiza el dataset
shp <- shp[!is.na(shp$MEAN_1) & !is.na(shp$STD) & !is.na(shp$AREA), ]

# Recalcula la normalización por desviacion estandar
shp$MEAN_1_normalized_STD <- shp$MEAN_1 / shp$STD

# Calcular el rango y definir los intervalos
range_MEAN_1 <- max(shp$MEAN_1, na.rm = TRUE) - min(shp$MEAN_1, na.rm = TRUE)
range_MEAN_1_normalized_STD <- max(shp$MEAN_1_normalized_STD, na.rm = TRUE) - min(shp$MEAN_1_normalized_STD, na.rm = TRUE)

interval_MEAN_1 <- range_MEAN_1 / 10
interval_MEAN_1_normalized_STD <- range_MEAN_1_normalized_STD / 10

# Definir los intervalos para las categorías (equal intervals)
breaks_MEAN_1 <- seq(min(shp$MEAN_1, na.rm = TRUE), max(shp$MEAN_1, na.rm = TRUE), by = interval_MEAN_1)
breaks_MEAN_1_normalized_STD <- seq(min(shp$MEAN_1_normalized_STD, na.rm = TRUE), max(shp$MEAN_1_normalized_STD, na.rm = TRUE), by = interval_MEAN_1_normalized_STD)

# Crear factores basados en equal intervals con etiquetas numéricas
shp$MEAN_1_cat <- cut(shp$MEAN_1, breaks = breaks_MEAN_1, include.lowest = TRUE, labels = FALSE)
shp$MEAN_1_normalized_STD_cat <- cut(shp$MEAN_1_normalized_STD, breaks = breaks_MEAN_1_normalized_STD, include.lowest = TRUE, labels = FALSE)

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
labels_MEAN_1 <- create_interval_labels(breaks_MEAN_1)
labels_MEAN_1_normalized_STD <- create_interval_labels(breaks_MEAN_1_normalized_STD)

# Mapa MEAN_1 2024
plot_shp_MEAN_1_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_1, breaks = breaks_MEAN_1, include.lowest = TRUE, labels = labels_MEAN_1)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_1)), labels = labels_MEAN_1) +
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


# Histograma para MEAN_1
hist_shp_MEAN_1 <- ggplot(data = shp, aes(x = MEAN_1)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 9, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y
    #axis.title.x = element_blank(), # Ocultar el título del eje x
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI")+
  xlab("Valor NDVI") + # Cambiar la etiqueta del eje x +
  ylab("Número de unidades")   # Cambiar la etiqueta del eje y

# Combinar mapa e histograma en una sola visualización
grid.arrange(plot_shp_MEAN_1_cat, hist_shp_MEAN_1, ncol = 2, widths = c(3, 2))

# Mapa de MEAN_1 normalizada por desviacion estandar
plot_shp_MEAN_1_normalized_STD_cat <- ggplot(data = shp) +
  geom_sf(aes(fill = cut(MEAN_1_normalized_STD, breaks = breaks_MEAN_1_normalized_STD, include.lowest = TRUE, labels = labels_MEAN_1_normalized_STD)), color = NA) +
  scale_fill_manual(values = colorRampPalette(c("green", "red"))(length(labels_MEAN_1_normalized_STD)), labels = labels_MEAN_1_normalized_STD) +
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
  labs(fill = "", title = "Mapa NDVI Norm. por STD")

# Histograma para MEAN_1 normalizada por desviacion estandar
hist_shp_MEAN_1_normalized_STD <- ggplot(data = shp, aes(x = MEAN_1_normalized_STD)) +
  geom_histogram(bins = 10, fill = "blue", color = "black") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
    axis.text.x = element_text(size = 9, angle = 90, vjust = 0.5),
    axis.text.y = element_text(size = 9, angle = 90, vjust = 0.5),
    #axis.text.y = element_blank(), # Ocultar las etiquetas del eje y
    #axis.title.x = element_blank(), # Ocultar el título del eje x
    plot.margin = margin(5, 5, 5, 5)
  ) +
  ggtitle("Histograma NDVI Norm. con la desv. estándar")+
  xlab("Valor NDVI/STD") + # Cambiar la etiqueta del eje x +
  ylab("Número de unidades")   # Cambiar la etiqueta del eje y

# Combina el mapa e histograma normalizados para MEAN_1
grid.arrange(plot_shp_MEAN_1_normalized_STD_cat, hist_shp_MEAN_1_normalized_STD, ncol = 2, widths = c(3, 2))


##########################################################################
#Aplicaciones de test estadisticos para comparar histogramas

#########################################################
#APLICACION WASSERSTEIN DISTANCE

# Normalize histograms to have equal mass if not already
total_mass_MEAN <- sum(hist_MEAN$counts)
total_mass_MEAN_1 <- sum(hist_MEAN_1$counts)
normalized_hist_MEAN <- hist_MEAN$counts / total_mass_MEAN
normalized_hist_MEAN_1 <- hist_MEAN_1$counts / total_mass_MEAN_1

plot(normalized_hist_MEAN)
plot(normalized_hist_MEAN_1)

# Assuming histograms are equally spaced and indices represent their positions
bin_count <- length(normalized_hist_MEAN)
cost_matrix <- matrix(0, nrow = bin_count, ncol = bin_count)
for (i in 1:bin_count) {
  for (j in 1:bin_count) {
    cost_matrix[i, j] <- abs(i - j)  # Simple cost: distance between indices
  }
}

# Compute Wasserstein distance
wasserstein_distance <- wasserstein(normalized_hist_MEAN, normalized_hist_MEAN_1, costm = cost_matrix)
print(wasserstein_distance)

##################################
#APLICACION DE KULLBACK-LEIBLER

# Calcula histogramas y normaliza
hist_MEAN <- hist(shp$MEAN, breaks=breaks_MEAN, plot=FALSE)
plot(hist_MEAN)
densities_MEAN <- hist_MEAN$counts / sum(hist_MEAN$counts)
plot(densities_MEAN)

hist_MEAN_1 <- hist(shp$MEAN_1, breaks=breaks_MEAN_1, plot=FALSE)
plot(hist_MEAN_1)
densities_MEAN_1 <- hist_MEAN_1$counts / sum(hist_MEAN_1$counts)
plot(densities_MEAN_1)

# Asegurarse de que todas las entradas son mayores que cero para evitar logaritmos de cero
densities_MEAN[densities_MEAN <= 0] <- 1e-10
densities_MEAN_1[densities_MEAN_1 <= 0] <- 1e-10

# Calcular la divergencia KL manualmente
kl_divergence <- sum(densities_MEAN * log(densities_MEAN / densities_MEAN_1))

# Imprimir la divergencia KL
print(paste("Divergencia KL entre 2024 (MEAN) y 2023 (MEAN_1):", kl_divergence))

###############################################################
#METODO Kolmogorov-Smirnov

# Accede a las cuentas de cada histograma
counts_MEAN <- hist_MEAN$counts
counts_MEAN_1 <- hist_MEAN_1$counts

# Ambos deben tener el mismo número de bins
if (length(counts_MEAN) == length(counts_MEAN_1)) {
  # Calcular la diferencia absoluta entre las cuentas
  hist_diff <- abs(counts_MEAN - counts_MEAN_1)
  
  # Visualizar la diferencia
  barplot(hist_diff, main = "Diferencia de Histogramas NDVI", xlab = "Bins de NDVI", ylab = "Diferencia Absoluta")
} else {
  print("Los histogramas no tienen el mismo número de bins")
}

# Código existente para calcular histogramas y normalizarlos
hist_MEAN <- hist(shp$MEAN, plot = FALSE)
hist_MEAN_1 <- hist(shp$MEAN_1, plot = FALSE)
densities_MEAN <- hist_MEAN$counts / sum(hist_MEAN$counts)
densities_MEAN_1 <- hist_MEAN_1$counts / sum(hist_MEAN_1$counts)

# Asegúrate de que ambos histogramas tienen el mismo número de bins
if (length(densities_MEAN) == length(densities_MEAN_1)) {
  # Realiza la prueba de Kolmogorov-Smirnov
  ks_test_result <- ks.test(densities_MEAN, densities_MEAN_1)
  
  # Imprime los resultados de la prueba
  print(ks_test_result)
} else {
  print("Error: Los histogramas no tienen el mismo número de bins")
}


########################################################
#TRABAJO SOBRE SHP

# Agregar columnas de deciles al DataFrame
shp <- shp %>%
  mutate(
    Decil2024 = ntile(MEAN, 10),
    Decil2023 = ntile(MEAN_1, 10),
    Dif24_23=Decil2024-Decil2023
  )


head(shp)

shp_reducido <- shp %>%
  select(ID_Polygon,Veg_l,MEAN, MEAN_1, Decil2024, Decil2023, Dif24_23)

# Intentar escribir el shapefile nuevamente
output_path <- "E:/ANGLO_Ortiga CDE/CAPAS/20240423/UnidadesVegetacionOrtiga_ActualizacionMayo24_FR_20240423_v1_MT_20240507_v8.shp"
st_write(shp_reducido, output_path)


