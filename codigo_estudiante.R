# 1. Media del Peso por Grupo (ignorar NA)
calcular_media_por_grupo <- function() {
  mean_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, mean, na.rm = TRUE) # Insertar código aquí
  return(mean_vals)
}

# 2. Mediana del Peso por Grupo (ignorar NA)
calcular_mediana_por_grupo <- function() {
  median_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, median, na.rm = TRUE) # Insertar código aquí
  return(median_vals)
}

# 3. Varianza del Peso por Grupo (ignorar NA)
calcular_varianza_por_grupo <- function() {
  var_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, var, na.rm = TRUE) # Insertar código aquí
  return(var_vals)
}

# 4. Desviación Estándar del Peso por Grupo (ignorar NA)
calcular_desviacion_por_grupo <- function() {
  sd_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, sd, na.rm = TRUE) # Insertar código aquí
  return(sd_vals)
}

# 5. Rango Intercuartil (IQR) del Peso por Grupo (ignorar NA)
calcular_rango_intercuartil_por_grupo <- function() {
  iqr_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, IQR, na.rm = TRUE) # Insertar código aquí
  return(iqr_vals)
}

# 6. Percentil 90 del Peso por Grupo (ignorar NA)
calcular_percentil_90_por_grupo <- function() {
  perc_90_vals <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, quantile, 0.9, na.rm = TRUE) # Insertar código aquí
  return(perc_90_vals)
}

# Funciones de Visualización para PlantGrowth_na por grupo

# 7. Histograma del Peso por Grupo (ignorar NA) - Devuelve conteos de frecuencias por grupo
crear_histograma_por_grupo <- function() {
  grupos <- unique(PlantGrowth_na$group)
  resultados <- list()
  for (grupo in grupos) {
    hist_data <- hist(PlantGrowth_na$weight[PlantGrowth_na$group == grupo & !is.na(PlantGrowth_na$weight)], # Insertar código aquí
                      plot = TRUE, main = "Histograma", xlab = grupo)
    resultados[[grupo]] <- hist_data$counts # conteos de frecuencias en cada intervalo
  }
  return(resultados)
}

# 8. Diagrama de Caja del Peso por Grupo (ignorar NA) - Devuelve estadísticas del boxplot por grupo
crear_boxplot_por_grupo <- function() {
  box_data <- boxplot(weight ~ group, data = PlantGrowth_na, plot = TRUE) # Insertar código aquí
  return(box_data$stats) # retorna estadísticas de los boxplots (min, Q1, mediana, Q3, max)
}

# 9. Diagrama de Dispersión de Peso por Grupo (con ruido aleatorio, ignorar NA) - Devuelve número de puntos por grupo
crear_dispersion_por_grupo <- function() {
  grupos <- unique(PlantGrowth_na$group)
  puntos_por_grupo <- sapply(grupos, function(grupo) {
    sum(!is.na(PlantGrowth_na$weight[PlantGrowth_na$group == grupo])) # cuenta puntos no NA # Insertar código aquí
  })
  return(puntos_por_grupo)
}

# 10. Gráfico de Barras de la Media del Peso por Grupo (ignorar NA) - Devuelve medias por grupo
crear_grafico_barras_por_grupo <- function() {
  medias_por_grupo <- tapply(PlantGrowth_na$weight, PlantGrowth_na$group, mean, na.rm = TRUE) # Insertar código aquí
  barplot(medias_por_grupo, main = "Media del Peso por Grupo con NA", col = "orange", 
          xlab = "Grupo", ylab = "Peso Medio")
  return(medias_por_grupo)
}

