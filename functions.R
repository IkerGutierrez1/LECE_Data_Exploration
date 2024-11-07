NA_histogram <- function(df , path = "output/plots/", name = "na_histogram.html", linea = 500){
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Contar los NA por columna
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  # Crear un dataframe con los resultados
  na_summary <- data.frame(Columna = names(na_counts), NA_Count = na_counts)
  # Ordenar el dataframe de mayor a menor
  na_summary <- na_summary[order(-na_summary$NA_Count), ]
  # Convertir la columna a factor para mantener el orden
  na_summary$Columna <- factor(na_summary$Columna, levels = na_summary$Columna)
  # Crear un gráfico interactivo
  p <- plot_ly(na_summary, x = ~Columna, y = ~NA_Count, type = 'bar') %>%
    layout(title = "Número de NA por Columna",
           xaxis = list(title = "Columnas", tickangle = -45),
           yaxis = list(title = "Número de NA"),
           shapes = list(
             list(type = 'line',
                  x0 = -0.5, x1 = nrow(na_summary) - 0.5,  # Extensión horizontal
                  y0 = linea, y1 = linea,
                  line = list(color = 'red', width = 2, dash = 'dash'))
           ))
  
  # Guarda el gráfico como un archivo HTML primero
  saveWidget(p, paste0(path,name), selfcontained = TRUE)
}

Time_series_graph <- function(df,columnas, fecha_inicio = NULL, fecha_fin = NULL, path = "output/plots/time_series/") {
  #Funcion que permite grafica  las time series, se pueden pasar tantas columnas como se
  #quieran, se puede especificar desde que fecha y hasta cual y se pueden no especificar
  #y cogera la primera fecha del dataframe y la ultima
  
  #TODO podria estar bien añadir una linea que sea la media del dia para mas claridad
  
  # Si no se pasan fechas, seleccionar la primera y última
  if (is.null(fecha_inicio)) {
    fecha_inicio <- min(df$timestamp, na.rm = TRUE)
  }
  if (is.null(fecha_fin)) {
    fecha_fin <- max(df$timestamp, na.rm = TRUE)
  }
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  # Filtrar el dataframe por el rango de fechas
  df_filtrado <- df %>%
    filter(timestamp >= fecha_inicio & timestamp <= fecha_fin)
  
  # Iterar sobre cada columna en la lista
  for (y_col in columnas) {
    # Crear la gráfica
    p <- plot_ly(data = df_filtrado, 
                 x = ~timestamp, 
                 y = df_filtrado[[y_col]], 
                 type = "scatter", 
                 mode = "markers") %>%
      layout(title = paste("Gráfica de", y_col),
             xaxis = list(title = "Timestamps"),
             yaxis = list(title = y_col),
             showlegend = FALSE)
    
    # Guardar la gráfica en formato .html
    file_name <- paste0(path, "/", y_col, ".html")
    htmlwidgets::saveWidget(p, file_name, selfcontained = TRUE)
  }
}

get_columns_with_string <- function(dataframe, search_string,ignore_case = TRUE) {
  #FUnción para obtener los nombres de las columnas que contenga el substring deseado
  #por defecto ignora Mayusculas/minusculas pero cambiando ignore_case se puede cambiar
  # Verificar si el dataframe es válido
  if (!is.data.frame(dataframe)) {
    stop("El primer argumento debe ser un dataframe.")
  }
  
  # Obtener los nombres de las columnas que contienen el substring
  matching_columns <- names(dataframe)[grepl(search_string, names(dataframe), ignore.case = ignore_case)]
  
  return(matching_columns)
}
