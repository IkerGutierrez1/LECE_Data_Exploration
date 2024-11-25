library(readxl)
library(dplyr)
library(purrr)
library(plotly)
library(tidyr)
library(htmlwidgets)
library(lubridate)

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

extract_excels_to_csv <- function(excel_path = "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640",
                                  save_dir = "input/" ,
                                  save_name = "data.csv" ){
  #Function to extract data from all excels in excel_path, it creates a .csv with all the data
  xlsx_names <- list.files(path = excel_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  df <- data.frame()
  for (excel in xlsx_names){
    
    df_sheets <- excel_sheets(excel)
    print(paste("Procesando excel:", excel))
    
    df_test <- data.frame()
    for (sheet in df_sheets) {
      
      
      if (grepl("Datos", sheet)) {
        temp_df <- read_excel(excel, sheet = sheet)
        temp_df <- temp_df %>%
          select(where(~ !all(is.na(.)))) #Esto puede ser lo que da problemas
        
        if (nrow(df_test) == 0) {
          df_test <- temp_df
        } else {
          # Si df_test ya tiene datos, los unimos por la columna Fecha
          df_test <- full_join(df_test, temp_df, by = "Fecha")
        }
      }
    }
    if (nrow(df) == 0) {
      df <- df_test
    }
    else{
      df <- bind_rows(df, df_test)}
    df <- df[!is.na(df$Fecha), ]
  }
  
  #Delete the auxiliar colum Tp and Met_Tpir_01 and created 81 colum, it is created because on of the sheets has columns which dont have values
  #but are taken into account, all of them are filter before except 81
  df <- df[, !(names(df) %in% c("Tp","Met_Idir_01", "...81"))]
  
  df <- df %>%
    rename(timestamp = Fecha)
  
  df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S")
  
  write.csv(df, file = paste0(save_dir,save_name), row.names = FALSE)
  print("Data was saved")
  
}

<<<<<<< HEAD
change_frequency_15min <- function(df){
  #Change frecuency
  #Create colum interval indicating start of the interval, timestamp has the mean of timestamps
  #if the the measures should only be befoer the specify timetamops change timestamp by start_interbal + interval
  time_aggregation <- "15 minutes"
  df <- df %>%
    mutate(timestamp = ymd_hms(timestamp)) %>%
    mutate(interval = floor_date(timestamp, unit = time_aggregation )) %>%
    group_by(interval) %>%
    summarise(across(everything(), \(x) mean(x, na.rm = TRUE))) %>%
    mutate(timestamp = interval + minutes(15))
  
  return (df)
  
}


fill_NA <- function(df, df_original_data, cols){
  #df_original data is a df with same amount of data with True if the data is not NA and False if not
  for (col in cols) {  # Reemplaza con el nombre de tus columnas con NA
    for (i in which(is.na(df[[col]]))) {  # Recorrer solo las filas con NA en la columna 'col'
      # Obtener el timestamp del valor NA
      timestamp_na <- df$timestamp[i]
      
      # Encontrar el valor en la columna correspondiente del día anterior (24 horas antes)
      timestamp_anterior <- timestamp_na - 24*60*60  # Restamos 24 horas
      valor_anterior <- df[[col]][df$timestamp == timestamp_anterior]
      
      
      # Encontrar el valor en la columna correspondiente del día posterior (24 horas después)
      timestamp_posterior <- timestamp_na + 24*60*60  # Sumamos 24 horas
      valor_posterior <- df[[col]][df$timestamp == timestamp_posterior]
      
      #Check for first or last day
      timestamp_na_first <- df$timestamp[i] - 15*60
      first <- df_original_data[[col]][df_original_data$timestamp == timestamp_na_first]
      if(first[1] == TRUE){
        #Calc dif for previous day
        timestamp_na_first_prev <- timestamp_na_first - 24*60*60
        timestamp_na_first_next <- timestamp_na_first + 24*60*60

        
        diff_first_prev <- df[[col]][df$timestamp == timestamp_na_first_prev] - valor_anterior #Valor bien calculado compararlo de alguna forma con la dif del punto de fill
        diff_first_next <- df[[col]][df$timestamp == timestamp_na_first_next] - valor_posterior

        diff_first <- (diff_first_prev+diff_first_next)/2
        first_timestamp <- timestamp_na
        
      }
      
      timestamp_na_post <- df$timestamp[i] + 15*60
      last <- df_original_data[[col]][df_original_data$timestamp == timestamp_na_post]
      if(last[1] == TRUE){
        timestamp_na_last_prev <- timestamp_na_post - 24*60*60
        timestamp_na_last_next <- timestamp_na_post + 24*60*60

        diff_last_prev <- df[[col]][df$timestamp == timestamp_na_last_prev] - valor_anterior
        diff_last_next <- df[[col]][df$timestamp == timestamp_na_last_next] - valor_posterior #Valor bien calculado compararlo de alguna forma con la dif del punto de fill
        
        diff_last <- (diff_last_prev+diff_last_next)/2
        last_timestamp <- timestamp_na
      }
      

      
      if (length(valor_anterior) > 1){
        valor_anterior = valor_anterior[1] #No clue why it takes more than 1 value and the second is NA
      }
      if (length(valor_posterior) > 1){
        valor_posterior = valor_posterior[1] #No clue why it takes more than 1 value and the second is NA
      }
      
      # Si ambos valores existen, calculamos la media
      if (!is.na(valor_anterior) && !is.na(valor_posterior)) {
        df[[col]][i] <- mean(c(valor_anterior, valor_posterior), na.rm = TRUE)
      }
      # Si solo hay un valor, se asigna ese valor (puedes modificar esto si prefieres otra lógica)
      else if (!is.na(valor_anterior)) {
        df[[col]][i] <- valor_anterior
      } else if (!is.na(valor_posterior)) {
        df[[col]][i] <- valor_posterior
      }
      
        
      
    }
    df <- adjust_points(df, col, first_timestamp, last_timestamp, diff_first, diff_last)
  }
  
  
  return (df)
}


adjust_points <- function(df, col, first_timestamp, last_timestamp, diff_first, diff_last){
  
  diff_first_fill <- df[[col]][df$timestamp == (first_timestamp - 15*60)] - df[[col]][df$timestamp == first_timestamp]
  diff_last_fill <- df[[col]][df$timestamp == (last_timestamp + 15*60)] - df[[col]][df$timestamp == (last_timestamp)]
  
  print("Fill diffs")
  print(diff_first_fill)
  print(diff_last_fill)
  
  first_offset <- diff_first - diff_first_fill
  last_offset <- diff_last - diff_last_fill
  
  print("Offsets")
  print(first_offset)
  print(last_offset)
  
  interval_minutes <- as.numeric(last_timestamp-first_timestamp, units = "mins")
  
  df_filtered <- df[df$timestamp >= first_timestamp & df$timestamp <= last_timestamp, ]
  
  indexes <- which(df$timestamp >= first_timestamp & df$timestamp <= last_timestamp)
  
  for (i in indexes) {
    # Accede a cada fila usando df_filtered[i, ]
    
    timestamp_na <- df$timestamp[i]
    
    f = as.numeric((timestamp_na - first_timestamp), units = "mins")/ interval_minutes
    
    df[[col]][df$timestamp == timestamp_na] <- df[[col]][df$timestamp == timestamp_na] - (1-f)*first_offset - f*last_offset

  }
  
  return (df)
=======

NA_points_col_graph <- function(df, col_na, col_timestamp = "timestamp", path = "output/plots/NA_time_series/"){
  #Fumcion que hace una grafica de los puntos donde falten valores de una columna frente a los tiempos
  #Verificar que existen ambas columnas
  if (!(col_timestamp %in% names(df))) {
    stop("La columna de timestamp no está en df")
  }
  
  # Asegurarse de que col_na es un vector de caracteres
  if (is.list(col_na)) {
    col_na <- unlist(col_na)  # Convierte a vector si es lista
  }
  
  
  # Asegúrate de que el directorio existe
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
  
  for (col in col_na) {
    
    timestamps <- df[[col_timestamp]]
    missing_data <- ifelse(is.na(df[[col]]), 1, NA)
    df_aux <- data.frame(timestamp = timestamps, missing_data = missing_data)
    
    p <- plot_ly(data = df_aux, x = ~timestamps, y = ~missing_data, type = "scatter", mode = "markers") %>%
      layout(title = paste("Missing Data for", col),
             xaxis = list(title = "Timestamps"),
             yaxis = list(title = "Missing Data"),
             yaxis = list(tickvals = c(0, 1), ticktext = c("No", "Sí")),
             showlegend = FALSE
      )
    
    full_path <- paste0(path, paste0(col, ".html"))
    saveWidget(p, full_path, selfcontained = TRUE)
  }
>>>>>>> 870f9f0a5f97f8f6e237187371184d273dedbc07
}
