library(readxl)
library(dplyr)
library(purrr)
library(plotly)
library(htmlwidgets)

path <- "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640"

xlsx_names <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)

df <- data.frame()
for (excel in xlsx_names){
  
  df_sheets <- excel_sheets(excel)
  print(paste("Procesando excel:", excel))
  
  df_test <- data.frame()
  for (sheet in df_sheets) {
    
    
    if (grepl("Datos", sheet)) {
      temp_df <- read_excel(excel, sheet = sheet)
      temp_df <- temp_df %>%
        select(where(~ !all(is.na(.))))
      
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

write.csv(df, file = "input/data.csv", row.names = FALSE)


#Plots
Tae_c <-get_columns_with_string(df,"Tae")

Time_series_graph(df,columnas = Tae_c)


