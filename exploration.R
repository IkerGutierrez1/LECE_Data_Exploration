library(readxl)
library(dplyr)
library(purrr)

path <- "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640"

xlsx_names <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)

df <- xlsx_names %>%
  map_dfr(~ read_excel(.))

df_sheets <- excel_sheets(xlsx_names[1])


df_test <- data.frame()
#Fecha no incluye año por lo que se repiten filas de Fecha?
for (excel in xlsx_names){
  
  df_sheets <- excel_sheets(excel)
  print(paste("Procesando excel:", excel))
  
  for (sheet in df_sheets) {
    print(paste("Procesando hoja:", sheet))
    
    if (grepl("Datos", sheet)) {
      temp_df <- read_excel(excel, sheet = sheet)
      temp_df <- temp_df %>%
        select(where(~ !all(is.na(.))))
      
      if (nrow(df_test) == 0) {
        df_test <- temp_df
      } else {
        # Si df_test ya tiene datos, los unimos por la columna Fecha
        #df_test <- full_join(df_test, temp_df, by = "Fecha")
        df_test <- bind_rows(df_test, temp_df)
      }
  }
  }}


NA_count <- sapply(df, function(x) sum(is.na(x)))
