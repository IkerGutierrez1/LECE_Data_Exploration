library(readxl)
library(dplyr)
library(purrr)

path <- "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640"

xlsx_names <- list.files(path = path, pattern = "\\.xlsx$", full.names = TRUE)

df <- xlsx_names %>%
  map_dfr(~ read_excel(.))

df_sheets <- excel_sheets(xlsx_names[1])
tibble <- lapply(df_sheets, function(x) read_excel(xlsx_names[1], sheet = x))

tibble <- lapply(df_sheets, function(sheet_name) {
  read_excel(xlsx_names[1], sheet = sheet_name)
})

i <- 0
for (sheet in df_sheets){
  print(sheet)
  if (grepl("Graphic", sheet)){
    print("skip")
  }
  else 
    if (grepl("Datos", sheet){
    df_test <- read_excel(xlsx_names[1], sheet = sheet)
    }
  }

NA_count <- sapply(df, function(x) sum(is.na(x)))
