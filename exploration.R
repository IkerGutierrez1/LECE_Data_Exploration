source("functions.R")
#Data from excel to csv
path <- "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640"
extract_excels_to_csv(excel_path = path)
#Load data
df <- read.csv("input/data.csv")
df$timestamp <- ifelse(grepl(" ", df$timestamp), df$timestamp, paste(df$timestamp, "00:00:00"))

#RDS files
saveRDS(df, file = "data", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)
df <- readRDS(file = "data", refhook = NULL)


#Fill missing timestapps 
df <- df %>%
  # Paso 1: Crear una secuencia de timestamps con una frecuencia de 1 minuto
  mutate(timestamp = ymd_hms(timestamp)) %>%  # Asegurarnos de que timestamp sea de tipo POSIXct
  complete(timestamp = seq(min(timestamp), max(timestamp), by = "1 min")) %>%
  # Paso 2: Rellenar los valores faltantes con NA en todas las columnas, excepto timestamp
  mutate(across(everything(), ~ if_else(is.na(timestamp), NA, .)))

df <- df %>%
  select(-Met_Tpir_01)

#Change frecuency
df_5min <- df %>%
  mutate(timestamp = ymd_hms(timestamp))  
#Create colum interval indicating start of the mean, timestamp has the mean of timestamps
df_5min <- df_5min %>%
  mutate(interval = floor_date(timestamp, unit = "5 minutes")) %>%
  group_by(interval) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))




#Plots
Tae_c <-get_columns_with_string(df,"Tae")

Time_series_graph(df,columnas = Tae_c)



df_compare <- df[, c("timestamp", "Mz_Ø_east")]
df_compare_5min <- df_5min[, c("timestamp", "Mz_Ø_east")]


df_compare <- df_compare[apply(is.na(df_compare), 1, any), ]
df_compare_5min <- df_compare_5min[apply(is.na(df_compare_5min), 1, any), ]


NA_histogram(df_5min,name = "na_histogram_5min.html")
NA_points_col_graph(df_5min,"Mz_Ø_east")
