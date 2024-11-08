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

#Change frecuency
df_5min <- df %>%
  mutate(timestamp = ymd_hms(timestamp))  
#Create colum interval indicating start of the mean, timestamp has the mean of timestamps
df_5min <- df_5min %>%
  mutate(interval = floor_date(timestamp, unit = "5 minutes")) %>%
  group_by(intervalo) %>%
  summarise(across(everything(), \(x) mean(x, na.rm = TRUE)))

#Plots
Tae_c <-get_columns_with_string(df,"Tae")

Time_series_graph(df,columnas = Tae_c)


