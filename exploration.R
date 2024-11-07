source("functions.R")
#Data from excel to csv
path <- "input/wetransfer_meteo-lece_01_2024-xlsx_2024-11-04_1640"
extract_excels_to_csv(excel_path = path)
#Load data
df <- read.csv("input/data.csv")
df$timestamp <- ifelse(grepl(" ", df$timestamp), df$timestamp, paste(df$timestamp, "00:00:00"))

#RDS files

#Plots
Tae_c <-get_columns_with_string(df,"Tae")

Time_series_graph(df,columnas = Tae_c)


