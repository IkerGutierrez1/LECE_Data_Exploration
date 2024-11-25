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

#Read
df <- readRDS(file = "data", refhook = NULL)

#Fill missing timestaps 
df <- df %>%
  mutate(timestamp = ymd_hms(timestamp)) %>%  
  complete(timestamp = seq(min(timestamp), max(timestamp), by = "1 min")) %>%
  mutate(across(everything(), ~ if_else(is.na(timestamp), NA, .)))

#Remove aux col if present
df <- df %>%
  select(-Met_Tpir_01)

df <- change_frequency_15min(df)

#Data frame to be able to differentiate between fill in data and original data
df_original_data <- df %>%
  mutate(across(
    .cols = -c(timestamp, interval),  
    .fns = ~ !is.na(.)
  ))


#Plots
df_filtered <- df %>%
  filter(timestamp >= ymd_hms("2024-07-22 00:00:00") & timestamp <= ymd_hms("2024-07-26 23:59:59"))

Tae_c <-get_columns_with_string(df,"Tae")

Mz_0_east <- c("Mz_Ø_east")

T_surr <- c("Tsurr")

Mz_Ts <- c("Mz_Ts_door")

cols = c("Mz_Ts_door","Mz_Ø_east","Tsurr")

Time_series_graph(df_filtered,columnas = cols)

df_t <- fill_NA(df_filtered, df_original_data, cols = Mz_Ts)

Time_series_graph(df_t,columnas = Mz_Ts)


#Modificar df para pruebas sobre datos que si tenemos 
filter_from <- as.POSIXct("2024-07-15 14:00:00", format = "%Y-%m-%d %H:%M:%S")
filter_to <- as.POSIXct("2024-07-23 16:00:00", format = "%Y-%m-%d %H:%M:%S")

df_test <- df %>%
  filter(timestamp >= filter_from & timestamp <= filter_to)

na_from <- as.POSIXct("2024-07-19 22:30:00", format = "%Y-%m-%d %H:%M:%S")
na_to <- as.POSIXct("2024-07-20 18:45:00", format = "%Y-%m-%d %H:%M:%S")

rows_in_range <- df_test$timestamp >= na_from & df_test$timestamp <= na_to
df_test_NA <- df_test
df_test_NA[rows_in_range, setdiff(names(df_test), "timestamp")] <- NA

df_test_original_data <- df_test_NA %>%
  mutate(across(
    .cols = -c(timestamp, interval),  
    .fns = ~ !is.na(.)
  ))

df_test_t <- fill_NA(df_test_NA, df_test_original_data, cols = Mz_0_east)
Time_series_graph(df_test_t,columnas = Mz_Ts)
Time_series_graph(df_tes_NA,columnas = Mz_Ts)
Time_series_graph(df_test,columnas = Mz_Ts)

column_to_plot <- Mz_0_east
p <- ggplot()+
  geom_point(data = df_test_t, aes( x = timestamp, y = !!sym(column_to_plot), color = "Fill")) + 
  geom_point(data = df_test, aes( x = timestamp, y = !!sym(column_to_plot), color = "Real")) 
p


