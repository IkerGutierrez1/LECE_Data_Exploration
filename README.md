# Data exploration and filling

This repository contains a R project used for exploring and filling data. It includes the R scripts and the plots created to visualize the data.

## Data sources

The dataset contains temperature, solar radiation, humidity... for a small building in the LECE facility of Almeria Spain. The data spans from November 2023 to September 2024 with a frequency of 1 minute.

## Repository structure

### Files 

- exploration.R contains the code to read the data fromt the excel files where the data was. It also fills timestamps because when the data for all signals was missing the timestmps were also missing. A new df is created to in
  indicate if the data was real or if filled, it only contains TRUE or FALSE. After line 37 different plots are obtained.
  
- functions.R contains different functions used in exploration.R, NA_histogram and Time_series_graph create the plot indicated in the funcion name, extract_excels_to_csv extracts the data from all the excels in a folder and from all their sheets, change_frequency_15min changes the frecuency of the dataframe, it takes the last timestamp of the itnerval, fill_NA and adjutst points are functions used to fill the data.

- output folder contains some of the obtained plots.

## Relevant functions

- fill_NA is used to fill short periodos of time (a day) of missing data, it could be adapated to fill longer periods. The process to fill the data starts with calculating the mean value from the previous and next days and filling with that value, after data adjust points is called to make it fit the real data.

- adjust_points fit the data from fill_NA to the real data, for that the difference between the first and last point of the filling with the real data is calculated and 2 offsets are obtained by comparing the difference with the ones from the same hour of the previous and next days. This offsets are then added to the mean value to complete the filling.

## Filling method

The filling method is seen in the equation:
$$
x^{\prime}(t) = x_{mean}(t) + (1-w) \cdot offset_{f} + w \cdot offset_{l}
\label{eq_fill}
$$

w is a weight indicating how far from the first point the point that is beeing filled is, and the offsets are calculated with:

## Ecuaciones

### Ecuación 2
$$
offset_{f} = \frac{x(t_{\text{first}} - 24\,\text{h} - t_{\text{step}}) - x(t_{\text{first}} - 24\,\text{h})}{2} + \frac{x(t_{\text{first}} + 24\,\text{h} - t_{\text{step}}) - x(t_{\text{first}} + 24\,\text{h})}{2} - \left( x(t_{\text{first}} - t_{\text{step}}) - x(t_{\text{first}}) \right) \label{eq_offset_first}
$$

### Ecuación 3
$$
offset_{l} = \frac{x(t_{\text{last}} - 24\,\text{h} + t_{\text{step}}) - x(t_{\text{last}} - 24\,\text{h})}{2} + \frac{x(t_{\text{last}} + 24\,\text{h} + t_{\text{step}}) - x(t_{\text{last}} + 24\,\text{h})}{2} - \left( x(t_{\text{last}} + t_{\text{step}}) - x(t_{\text{last}}) \right) \label{eq_offset_last}
$$



