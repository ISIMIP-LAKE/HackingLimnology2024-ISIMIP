#1 July 2024
#Author: Daniel Mercado-Bettín

# Load necessary libraries
#install.packages("terra")
#install.packages("ggplot2")
library(terra)
library(ggplot2)

setwd("/HackingLimnology2024-ISIMIP/global-local_data/")

##····································DOWNLOAD DATA data.isimip.org portal································
##········································································································
##········································································································
#Previouly you should have had donwloaded the files need as instructed in the workshop
#The data is download already as input in the repository

##····································GLOBAL SIMULATION, VISUALIZATION····································
##········································································································
##········································································································
  
  
# 1. Exploring NetCDF data - GLOBAL LAKES
#Example: Scenario: historical, time range: 1991-2000, region: Africa 
# Open the NetCDF file
nc_file <- rast("input/gotm_gfdl-esm4_w5e5_historical_histsoc_default_surftemp_global_daily_1991_2000_Africa.nc")

# Explore the contents of the NetCDF file
print(nc_file)

#Get coordinates of the NetCDF file
#coordinates <- crds(nc_file)

# Get the time dimension 
time_info <- time(nc_file)

# Convert the time_info to Date format
time_info <- as.Date(time_info, format="%Y-%m-%d")

# Get the extent of the NetCDF file
ext <- ext(nc_file)
print(ext)

# 2. Plot a lake variable for 1 particular time for a whole region - GLOBAL LAKES

specific_time_index <- 150  # Change this to select a different time point
temp_at_time <- nc_file[[specific_time_index]]

# Convert the data to a data frame for plotting
temp_df <- as.data.frame(temp_at_time-273.15, xy = TRUE, na.rm = TRUE)
colnames(temp_df) <- c("lon", "lat", "Degree_C")

# Plot the data using ggplot2
ggplot(temp_df, aes(x = lon, y = lat, fill = Degree_C)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = paste("Surface Temperature at Time", specific_time_index),
       x = "Longitude",
       y = "Latitude")

# 3. Plot a lake variable for the average over time for a whole region - GLOBAL LAKES

# Calculate the mean across the time dimension
mean_temp <- app(nc_file, mean)

# Convert the data to a data frame for plotting
mean_temp_df <- as.data.frame(mean_temp-273.15, xy = TRUE, na.rm = TRUE)
colnames(mean_temp_df) <- c("lon", "lat", "Degree_C")

# Plot the data using ggplot2
ggplot(mean_temp_df, aes(x = lon, y = lat, fill = Degree_C)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Mean Surface Temperature",
       x = "Longitude",
       y = "Latitude")

# (OPTIONAL) 4. Animation for the first X days - GLOBAL LAKES
#install.packages("gganimate")
#install.packages("transformr")
#install.packages("gifski")
library(gganimate)
library(transformr)
library(gifski)

# Convert the data for the first 30 time steps to a long format data frame for plotting
days <- 30
temp_df_list <- list()
num_time_steps <- min(nlyr(nc_file), days)  # Limit to first 30 time steps

for (i in 1:num_time_steps) {
  temp_at_time <- as.data.frame(nc_file[[i]], xy = TRUE, na.rm = TRUE)-273.15
  temp_at_time$time <- i
  colnames(temp_at_time) <- c("lon", "lat", "Degree_C", "time")
  temp_df_list[[i]] <- temp_at_time
}

temp_df_animate <- do.call(rbind, temp_df_list)

# Create the animation using ggplot2 and gganimate
p <- ggplot(temp_df_animate, aes(x = lon, y = lat, fill = Degree_C)) +
  geom_raster() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Surface Temperature over Time", x = "Longitude", y = "Latitude") +
  transition_time(time) +
  ease_aes('linear')

animate(p, nframes = num_time_steps, fps = 10, width = 800, height = 600, renderer = gifski_renderer())

# Save the animation to a file
anim_save("surftemp_animation.gif", animation = last_animation())

#····································SELECT ONE POINT, COMPARISON WITH LOCAL LAKES·····················...
##········································································································
##········································································································
# WARNING: THIS IS NOT AN USUAL WAY OF USING GLOBAL DATA, BUT WE WILL DO IT FOR THE WORKSHOP

# 1. Plot time series for one pixel (example Lake Kivu) - GLOBAL LAKES

# Define the specific latitude and longitude point
# Lake Kivu: 2.0448° S, 29.1856° E
specific_lat <- -2.04 #-2.04  # Replace with your latitude Spain: 39; Turkey: 39 SA: -29
specific_lon <- 29.18 #29.18  # Replace with your longitude Spain: -5 Turkey: 31 SA: 25
ggplot() +
  geom_raster(data = mean_temp_df, aes(x = lon, y = lat, fill = Degree_C)) +
  scale_fill_viridis_c() +
  geom_point(aes(x = specific_lon, y = specific_lat), color = "red", size = 2) +
  #geom_point(aes(x = -5, y = 39), color = "red", size = 2) +
  #geom_point(aes(x = 31, y = 39), color = "red", size = 2) +
  #geom_point(aes(x = 25, y = -29), color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Mean Temperature Map with Specific Point",
       x = "Longitude",
       y = "Latitude",
       fill = "Degree_C")

# Extract the temperature data for the specific point
temperature_data <- as.numeric(extract(nc_file, cbind(specific_lon, specific_lat)))

# Create a data frame with time and temperature
temperature_data_global<- data.frame(time = time_info, temperature = temperature_data-273.15)

# Plot the time series 
global_plot <- ggplot() +
  geom_line(data = temperature_data_global, aes(x = time, y = temperature), color = "blue") +
  scale_y_continuous(limits = c(20, 28)) +
  labs(
    y = "Surface temp (ºC)",
    x = "Time (days)",
    title = paste("Surface Temperature at (lat:", specific_lat, ", lon:", specific_lon, ")", sep = ""),
  ) +
  theme_minimal()
global_plot

# 2. Opening calibrated simulation of Lake Kivu - (LOCAL LAKES)

# Open the NetCDF file
nc_file_local <- rast("input/gotm-ler_gfdl-esm4_w5e5_historical_2015soc_default_surftemp_kivu_daily_1850_2014.nc")
#neglect warning here!

# Explore the contents of the NetCDF file
print(nc_file_local)

# Get the time dimension 
time_info_local <- time(nc_file_local)

# Convert the time_info to Date format
time_info_local <- as.Date(time_info, format="%Y-%m-%d")

# Extract data in the same period as Global data 
years <- format(time_info, "%Y")  # Extract year and month in "YYYY-MM" format
unique_months <- unique(months)
temp_same_period <- nc_file_local[[years %in% 1991:2000]]
time_info_local <- time_info_local[years %in% 1991:2000]

# Convert the data to a data frame for plotting
temp_time_df_local <- as.data.frame(temp_same_period-273.15, xy = TRUE, na.rm = TRUE)
temp_time_df_local <- temp_time_df_local[3:length(temp_time_df_local)] #neglect two first points, they are not temperatures 

# Extract the temperature data local lake
temperature_data_local <- as.numeric(temp_time_df_local)

temperature_data_local <- data.frame(time=time_info_local, temperature=temperature_data_local)

global_local_plot <- global_plot +
  geom_line(data = temperature_data_local, aes(x = time, y = temperature), color = "black") +
  #scale_y_continuous(limits = c(20, 28)) +
  labs(
    y = "Surface temp (ºC)",
    x = "Time (days)") +
  theme_minimal()

global_local_plot




