## HACKING LIMNOLOGY 2024
# Climate Change - ISIMIP
# author: Robert Ladwig

library(ncdf4)
library(tidyverse)
library(rLakeAnalyzer)
library(reshape2)
library(patchwork)

setwd('C:/Users/au740615/Documents/Projects/ISIMIP-LAKE/HackingLimnology-2024/robert_HL/robert_HL')

# load local ISIMIP output for Lake Mendota, WI, "the best studied lake in the world"
nc <- nc_open(filename = 'gotm-ler_gfdl-esm4_w5e5_ssp585_2015soc_default_watertemp_mendota_daily_2015_2100.nc')
print(nc)
attributes(nc$var)$names

# get tempora dimension
time <- ncvar_get(nc, "time")
nc$dim$time$units

time <- as.POSIXct('1601-01-01 00:00:00') + days(time)

# get simulated water temperature data
temp <- ncvar_get(nc, "watertemp")
depth <- ncvar_get(nc, "depth")

# close nc
nc_close(nc)

# get meteorological driver data
meteo_df <- read_csv(file = 'gfdl-esm4_ssp585_bias-adjusted_mendota_daily_2015_2100.csv')

# get local information
hyps_data <- read_csv(file = 'Mendota_hypsometry.csv')

# prepare for rLakeAnalyzer
temp_df <- data.frame(t(temp))
temp_df <- temp_df - 273.15
colnames(temp_df) <- paste0('wtr_', depth)
temp_df$datetime <- time

# will our lakes warm up?
ts_surface <- ts.layer.temperature(wtr = temp_df, top = 0, bottom = 2, 
                                   bathy = data.frame('depths' = hyps_data$DEPTH,
                                                      'areas' = hyps_data$BATHYMETRY_AREA), 
                                   na.rm = FALSE)

ts_surface <- ts_surface %>%
  mutate('air_temp' = meteo_df$tas - 273.15,
         'wind_spd' = meteo_df$sfcwind)


g1 <- ggplot(ts_surface, aes(datetime, layer.temp)) +
  geom_point() +
  ggtitle('surface water temperature') +
  geom_smooth(method = "lm")  
g2 <- ggplot(ts_surface, aes(datetime, air_temp)) +
  geom_point() +
  ggtitle('air temperature') +
  geom_smooth(method = "lm") 
g3 <- ggplot(ts_surface, aes(datetime, wind_spd)) +
  geom_point() +
  ggtitle('wind speed') +
  geom_smooth(method = "lm") 

g1 / g2 / g3

annual_surface <- ts_surface %>%
  mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarise(avg_temp = mean(layer.temp),
            avg_atmp = mean(air_temp),
            avg_windspd = mean(wind_spd))

summary(lm(avg_temp ~ year, data = annual_surface))
summary(lm(avg_atmp ~ year, data = annual_surface))
summary(lm(avg_windspd ~ year, data = annual_surface))

# how will climate change affect lake stability?
ts_St <- ts.schmidt.stability(wtr = temp_df, bathy = data.frame('depths' = hyps_data$DEPTH,
                                                       'areas' = hyps_data$BATHYMETRY_AREA))

ggplot(ts_St) +
  geom_point(aes(datetime, schmidt.stability))


ts_LN <- ts.lake.number(wtr = temp_df, wnd = data.frame('datetime' = meteo_df$time,
                                                        'wsp' = meteo_df$sfcwind), 
                        wnd.height = 10, 
                        bathy = data.frame('depths' = hyps_data$DEPTH,
                                                                'areas' = hyps_data$BATHYMETRY_AREA),
                        seasonal = TRUE)

ggplot(ts_LN) +
  geom_point(aes(datetime, lake.number))

# run a custom model with ISIMIP data
input_data <- temp_df %>% filter(datetime <= '2016-01-01')

time_ind <- length(input_data$datetime)
space_ind <- length(depth)

time =  paste0(seq(1,time_ind))

df_temp <- data.frame(cbind(time, (as.matrix(input_data[,2:ncol(input_data)-1]))) )
colnames(df_temp) <- c("time", as.character(paste0(seq(1,ncol(input_data) -1))))

m.df_temp <- reshape2::melt(df_temp, "time")
m.df_temp$time <- time

ggplot(m.df_temp, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0, 30),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'wtemp [dC]') +
  scale_y_continuous(trans = "reverse")


K <- matrix(0, nrow = space_ind, ncol = time_ind)  
min_n2 <- 7 * 10**(-5)

for (n in 1:ncol(K)){
  wt <- as.numeric(input_data[n, 1:(ncol(input_data)-1)])
  
  n2 <- buoyancy.freq(wtr = wt, depths = depth)
  
  n2[n2 < min_n2] <- min_n2
  
  ak <- 0.00706 * max(hyps_data$BATHYMETRY_AREA / 1e6) ** 0.56 
  
  K[, n] <- ak * abs(c(min_n2, as.numeric(n2))) ** (-0.43) / (24 * 3600)
}

which(K == Inf, arr.ind = TRUE)

df_K <- data.frame(cbind(time, t(K)) )
colnames(df_K) <- c("time", as.character(paste0(seq(1,nrow(K)))))

m.df_K <- reshape2::melt(df_K, "time")
m.df_K$time <- time

ggplot(m.df_K, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(1e-6,1e-4),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'diffusivity [m2/s]') +
  scale_y_continuous(trans = "reverse")




conc <- matrix(0, nrow = space_ind, ncol = time_ind * 24) 
# our results in a matrix: 100 seconds times 100 m over the depth
diff = K  # diffusion coefficient, unit: m2/s
dx = diff(depth) # our spatial step, unit: m
dt = 3600 #3600 * 24 # our time step, unit: s
conc[, 1] = 100 # dnorm(seq(1,nrow(conc),1), mean = nrow(conc)/2, sd = 1) * 100
# initial conc. is defined vertically through a normal distribution, unit: -

for (n in 2:ncol(conc)){ # time index
  conc[1, n] = 100
  for (i in 2:(nrow(conc)-1)){ # space index
    conc[i, n] = conc[i, n-1] + diff[i, max((n %/% 24 + 1)-1, 1)] * dt / dx[i]**2 * (conc[i+1, n-1] - 2 * conc[i, n-1] + conc[i-1, n-1]) # our FTCS scheme 
    # conc[i, n] = conc[i, n-1]  * exp(- 1e-20 * dt)
  
    }
}

which(conc == Inf, arr.ind = TRUE)

# Let's plot this!

time =  paste0(seq(1,ncol(conc)))
df <- data.frame(cbind(time, t(conc)) )
colnames(df) <- c("time", as.character(paste0(seq(1,nrow(conc)))))

m.df <- reshape2::melt(df, "time")
m.df$time <- time

ggplot(m.df, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,100),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'Conc. [%]') +
  scale_y_continuous(trans = "reverse")

