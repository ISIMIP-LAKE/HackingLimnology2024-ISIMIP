## HACKING LIMNOLOGY 2024
# Climate Change - ISIMIP
# author: Robert Ladwig
# email:  rladwig@ecos.au.dk
# theme:  LOCAL LAKE ANALYSIS

# this part deals with analysing local lake output
# (1) quantifying long-term trends of water temp, air temp, and wind speed
# (2) exploring changes in lake physics (water column stability)
# (3) running a custom "water quality" model that uses ISIMIP data as input
# (this is an example of a very simple model)

# note, we will do a crash course in physical limnology. i am very sorry for that.
# keep in mind, water density is a function of temperature and salinity
# and density peaks at about 4 dC
# a stable density profile means that density has to increase over depth

Sys.setenv(LANG = "en")
Sys.setenv(TZ = "UTC")

# read in libraries
library(ncdf4)          # for ncdf4 data storage
library(tidyverse)      # awesome package for data analysis
library(rLakeAnalyzer)  # great for calculating physical lake indices
library(reshape2)       # data manipulations
library(lubridate)       # data manipulations
library(patchwork)      # visualizations

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# load local ISIMIP output for Lake Mendota, WI, "the best studied lake in the world"
nc <- nc_open(filename = 'data/gotm-ler_gfdl-esm4_w5e5_ssp585_2015soc_default_watertemp_mendota_daily_2015_2100.nc')
print(nc)

# output variable names
attributes(nc$var)$names

# get temporal dimension
time <- ncvar_get(nc, "time")
nc$dim$time$units

# get time information
time <- as.POSIXct('1601-01-01 00:00:00') + days(time)

# get simulated water temperature data
temp <- ncvar_get(nc, "watertemp")

# get depth data
depth <- ncvar_get(nc, "depth")

# close nc
nc_close(nc)

# get meteorological driver data
meteo_df <- read_csv(file = 'data/gfdl-esm4_ssp585_bias-adjusted_mendota_daily_2015_2100.csv')

# get local information
hyps_data <- read_csv(file = 'data/Mendota_hypsometry.csv')

## (1) quantifying long-term trends
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

# what is the trend?
annual_surface <- ts_surface %>%
  mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarise(avg_temp = mean(layer.temp),
            avg_atmp = mean(air_temp),
            avg_windspd = mean(wind_spd))

summary(lm(avg_temp ~ year, data = annual_surface))
summary(lm(avg_atmp ~ year, data = annual_surface))
summary(lm(avg_windspd ~ year, data = annual_surface))

# trend of surface water temp.: 
# trend of air temp.:           
# trend of wind speed:          

## (2) exploring changes in lake physics (water column stability)
# how will climate change affect lake stability?
# Schmidt stability (J/m2) tells us how much energy is needed to mix the entire water 
# column to uniform temperatures without affecting the amount of internal energy
# Idso (1973) https://doi.org/10.4319/lo.1973.18.4.0681
ts_St <- ts.schmidt.stability(wtr = temp_df, bathy = data.frame('depths' = hyps_data$DEPTH,
                                                       'areas' = hyps_data$BATHYMETRY_AREA))

ggplot(ts_St) +
  geom_point(aes(datetime, schmidt.stability))

ggplot(ts_St %>% mutate(decade =  year(datetime)- year(datetime) %% 10 ), 
       aes(schmidt.stability, fill = as.factor(decade))) +
  geom_density(alpha =.3)

ggplot(ts_St %>% mutate(decade =  year(datetime)- year(datetime) %% 10 ) %>%
         filter(decade == 2020 | decade == 2090), 
       aes(schmidt.stability, fill = as.factor(decade))) +
  geom_density(alpha =.3)

# Lake Number (dimensionless) is the ratio of the moments of the stabilizing 
# force of gravity associated with density stratification 
# to the destabilizing forces supplied by wind, cooling, inflow, etc.
# Robertson & Imberger (1994) http://dx.doi.org/10.1002/iroh.19940790202 
# LN ==1:   wind is sufficient to deflect thermocline
# LN >> 1:  stratification is strong and stable
# LN << 1:  stratification is weak, strong internal waves, deep mixing
ts_LN <- ts.lake.number(wtr = temp_df %>% mutate(datetime = as.Date(datetime)), 
                        wnd = data.frame('datetime' = as.Date(meteo_df$time),
                                                        'wsp' = meteo_df$sfcwind), 
                        wnd.height = 10, 
                        bathy = data.frame('depths' = hyps_data$DEPTH,
                                                                'areas' = hyps_data$BATHYMETRY_AREA),
                        seasonal = TRUE)

ggplot(ts_LN) +
  geom_point(aes(datetime, lake.number))

n_LN <- ts_LN %>% mutate(year = year(datetime)) %>%
  group_by(year) %>%
  summarize(n = sum(lake.number > 1, na.rm = T)) 

ggplot(n_LN, aes(year, n)) +
  geom_point()+
  geom_smooth(method = "lm")

# take-aways:
# Schmidt stability (water column stability): 
# Lake Number (deep mixing):                  


# (3) running a custom "water quality" model
# run a custom model with ISIMIP data
# one year of Lake Mendota
input_data <- temp_df %>% filter(datetime <= '2016-01-01')

time_ind <- length(input_data$datetime)
space_ind <- length(depth)

time =  paste0(seq(1,time_ind))

df_temp <- data.frame(cbind(time, (as.matrix(input_data[,2:ncol(input_data)-1]))) )
colnames(df_temp) <- c("time", as.character(paste0(seq(1,ncol(input_data) -1))))

m.df_temp <- reshape2::melt(df_temp, "time")
m.df_temp$time <- time

# plot temperature data
ggplot(m.df_temp, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0, 30),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'wtemp [dC]') +
  scale_y_continuous(trans = "reverse")

# get eddy diffusivity values based on buoyancy frequency
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

# plot our estimates of eddy diffusivities
ggplot(m.df_K, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(1e-6,1e-4),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'diffusivity [m2/s]') +
  scale_y_continuous(trans = "reverse")


# our simple water quality model of a concentration C
# dC / dt = K d^2C / dz^2 
# z = 0: constant C
# z = z_max: dC / dt = - k C
K_multiplier = 1 # let's play with this later

conc <- matrix(0, nrow = space_ind, ncol = time_ind * 24) 

diff = K * K_multiplier # diffusion coefficient, unit: m2/s
depl_flux = 0.1 # depletion flux at lowest layer, unit: [-] per s
dx = diff(depth) # our spatial step, unit: m
dt = 3600 # our time step, unit: s
conc[, 1] = 10 
# initial conc. is defined vertically as uniform distribution, unit: [-]

for (n in 2:ncol(conc)){ # time index
  conc[1, n] = 100
  conc[(nrow(conc)), n] = conc[(nrow(conc)), n-1] * exp(-depl_flux * dt) 
  for (i in 2:(nrow(conc)-1)){ # space index
    conc[i, n] = conc[i, n-1] + diff[i, max((n %/% 24 + 1)-1, 1)] * dt / dx[i]**2 * (conc[i+1, n-1] - 2 * conc[i, n-1] + conc[i-1, n-1]) # our FTCS scheme 
    }
}

which(conc == Inf, arr.ind = TRUE)

# Let's plot this!

time =  paste0(seq(1,ncol(conc)))
df <- data.frame(cbind(time, t(conc)) )
colnames(df) <- c("time", as.character(paste0(seq(1,nrow(conc)))))

m.df <- reshape2::melt(df, "time")
m.df$time <- time


# plot the concentration map
ggplot(m.df, aes(as.numeric(time), as.numeric(variable))) +
  geom_raster(aes(fill = as.numeric(value)), interpolate = TRUE) +
  scale_fill_gradientn(limits = c(0,100),
                       colours = rev(RColorBrewer::brewer.pal(11, 'Spectral')))+
  theme_minimal()  +xlab('Time') +
  ylab('Depth') +
  labs(fill = 'Conc. [%]') +
  scale_y_continuous(trans = "reverse")

# explore our model:

# assume lower diffusivity
K_multiplier = 1e-2

# lower depletion flux
depl_flux = 1e-3

# future:
input_data <- temp_df %>% filter(datetime >= '2090-01-01' &
                                   datetime <= '2099-01-01')

