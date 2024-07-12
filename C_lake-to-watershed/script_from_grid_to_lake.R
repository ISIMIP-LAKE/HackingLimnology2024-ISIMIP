### HACKING LIMNOLOGY 2024
# climate change - ISIMIP
# author: Ana Isabel Ayala Zamora
# email:  isabel.ayala.zamora@ebc.uu.se
# theme:  GRID TO LAKE
# date: 12 July 2024

# load necessary libraries
#install.packages("data.table")
#install.packages("sf")
#install.packages("ncdf4")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("readxl")
library(data.table)
library(sf)
library(ncdf4)
library(dplyr)
library(ggplot2)
library(readxl)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("helper_functions.R") # read functions written in another script

# 1. data: water (global)
# https://data.isimip.org/search/tree/ISIMIP3a
# simulation round: ISIMIP3a, output data, sector: water (global), impact model: WaterGAP2-2e, climate forcing: gswp3-w5e5, 
# climate experiment: obsclim, Direct human forcing experiment: histsoc, sensitivity experiment: default
# variable: qtot (total runoff)
# qtot[lon, lat, time]
nc_qtot <- nc_open(file.path("data","watergap2-2e_gswp3-w5e5_obsclim_histsoc_default_qtot_global_monthly_1901_2019.nc")) # open a netCDF file
names(nc_qtot$var) # var name
qtot <- ncvar_get(nc_qtot,"qtot") # total runoff (kg/m2/s)
qtot <- qtot/1000 # kg/m2/s -> m/s, kg/m2/s * 1l/1kg * 1m3/1000l 
lon <- ncvar_get(nc_qtot,"lon") # longitude (degree) 
lat <- ncvar_get(nc_qtot,"lat") # latitude (degree)
time <- ncvar_get(nc_qtot, "time") # time (days), start on 1901-01-01
date <- seq(as.Date("1901-01-01"), by="month", length.out=dim(time))
nc_close(nc_qtot) # close a netCDF file
# variable: qg (groundwater runoff)
# qg[lon, lat, time]
nc_qg <- nc_open(file.path("data","watergap2-2e_gswp3-w5e5_obsclim_histsoc_default_qg_global_monthly_1901_2019.nc")) # open a netCDF file
names(nc_qg$var) # var name
qg <- ncvar_get(nc_qg,"qg") # groundwater runoff (kg/m2/s)
qg <- qg/1000 # kg/m2/s -> m/s, kg/m2/s * 1l/1kg * 1m3/1000l 
nc_close(nc_qg) # close a netCDF file
# simulation round: ISIMIP3a, input data, static geographic information, river routing
# variable: fdir (flow direction)
# fdir[lon, lat]
nc_fdir <- nc_open(file.path("data","ddm30_flowdir_cru_neva.nc")) # open a netCDF file
names(nc_fdir$var) # var name
fdir <- ncvar_get(nc_fdir,"flowdirection") # flow direction (1: east, 2: south east, 3: south, 4: south west,
#                                                            5: west, 6: north west; 7: north; 8: north east,
#                                                            0: sink or outlet to the ocean)
lon_fdir <- ncvar_get(nc_fdir,"lon") # longitude (degree) 
lat_fdir <- ncvar_get(nc_fdir,"lat") # latitude (degree)

# 2. data: Lake (global)
# https://github.com/icra/ISIMIP_Lake_Sector
# representative lakes: one selected lake per pixel at a 0.5º resolution
# the selected lake has a depth corresponding to the depth weighted median (weighted by area of the lakes) for all the lakes contained in each pixel with a 0.5º resolution
lakes <- st_read(file.path("data","HL_selected_cent.shp")) # load shapefile 

# 3. case study I: big lake, small watershed
# lake_id=1074 (from hydrolakes)
lake_1074 <- lakes[lakes$Hylak_id==1074, ]; lake_1074 # hydrolakes + pseudocentroid inside polygon (lat, long)

# coordinates of the grid centre
coords_lake_1074 <- data.frame(lon=the_nearest(lake_1074$Cent_Long), lat=the_nearest(lake_1074$Cent_Lat)); coords_lake_1074

# grids around the grid where the pseudecentroid of the lake polygon is located
grid_res <- 0.5 # grid resolution (degree)
grids_lake_1074 <- generate_grids(coords_lake_1074$lon, coords_lake_1074$lat, 1, grid_res) 

# plot: lake, pseudocentroid and grids
ggplot() + 
  geom_sf(data=grids_lake_1074, fill='white') +
  geom_sf(data=lake_1074, fill="blue", alpha=0.5) +
  geom_point(data=lake_1074, aes(x=Cent_Long, y=Cent_Lat), shape=19) +
  scale_x_continuous(breaks=seq(13.5, 15.0, 0.5), expand=expansion()) +
  scale_y_continuous(breaks=seq(65.0, 66.5, 0.5), expand=expansion()) +
  xlab("Longitude") + ylab("Latitude")

# ratio between grid area and watershed area
grid_area_lake_1074 <- areakm2lat(coords_lake_1074$lat, res=grid_res, R=6371007); grid_area_lake_1074 # grid area where the pseudocentroid is located (km2)

lake_1074$Wshd_area # watershed area (km2)

ratio_area_lake_1074 <- lake_1074$Wshd_area/grid_area_lake_1074; ratio_area_lake_1074 # ~1

# intersection between lake polygon and grids
int_grids_lake_1074 <- st_intersection(grids_lake_1074, lake_1074); int_grids_lake_1074 # find the intersection and divide it into parts   

# pseudocentroid inside polygon (lat, long) of each part
int_grids_cent_lake_1074 <- st_coordinates(st_point_on_surface(int_grids_lake_1074)) 
colnames(int_grids_cent_lake_1074) <- c("lon", "lat"); int_grids_cent_lake_1074

# nearest grid cell 
int_grids_lake_1074 <- data.table(NULL)
for (i in 1:nrow(int_grids_cent_lake_1074)) {
  int_grids_lake_1074 <- rbind(int_grids_lake_1074, t(c(the_nearest(int_grids_cent_lake_1074[i, "lon"]), the_nearest(int_grids_cent_lake_1074[i, "lat"])))) # nearest grid (grid it belongs to; long, lat)
}
int_grids_lake_1074

# plot: lake, pseudocentroid, grids and pseudocentroid of each part
ggplot() + 
  geom_sf(data=grids_lake_1074, fill='white') +
  geom_sf(data=lake_1074, fill="blue", alpha=0.5) +
  geom_point(data=lake_1074, aes(x=Cent_Long, y=Cent_Lat), shape=19) +
  scale_x_continuous(breaks=seq(13.5, 15.0, 0.5), expand=expansion()) +
  scale_y_continuous(breaks=seq(65.0, 66.5, 0.5), expand=expansion()) +
  xlab("Longitude") + ylab("Latitude") +
  geom_point(data=as.data.frame(int_grids_cent_lake_1074), aes(x=lon, y=lat), shape=3) 

# flow to the lake
# qin=(qtot+qg)*wshd_area [m3/s] for each grid
l_qin_grids_lake_1074 <- list()
for (i in 1:nrow(int_grids_lake_1074)) {
  dt_qin_grid_lake_1074 <- data.table(date, qtot[which(lon==int_grids_lake_1074[i, lon]), which(lat==int_grids_lake_1074[i, lat]), 1:dim(qtot)[3]], qg[which(lon==int_grids_lake_1074[i, lon]), which(lat==int_grids_lake_1074[i, lat]), 1:dim(qtot)[3]])
  setnames(dt_qin_grid_lake_1074, c("date","qtot","qg"))
  dt_qin_grid_lake_1074[, qin := (qtot+qg)*lake_1074$Wshd_area*1e+06] # qtot+qg [m/s] * wshd_area [m2] -> qin [m3/s]
  l_qin_grids_lake_1074 <- append(l_qin_grids_lake_1074, list(dt_qin_grid_lake_1074))
}  
# grids average
qin_sim <- 0
for (i in 1:length(l_qin_grids_lake_1074)) {
  qin_sim <- qin_sim+(l_qin_grids_lake_1074[[i]]$qin/length(l_qin_grids_lake_1074))
  dt_qin_sim_lake_1074 <- data.table(date, qin_sim)
}
# plot
par(mar=c(4.1, 5, 4.1, 5)) 
plot(dt_qin_sim_lake_1074$date, dt_qin_sim_lake_1074$qin, type="l", 
     xlab="", ylab=expression("Flow to the lake [m"^3*" s"^-1*"]"))

# compare with outputs from other model
# HYPE outputs: https://hypeweb.smhi.se/explore-water/historical-data/europe-time-series/
dt_qin_hype_lake_1074 <- data.table(read_excel(file.path("data","8101617.xls"))) # daily
setnames(dt_qin_hype_lake_1074, c("date", "qin_hype")) # qin (m3/s)
dt_qin_hype_lake_1074[, date := as.Date(date)]
# monthly average
dt_qin_hype_lake_1074[, year := year(date)]
dt_qin_hype_lake_1074[, month := month(date)]
dt_qin_hype_monthly_lake_1074 <- dt_qin_hype_lake_1074[, lapply(.SD, mean, na.rm=TRUE), by=.(year, month), .SDcols=c("qin_hype")] # monthly average per year
# set a date
lag=TRUE # sometimes there is a time lag of one month
if (lag==TRUE) {
  dt_qin_hype_monthly_lake_1074[, date := as.Date(paste(ifelse(month!=1, year, year-1), ifelse(month!=1, month-1, 12), "01", sep="-"), format="%Y-%m-%d")] 
} else {
  dt_qin_hype_monthly_lake_1074[, date := as.Date(paste(year, month, "01", sep="-"), format="%Y-%m-%d")]
}
# merge the calculated inflow with HYPE outputs 
dt_qin_lake_1074 <- merge(dt_qin_sim_lake_1074, dt_qin_hype_monthly_lake_1074[, c("date","qin_hype")], by="date")
# plot: Simulations/HYPE over time
par(mar=c(4.1, 5, 4.1, 5)) 
plot(dt_qin_lake_1074$date, dt_qin_lake_1074$qin_sim, type="l", col="blue", lwd=2,  
     xlab="", ylab=expression("Flow to the lake [m"^3*" s"^-1*"]"))
lines(dt_qin_lake_1074$date, dt_qin_lake_1074$qin_hype, col="black", lwd=2)
legend("topleft", legend=c("Simulations", "HYPE"), col=c("blue", "black"), lwd=2, cex=1, bty="n")
# plot: Simulations vs HYPE 
plot(dt_qin_lake_1074[, qin_sim], dt_qin_lake_1074[, qin_hype], type="p", pch=19,
     xlab=expression("Simulated flow to the lake [m"^3*" s"^-1*"]"), ylab=expression("HYPE flow to the lake [m"^3*" s"^-1*"]"))
abline(0, 1, col="black")
# model performance
mp_lake_1074 <- model_performance(dt_qin_lake_1074[, qin_hype], dt_qin_lake_1074[, qin_sim]) 
mtext(paste0("BIAS=", round(mp_lake_1074$bias, 2), ", ", "RMSE=", round(mp_lake_1074$rmse, 2), ", ", "NRMSE=", round(mp_lake_1074$nrmse, 2), ", ", "R=", round(mp_lake_1074$r, 2), ", ", "NSE=", round(mp_lake_1074$nse, 2)), 3, line=-2, col="black", cex=1)

# 4. case study II: small lake, big watershed
# lake_id=152117 (from hydrolakes)
lake_152117 <- lakes[lakes$Hylak_id==152117, ]; lake_152117 # hydrolakes + pseudocentroid inside polygon (lat, long)

# coordinates of the grid centre
coords_lake_152117 <- data.frame(lon=the_nearest(lake_152117$Cent_Long), lat=the_nearest(lake_152117$Cent_Lat)); coords_lake_152117

# grids around the grid where the pseudecentroid of the lake polygon is located
grids_lake_152117 <- generate_grids(coords_lake_152117$lon, coords_lake_152117$lat, 1, grid_res) 

# plot: lake, pseudocentroid and grids
ggplot() + 
  geom_sf(data=grids_lake_152117, fill='white') +
  geom_sf(data=lake_152117, fill="blue", alpha=0.5) +
  geom_point(data=lake_152117, aes(x=Cent_Long, y=Cent_Lat), shape=19) +
  scale_x_continuous(breaks=seq(13.0, 14.5, 0.5), expand=expansion()) +
  scale_y_continuous(breaks=seq(61.5, 63.0, 0.5), expand=expansion()) +
  xlab("Longitude") + ylab("Latitude")

# ratio between grid area and watershed area
grid_area_lake_152117 <- areakm2lat(coords_lake_152117$lat, res=grid_res, R=6371007); grid_area_lake_152117 # grid area where the pseudocentroid is located (km2)

lake_152117$Wshd_area # watershed area (km2)

ratio_area_lake_152117 <- lake_152117$Wshd_area/grid_area_lake_152117; ratio_area_lake_152117 # ~2

# flow direction around the grid where the lake is located
fdir_grid_152117 <- fdir_grid(fdir, lon_fdir, lat_fdir, coords_lake_152117$lon, coords_lake_152117$lat, grid_res); fdir_grid_152117
fdir_grid_152117 <- data.table(fdir_grid_152117[[2]][fdir_grid_152117[[2]]$fdir==TRUE, ]); fdir_grid_152117

# flow to the lake
# qin=(qtot+qg)*wshd_area [m3/s] for each grid
l_qin_grids_lake_152117 <- list()
for (i in 1:nrow(fdir_grid_152117)) {
  dt_qin_grid_lake_152117 <- data.table(date, qtot[which(lon==fdir_grid_152117[i, lon]), which(lat==fdir_grid_152117[i, lat]), 1:dim(qtot)[3]], qg[which(lon==fdir_grid_152117[i, lon]), which(lat==fdir_grid_152117[i, lat]), 1:dim(qtot)[3]])
  setnames(dt_qin_grid_lake_152117, c("date","qtot","qg"))
  dt_qin_grid_lake_152117[, qin := (qtot+qg)*lake_152117$Wshd_area*1e+06] # qtot+qg [m/s] * wshd_area [m2] -> qin [m3/s]
  l_qin_grids_lake_152117 <- append(l_qin_grids_lake_152117, list(dt_qin_grid_lake_152117))
}  
# grids average
qin_sim <- 0
for (i in 1:length(l_qin_grids_lake_152117)) {
  qin_sim <- qin_sim+(l_qin_grids_lake_152117[[i]]$qin/length(l_qin_grids_lake_152117))
  dt_qin_sim_lake_152117 <- data.table(date, qin_sim)
}
# plot
par(mar=c(4.1, 5, 4.1, 5)) 
plot(dt_qin_sim_lake_152117$date, dt_qin_sim_lake_152117$qin, type="l", 
     xlab="", ylab=expression("Flow to the lake [m"^3*" s"^-1*"]"))

# compare with outputs from other model
# HYPE outputs: https://hypeweb.smhi.se/explore-water/historical-data/europe-time-series/
dt_qin_hype_lake_152117 <- data.table(read_excel(file.path("data","8301441.xls"))) # daily
setnames(dt_qin_hype_lake_152117, c("date", "qin_hype")) # qin (m3/s)
dt_qin_hype_lake_152117[, date := as.Date(date)]
# monthly average
dt_qin_hype_lake_152117[, year := year(date)]
dt_qin_hype_lake_152117[, month := month(date)]
dt_qin_hype_monthly_lake_152117 <- dt_qin_hype_lake_152117[, lapply(.SD, mean, na.rm=TRUE), by=.(year, month), .SDcols=c("qin_hype")] # monthly average per year
# set a date
lag=TRUE # sometimes there is a time lag of one month
if (lag==TRUE) {
  dt_qin_hype_monthly_lake_152117[, date := as.Date(paste(ifelse(month!=1, year, year-1), ifelse(month!=1, month-1, 12), "01", sep="-"), format="%Y-%m-%d")] 
} else {
  dt_qin_hype_monthly_lake_152117[, date := as.Date(paste(year, month, "01", sep="-"), format="%Y-%m-%d")]
}
# merge the calculated inflow with HYPE outputs 
dt_qin_lake_152117 <- merge(dt_qin_sim_lake_152117, dt_qin_hype_monthly_lake_152117[, c("date","qin_hype")], by="date")
# plot: Simulations/HYPE over time
par(mar=c(4.1, 5, 4.1, 5)) 
plot(dt_qin_lake_152117$date, dt_qin_lake_152117$qin_sim, type="l", col="blue", lwd=2,  
     xlab="", ylab=expression("Flow to the lake [m"^3*" s"^-1*"]"))
lines(dt_qin_lake_152117$date, dt_qin_lake_152117$qin_hype, col="black", lwd=2)
legend("topleft", legend=c("Simulations", "HYPE"), col=c("blue", "black"), lwd=2, cex=1, bty="n")
# plot: Simulations vs HYPE 
plot(dt_qin_lake_152117[, qin_sim], dt_qin_lake_152117[, qin_hype], type="p", pch=19,
     xlab=expression("Simulated flow to the lake [m"^3*" s"^-1*"]"), ylab=expression("HYPE flow to the lake [m"^3*" s"^-1*"]"))
abline(0, 1, col="black")
# model performance
mp_lake_152117 <- model_performance(dt_qin_lake_152117[, qin_hype], dt_qin_lake_152117[, qin_sim]) 
mtext(paste0("BIAS=", round(mp_lake_152117$bias, 2), ", ", "RMSE=", round(mp_lake_152117$rmse, 2), ", ", "NRMSE=", round(mp_lake_152117$nrmse, 2), ", ", "R=", round(mp_lake_152117$r, 2), ", ", "NSE=", round(mp_lake_152117$nse, 2)), 3, line=-2, col="black", cex=1)