# function: the_nearest
# given a number, calculate the nearest to .25 or .75 (nearest number)
# the_nearest(18.6)=18.75
the_nearest <- function(x){
  a <- (x+0.25)*2
  b <- floor(a)
  c <- b+1
  if (abs(a-b) <= abs(a-c)){
    d <- b
  } else {
    d <- c
  }
  y <- d/2-0.25 # the nearest
  return(y)
}

# function: grid_coords
# grid coordinates (lon, lat)
grid_coords <- function(xgrid_min, ygrid_min, res){
  m <- rbind(c(xgrid_min, ygrid_min), 
             c(xgrid_min+res, ygrid_min),
             c(xgrid_min+res, ygrid_min+res), 
             c(xgrid_min, ygrid_min+res),
             c(xgrid_min, ygrid_min))
  return(m)
}

# function: areakm2lat
# grid area (km2) from latitude
areakm2lat <- function(lat, res=0.5, R=6371007){
  # R=6371007m is the authalic earth radius at equator
  height <- res*pi/180*R # height of the cells, same value for the whole grid
  width <- (sin((lat+res/2)*pi/180)-sin((lat-res/2)*pi/180))*R # cells width
  a_km2 <- width*height/1e6 #cells area depending on latitude
  return(a_km2)
}

# function: model_performance
# bias, mae, rmse, nrmse, nse
model_performance <- function(obs, sim) {
  #  bias
  bias <- mean((sim-obs), na.rm=TRUE)
  # mean absolute error
  mae <- mean(abs(obs-sim), na.rm=TRUE)
  # root mean square error
  rmse <- sqrt(mean((obs-sim)^2, na.rm=TRUE))
  # normalized root mean square error
  nrmse <- rmse/(max(obs, na.rm=TRUE)-min(obs, na.rm=TRUE))
  # correlation coef
  r <- sum((obs - mean(obs, na.rm=TRUE))*(sim - mean(sim, na.rm=TRUE)), na.rm=TRUE)/sqrt(sum((obs - mean(obs, na.rm=TRUE))^2, na.rm=TRUE)*sum((sim - mean(sim, na.rm=TRUE))^2, na.rm=TRUE))
  # nash sutcliff efficiency
  nse <- 1 - sum((obs-sim)^2, na.rm=TRUE)/sum((obs - mean(obs, na.rm=TRUE))^2, na.rm=TRUE)
  
  df <- data.frame(bias=bias, mae=mae, rmse=rmse, nrmse=nrmse, r=r, nse=nse)
  return(df)
}

# function: grids around the grid where the lake is located
generate_grids <- function(grid_lon, grid_lat, num_grids, grid_res) {
  xgrid_min <- grid_lon-(num_grids*grid_res+(grid_res/2)) # longitude
  ygrid_min <- grid_lat-(num_grids*grid_res+(grid_res/2)) # latitude
  grids <- list() 
  for(i in 1:((num_grids*2)+1)){
    for(j in 1:((num_grids*2)+1)){
      grids <- append(grids, list(st_polygon(list(grid_coords(xgrid_min+(i-1)*grid_res, ygrid_min+(j-1)*grid_res, grid_res)))))
    }
  }
  grids <- grids%>%st_sfc()%>%st_as_sf()
  st_crs(grids) <- 4326
  return(grids)
}

# function: fdir_grid
# flow direction around a grid
fdir_grid <- function(fdir, lon_fdir, lat_fdir, lon_grid, lat_grid, grid_res) {
  fdir_center_grid <- fdir[which(lon_fdir==lon_grid), which(lat_fdir==lat_grid)] # center
  fdir_left_grid <- fdir[which(lon_fdir==(lon_grid-grid_res)), which(lat_fdir==lat_grid)] # left 
  fdir_top_left_grid <- fdir[which(lon_fdir==(lon_grid-grid_res)), which(lat_fdir==(lat_grid+grid_res))] # top-left
  fdir_top_grid <- fdir[which(lon_fdir==lon_grid), which(lat_fdir==(lat_grid+grid_res))] # top
  fdir_top_right_grid <- fdir[which(lon_fdir==(lon_grid+grid_res)), which(lat_fdir==(lat_grid+grid_res))] # top-right
  fdir_right_grid <- fdir[which(lon_fdir==(lon_grid+grid_res)), which(lat_fdir==lat_grid)] # right
  fdir_bot_right_grid <- fdir[which(lon_fdir==(lon_grid+grid_res)), which(lat_fdir==(lat_grid-grid_res))] # bottom-right
  fdir_bot_grid <- fdir[which(lon_fdir==lon_grid), which(lat_fdir==(lat_grid-grid_res))] # bottom
  fdir_bot_left_grid <- fdir[which(lon_fdir==(lon_grid-grid_res)), which(lat_fdir==(lat_grid-grid_res))] # bottom-left
  
  fdir_grid <- matrix(c(fdir_top_left_grid, fdir_top_grid, fdir_top_right_grid,
                        fdir_left_grid, fdir_center_grid, fdir_right_grid, 
                        fdir_bot_left_grid, fdir_bot_grid, fdir_bot_right_grid),
                      nrow=3, ncol=3, byrow=TRUE,
                      dimnames=list(c(lat_grid+grid_res, lat_grid, lat_grid-grid_res),
                                      c(lon_grid+grid_res, lon_grid, lon_grid-grid_res)))
  
  fdir_center_grid <- TRUE
  fdir_left_grid <- ifelse(fdir_left_grid!=1, FALSE, TRUE)
  fdir_top_left_grid <- ifelse(fdir_top_left_grid!=2, FALSE, TRUE)
  fdir_top_grid <- ifelse(fdir_top_grid!=3, FALSE, TRUE)
  fdir_top_right_grid <- ifelse(fdir_top_right_grid!=4, FALSE, TRUE)
  fdir_right_grid <- ifelse(fdir_right_grid!=5, FALSE, TRUE)
  fdir_bot_right_grid <- ifelse(fdir_bot_right_grid!=6, FALSE, TRUE)
  fdir_bot_grid <- ifelse(fdir_bot_grid!=7, FALSE, TRUE)
  fdir_bot_left_grid <- ifelse(fdir_bot_left_grid!=8, FALSE, TRUE)
  
  selected_fdir_grid <- data.frame(c(lon_grid, lon_grid-grid_res, lon_grid-grid_res, lon_grid, lon_grid+grid_res, lon_grid+grid_res, lon_grid+grid_res, lon_grid, lon_grid-grid_res),
                                   c(lat_grid, lat_grid, lat_grid+grid_res, lat_grid+grid_res, lat_grid+grid_res, lat_grid, lat_grid-grid_res, lat_grid-grid_res, lat_grid-grid_res),
                                   c(fdir_center_grid, fdir_left_grid, fdir_top_left_grid, fdir_top_grid, fdir_top_right_grid, fdir_right_grid, fdir_bot_right_grid, fdir_bot_grid, fdir_bot_left_grid))
  colnames(selected_fdir_grid) <- c("lon","lat","fdir")

  return(list(fdir_grid, selected_fdir_grid))
}