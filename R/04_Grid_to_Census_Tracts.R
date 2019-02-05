#' -----------------------------------------------------------------------------
#' Date created: February 4, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script converts the kriged grid to a raster and then 
#' uses sp::over to get a weighted mean at the census tract level
#' -----------------------------------------------------------------------------

library(sf)
library(sp)
library(raster)
library(tidyverse)
library(ncdf4)
library(tmaptools)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' Read in Kate's grid directly from the netCDF to a raster
ncdf_name <- "grid_points.nc"
kate_raster <- raster(here::here("Data/netCDF_Data", ncdf_name))
plot(kate_raster)
kate_raster_nrow <- nrow(kate_raster)
kate_raster_ncol <- ncol(kate_raster)

#' Read in kriging grid (.csv to sf object)
grid_name <- "grid_points.csv"
krige_grid <- read_csv(here::here("Data/netCDF_Data", grid_name)) %>% 
  st_as_sf(wkt = "WKT", crs = ll_wgs84)

head(krige_grid)
plot(st_geometry(krige_grid), pch = ".")

#' Read in the Denver census tracts (shapefile to sf object)
den_tracts <- st_read(here::here("Data", "tl_2017_08_tract.shp"),
                     stringsAsFactors = F) %>%
  filter(COUNTYFP == "031") %>% 
  st_transform(ll_wgs84)
plot(st_geometry(den_tracts))

#' Confirm all tracts are within the grid
plot(st_geometry(krige_grid), pch = ".")
plot(st_geometry(den_tracts), fill = NA, border = "red", add = T)

#' Read in the kriged estimates
kriged_name <- "8hour_44201_2011_grid_points_Kriged.csv"
results <- read_csv(here::here("Results", kriged_name))

#' -----------
#' Zonal statistics
#' Create a loop for each date in the dataset
date_list <- unique(results$date)
tract_exp <- data.frame()

for (i in 1:length(date_list)) {
  date_results <- filter(results, date == date_list[i])
  
  #' Join daily results to grid cell centers
  results_grid <- left_join(krige_grid, date_results, by = "id")
  # plot(results_grid)
  results_coords <- st_coordinates(results_grid)
  
  #' Convert the grid points into a raster
  #' Use the raster from Kate's netCDF file as the template (nrow and ncol)
  #' If more than one point falls in the raster cell, take the average
  #' Some rasterized cells don't have values, but they are far from the study 
  #' area
  empty_raster <- raster(extent(results_grid), crs = ll_wgs84,
                         nrow = kate_raster_nrow, ncol = kate_raster_ncol)
  
  results_raster <-  rasterize(x = results_grid, y = empty_raster,
                               field = "conc_pred_ok", 
                               fun = mean, update = T, na.rm=T)
  plot(results_raster)

  #' Extract average raster value for each census tract polygon
  ct_means <- raster::extract(results_raster, den_tracts, fun = mean, small = T,
                              weights=T, normalizeWeights = T)
  
  temp <- data.frame(GEOID = den_tracts$GEOID,
                     ct_mean_exposure = ct_means, 
                     pollutant = unique(results_grid$pollutant),
                     date = date_list[i])
  tract_exp <- bind_rows(tract_exp, temp)
}

head(tract_exp)









