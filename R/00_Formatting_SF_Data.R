#' -----------------------------------------------------------------------------
#' Date created: February 1, 2019
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script converts Kate O'Dell's netCDF file into an sf object
#' -----------------------------------------------------------------------------

library(sf)
library(sp)
library(spatialEco)
library(gstat)
library(automap)
library(tidyverse)
library(ncdf4)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Read in the netCDF
#' -----------------------------------------------------------------------------

#' Open CDF
cdf_name <- "grid_points.nc"
cdf <- nc_open(here::here("Data/netCDF_Data", cdf_name))
print(cdf)

#' Extract coordinates
cdf_lon <- ncvar_get(cdf, varid="center lon")
cdf_lat <- ncvar_get(cdf, varid="center lat")

#' Create an sf object
cdf_df <- data.frame(lon = as.vector(cdf_lon),
                     lat = as.vector(cdf_lat))
cdf_df$id <- 1:nrow(cdf_df)
plot(cdf_df$lon, cdf_df$lat)

cdf_sf <- st_as_sf(cdf_df, coords = c("lon", "lat"), crs = ll_wgs84)
plot(st_geometry(cdf_sf))

cdf_sf_name <- "grid_points.csv"
st_write(cdf_sf, here::here("Data/netCDF_Data", cdf_sf_name),
         layer_options = "GEOMETRY=AS_WKT", delete_dsn = T)
  