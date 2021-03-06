#' -----------------------------------------------------------------------------
#' Date created: October 25, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: This script calculates daily means at each monitor
#' Also includes code to calculate MDA8 for ozone data
#' -----------------------------------------------------------------------------

library(sf)
library(sp)
library(spatialEco)
library(gstat)
library(automap)
library(tidyverse)

ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Read in pollutant data and summarize to daily means
#' Source: AQS Data Mart (Script: 01_AQS_Data_Scrape.R)
#' -----------------------------------------------------------------------------

#' For which years and which state do we want data?
years <- c(2011:2017)
state <- "08" #Colorado
time_zone <- "America/Denver"

#' https://aqs.epa.gov/aqsweb/codes/data/ParametersByDesc.csv
#' Criteria pollutants and carbon parameters

pol <- c("88101", "44201") # PM2.5 and O3

#' -----------------------------------------------------------------------------
#' Points for krigings
#' gstat requires sp objects, not sf objects
#' -----------------------------------------------------------------------------

#' ----------------
#' Using census tract centroids
#' 
#' grid_name <- "denver_tracts"
#' 
#' #' First, create a boundary based on the 470 loop
#' #' Coordinates based on Google Maps
#' pts_df <- data.frame(lon=c(-105.194150, -105.086157, -104.785862,
#'                            -104.715897, -105.007800),
#'                      lat=c(39.712174, 39.553840, 39.548127,
#'                            39.740367, 39.984862))
#' pts <-  st_as_sf(pts_df, coords=c("lon", "lat"), crs=ll_wgs84)
#' plot(st_geometry(pts), main="Boundary Points: latlong")
#' 
#' #' Project to Albers equal area (used by the Land Use dataset)
#' #' Want to preserve area so we can calculate area-based metrics such as 
#' #' population density
#' pts_aea <- st_transform(pts, crs=albers)
#' plot(st_geometry(pts_aea), main="Boundary Points: Albers Equal Area")
#' 
#' #' Get boundary around these points
#' pts_bound <- st_make_grid(pts_aea, n = 1)
#' plot(st_geometry(pts_bound), main="bounding box", border="red", col=NA)
#' plot(st_geometry(pts_aea), col="blue", add=T)
#' 
#' #' Add a 5 km buffer around the bounding box
#' bound_5km <- st_buffer(pts_bound, dist=5000)
#' plot(st_geometry(bound_5km), main="extended bounding box")
#' plot(st_geometry(pts_bound), col=NA, border="red", add=T)
#' plot(st_geometry(pts_aea), col="blue", add=T)
#' 
#' #' Next, select census tracts that have centroids completely within this boundary
#' co_tracts <- st_read(here::here("Data", "tl_2017_08_tract.shp")) %>% 
#'   st_transform(albers) %>% 
#'   mutate(GEOID = as.character(GEOID))
#' plot(st_geometry(co_tracts))
#' 
#' co_centroids <- st_centroid(co_tracts) 
#' 
#' denver_centroids <- co_centroids[st_within(co_centroids, bound_5km) %>% lengths > 0,]
#' 
#' denver_tracts <- filter(co_tracts, GEOID %in% denver_centroids$GEOID)
#' 
#' plot(st_geometry(denver_tracts))
#' plot(st_geometry(denver_centroids), col="blue", add=T)
#' plot(st_geometry(bound_5km), add=T, border="red")
#' 
#' #' Lastly, identify the centroids of the census tracts for kriging
#' krige_pts <- denver_centroids %>% 
#'   mutate_if(is.character, as.numeric) %>% 
#'   rename(id = GEOID)
#'   
#' 

#' ----------------
#' Using Kate's grid

grid_name <- "grid_points.csv"

krige_pts <- read_csv(here::here("Data/netCDF_Data", grid_name)) %>% 
  st_as_sf(wkt = "WKT", crs = ll_wgs84) %>% 
  st_transform(albers)

head(krige_pts)
plot(st_geometry(krige_pts), pch = ".")

#' ----------------
#' Finalize data set

#' Converting from sf to sp 
krige_pts_sp <- as(krige_pts, "Spatial")
plot(krige_pts_sp)

grid_name2 <- gsub(".csv", "", grid_name)


#' -----------------------------------------------------------------------------
#' Set up objects for kriging
#' #' Note: gstat (for kriging) requires sp objects (not sf)
#' -----------------------------------------------------------------------------

#' List of files to krige
krige_files <- data.frame(krige_files = list.files(path = here::here("Data/AQS_Data"))) %>%
  mutate(krige_files = as.character(krige_files)) %>%
  filter(str_detect(krige_files,
                    paste(as.character(years),collapse = '|')))
krige_files <- krige_files$krige_files

for (i in 1:length(krige_files)) {
  
  #' Output file names
  aqs_krige_name <- paste0(str_replace(krige_files[i], ".csv", ""),
                           "_", grid_name2, "_Kriged.csv")
  
  #' Output file names
  aqs_cv_name <- paste0(str_replace(krige_files[i], ".csv", ""),
                        "_", grid_name2, "_CV.csv")
  
  #' Output file names
  aqs_diagnostics_name <- paste0(str_replace(krige_files[i], ".csv", ""), 
                                 "_", grid_name2, "_Diagnostics.csv")
  
  daily <- read_csv(here::here("Data/AQS_Data", krige_files[i]))
  colnames(daily) <- gsub(" ", "_", colnames(daily))
  daily <- daily %>%  
    filter(State_Code == state) %>% 
    mutate(County_Code = str_pad(County_Code, 3, pad = "0"),
           Site_Num = str_pad(Site_Num, 4, pad = "0")) %>% 
    mutate(monitor_id = paste0(County_Code, Site_Num)) %>% 
    
    #' using the mean with flagged data excluded for 9h ozone
    #' Arithmetic mean for all other measures
    rowwise() %>% 
    mutate(mean = ifelse(str_detect(krige_files[i], "8hour_"), 
                         Mean_Excluding_All_Flagged_Data, 
                         Arithmetic_Mean)) %>% 
    
    #' convert ppm to ppb
    mutate(mean = ifelse(Units_of_Measure == "Parts per million", 
                         mean * 1000, mean)) %>% 
    mutate(Units_of_Measure = ifelse(Units_of_Measure == "Parts per million", 
                                     "ppb", Units_of_Measure)) %>%
    
    #' Just get the first measurement, not the co-located one
    filter(POC == 1)
  
  #' Monitor locations
  monitor_pts <- select(daily, monitor_id, Longitude, Latitude) %>%
    filter(!is.na(Longitude)) %>% 
    distinct() %>%
    st_as_sf(coords = c("Longitude", "Latitude"), crs = ll_wgs84) %>% 
    st_transform(albers)
  
  # plot(st_geometry(krige_pts))
  # plot(st_geometry(monitor_pts), col="red", add=T)
  
  #' if using 8h ozone data, need to select daily 8h max
  if(str_detect(krige_files[i], "8hour_44201")) {
    ozone <- select(daily, monitor_id, Date_Local, mean) %>% 
      group_by(monitor_id, Date_Local) %>% 
      summarize(mean = max(mean))
    
    daily <- select(daily, monitor_id, Date_Local, Units_of_Measure) %>%
      left_join(ozone, by=c("monitor_id", "Date_Local")) %>% 
      distinct()
    
    rm(ozone)
  }
  
  #' if using monthly 8h ozone data, calculate monthly mean of the daily 8 h max
  if(str_detect(krige_files[i], "monthly")) {
    ozone <- select(daily, monitor_id, Date_Local, month, mean) %>% 
      group_by(monitor_id, Date_Local, month) %>% 
      summarize(mean = max(mean)) %>% 
      ungroup() %>% 
      group_by(monitor_id, month) %>% 
      summarize(mean = mean(mean))

    daily <- select(daily, monitor_id, month, Units_of_Measure) %>%
      left_join(ozone, by=c("monitor_id", "month")) %>% 
      distinct() %>% 
      rename("Date_Local" = "month")

    rm(ozone)
  }
  
  #' ------------------------------------------------------------------
  #' Interpolation and Ordinary kriging
  
  #' List of models to try to fit
  #' The ones listed in "all models" are the standard ones to try
  show.vgms()
  all_models <- c("Exp", "Sph", "Gau", "Cir", "Lin", "Log")
  
  #' cutoff distance is 50 km
  #' May need to play with this
  c_dist = 50000
  
  #' list of dates, pollutants to loop through
  dates <- unique(daily$Date_Local)
  
  #' data frames to collect results
  krige_data <- data.frame()
  cv_data <- data.frame()
  cv_diagnostics <- data.frame()
  
  for (j in 1:length(dates)) {
    daily2 <- filter(daily, Date_Local == dates[j])
    #if (nrow(daily2 == 0)) next
    
    monitors <- left_join(monitor_pts, daily2, by="monitor_id") %>% 
      as("Spatial")
    
    monitors <- monitors[!is.na(monitors@data$mean),]
    monitors@data$mean <- ifelse(is.infinite(monitors@data$mean), NA,
                                 monitors@data$mean)
    summary(monitors@data$mean)
    
    hist(monitors@data$mean)
    qqnorm(monitors@data$mean);qqline(monitors@data$mean, col=2)
    
    if (nrow(monitors) > 3) {
      data_norm_test <- shapiro.test(monitors@data$mean)
    } else {
      data_norm_test <- NA
    }
    
    #' Some IDW estimates for comparison
    idw_pwr2 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 2)
    idw_pwr2.5 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 2.5)
    idw_pwr3 <- idw(mean ~ 1, monitors, krige_pts_sp, idp = 3)
    
    #' if data are not normally distributed (based on Shapiro Wilk test), 
    #' use log-transformation-- this can sometimes help, but not always
    #' can change this criterion if needed
    if(all(!is.na(data_norm_test))) {
      if(data_norm_test$p.value < 0.05) {
        monitors@data$mean <- log(monitors@data$mean)
        monitors@data$mean <- ifelse(is.infinite(monitors@data$mean), NA,
                                     monitors@data$mean)
        monitors <- monitors[!is.na(monitors@data$mean),]
        hist(monitors@data$mean)
        qqnorm(monitors@data$mean);qqline(monitors@data$mean, col=2)
      }
    }
    
    #' Kriging using gstat
    #' First, fit the empirical variogram
    vgm <- variogram(mean ~ 1, monitors, cutoff = c_dist)
    # plot(vgm)
    
    #' Second, fit the model
    
    #' ERROR HANDLING
    fit_error <- tryCatch(
      fit.variogram(vgm, model=vgm(all_models),
                    fit.kappa = seq(.3,5,.01)),
      error = function(e) e
    )
    
    range_check <- "range" %in% colnames(fit_error)
    fit_method <- 7
    
    if(range_check == F) {
      fit_error <- tryCatch(
        fit.variogram(vgm, model=vgm(all_models),
                      fit.kappa = seq(.3,5,.01), fit.method = 6),
        error = function(e) e
      )
      fit_method <- 6
    } 
    
    range_check <- "range" %in% colnames(fit_error)
    range_val_check <- ifelse(range_check == F, F, 
                              fit_error$range[length(fit_error$range)] > 0)
    error_check <- !inherits(fit_error, "error")
    
    if(range_check == T & range_val_check == T & error_check == T) {
      
      vgm_fit <- fit.variogram(vgm, model=vgm(all_models),
                               fit.kappa = seq(.3,5,.01), fit.method = fit_method)
      
      
      model <- as.character(vgm_fit$model)[nrow(vgm_fit)]
      
      jpeg(filename = paste0(here::here("Figures/Kriging"), "/",
                             gsub(".csv", "", krige_files[i]),
                             "_", dates[j], ".jpeg"))
      plot(vgm, vgm_fit)
      dev.off()
      
      #' Third, krige
      ok_result <- krige(mean ~ 1, monitors, krige_pts_sp, vgm_fit)
      
      #' Format kriged data
      #' if data weren't normal, back-transform to original units
      #' need to apply a correction (See Oliver and Webster 2007, PAGE 185)
      #' https://books.google.com/books?hl=en&lr=&id=WBwSyvIvNY8C&oi=fnd&pg=PR5&ots=CCLmSNqK1c&sig=lFZanxv2eVSKec6nPdESzuIFrA4#v=onepage&q&f=false
      #' A back-transformed variance estimate for OK cannot be calculated because
      #' the mean is not known (page 185)
      if(all(!is.na(data_norm_test))) {
        if(data_norm_test$p.value < 0.05) {
          ok_result$var1.pred <- exp(ok_result$var1.pred + (0.5*ok_result$var1.var))
          ok_result$var1.var <- NA
        }
      }
      
    } else {
      ok_result <- data.frame(var1.pred = rep(NA, nrow(krige_pts_sp)),
                              var1.var = rep(NA, nrow(krige_pts_sp)))
      model <- NA
    }
    
    #' Summary data frame
    temp <- data.frame(id = krige_pts_sp@data$id,
                       pollutant = str_replace(krige_files[i], ".csv", ""),
                       date = dates[j],
                       conc_pred_idw_pwr2 = idw_pwr2$var1.pred,
                       conc_pred_idw_pwr2.5 = idw_pwr2.5$var1.pred,
                       conc_pred_idw_pwr3 = idw_pwr3$var1.pred,
                       conc_pred_ok = ok_result$var1.pred,
                       conc_var_ok = ok_result$var1.var)
    krige_data <- bind_rows(krige_data, temp)
    
    #' Fourth, leave-one out cross validation
    if(range_check == T & range_val_check == T & error_check == T) {
      cv_result <- krige.cv(mean ~ 1, monitors, vgm_fit)
      summary(cv_result)
      
      if(sum(is.na(cv_result$var1.pred)) == 0) {
        hist(cv_result$residual)
        qqnorm(cv_result$residual);qqline(cv_result$residual, col=2)
        
        if(nrow(monitors) >= 3) {
          cv_res_norm_test <- shapiro.test(cv_result$residual)
        } else {
          cv_res_norm_test <- NA
        }
        
        #' Data frames of cross-validation and diagnostic results
        
        #' See Li and Heap for a nice explanation of diagnostics
        #' https://pdfs.semanticscholar.org/686c/29a81eab59d7f6b7e2c4b060b1184323a122.pdf
        #' Mean error (same units as pollutants), measures bias should be small
        #' RMSE = root mean squared error (same units as pollutant), should be small
        #' cor_obs_pred = correlation between observed and predicted, should be 1
        #' cor_pred_res = correlation between predicted and residual, should be 0
        cv_compare <- compare.cv(list(krige.cv_output = cv_result))
        cv_result <- as.data.frame(cv_result) %>% 
          mutate(pollutant = str_replace(krige_files[i], ".csv", ""),
                 date = dates[j])
        cv_data <- bind_rows(cv_data, cv_result)
        
        temp2 <- data.frame(pollutant = str_replace(krige_files[i], ".csv", ""),
                            date = dates[j],
                            log_transformed = ifelse(all(is.na(data_norm_test)), F, 
                                                     data_norm_test$p.value < 0.05),
                            monitor_n = nrow(monitors),
                            monitor_mean = mean(monitors$mean, na.rm=T),
                            monitor_min = min(monitors$mean, na.rm=T),
                            monitor_max = max(monitors$mean, na.rm=T),
                            model = ifelse(exists("model"),model, NA),
                            modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                            modeled_min = min(ok_result$var1.pred, na.rm=T),
                            modeled_max = max(ok_result$var1.pred, na.rm=T),
                            mean_error = unname(unlist(cv_compare[1,1])),
                            me_mean = unname(unlist(cv_compare[2,1])),
                            msne = unname(unlist(cv_compare[5,1])),
                            rmse = unname(unlist(cv_compare[8,1])),
                            cor_obs_pred = unname(unlist(cv_compare[6,1])),
                            cor_pred_res = unname(unlist(cv_compare[7,1])),
                            data_norm_test_p = ifelse(is.na(cv_res_norm_test), NA, 
                                                      cv_res_norm_test$p.value),
                            cv_res_norm_test_p = ifelse(is.na(cv_res_norm_test), NA, 
                                                        cv_res_norm_test$p.value))
        cv_diagnostics <- bind_rows(cv_diagnostics, temp2)
        
        rm(vgm, vgm_fit, cv_result, cv_compare, temp2)
      }
    } else {
      temp2 <- data.frame(pollutant = str_replace(krige_files[i], ".csv", ""),
                          date = dates[j],
                          log_transformed = ifelse(all(is.na(data_norm_test)), F, 
                                                   data_norm_test$p.value < 0.05),
                          monitor_n = nrow(monitors),
                          monitor_mean = mean(monitors$mean, na.rm=T),
                          monitor_min = min(monitors$mean, na.rm=T),
                          monitor_max = max(monitors$mean, na.rm=T),
                          model = ifelse(exists("model"),model, NA),
                          modeled_mean = NA,
                          modeled_min = NA,
                          modeled_max = NA,
                          model = model,
                          modeled_mean = mean(ok_result$var1.pred, na.rm=T),
                          modeled_min = min(ok_result$var1.pred, na.rm=T),
                          modeled_max = max(ok_result$var1.pred, na.rm=T))
      cv_diagnostics <- bind_rows(cv_diagnostics, temp2)
      rm(temp2)
    }
    
    rm(daily2, monitors, model, ok_result, temp)
  }
  #' Write out results
  
  write_csv(krige_data, here::here("Results", aqs_krige_name))
  write_csv(cv_data, here::here("Results", aqs_cv_name))
  write_csv(cv_diagnostics, here::here("Results", aqs_diagnostics_name))
}

#' #' clean diagnostics files
#' diag_files <- list.files(here::here("Results"), 
#'                          pattern = paste0(grid_name2, "_Diagnostics"))
#' 
#' for (diag in 1:length(diag.files)) {
#'   temp <- read_csv(here::here("Results", diag_files[diag])) %>%
#'     distinct()
#'   write_csv(temp,here::here("Results", diag_files[diag]))
#' }


