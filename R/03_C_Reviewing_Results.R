#' Checking out results
#' 


library(tidyverse)

pm_2011 <- read_csv(here::here("Results", "daily_88101_2011_Kriged.csv")) %>%
  select(GEOID, date, idw_pm = conc_pred_idw_pwr2, ok_pm = conc_pred_ok)
pm_2012 <- read_csv(here::here("Results", "daily_88101_2012_Kriged.csv")) %>%
  select(GEOID, date, idw_pm = conc_pred_idw_pwr2, ok_pm = conc_pred_ok)
pm_2013 <- read_csv(here::here("Results", "daily_88101_2013_Kriged.csv")) %>%
  select(GEOID, date, idw_pm = conc_pred_idw_pwr2, ok_pm = conc_pred_ok)
pm_2014 <- read_csv(here::here("Results", "daily_88101_2014_Kriged.csv")) %>%
  select(GEOID, date, idw_pm = conc_pred_idw_pwr2, ok_pm = conc_pred_ok)

pm <- bind_rows(pm_2011, pm_2012, pm_2013, pm_2014)

# o3_2011 <- read_csv(here::here("Results", "daily_44201_2011_Kriged.csv")) %>%
#   select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
# o3_2012 <- read_csv(here::here("Results", "daily_44201_2012_Kriged.csv")) %>%
#   select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
# o3_2013 <- read_csv(here::here("Results", "daily_44201_2013_Kriged.csv")) %>%
#   select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
# o3_2014 <- read_csv(here::here("Results", "daily_44201_2014_Kriged.csv")) %>%
#   select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)

o3_2011 <- read_csv(here::here("Results", "8hour_44201_2011_Kriged.csv")) %>%
  select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
o3_2012 <- read_csv(here::here("Results", "8hour_44201_2012_Kriged.csv")) %>%
  select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
o3_2013 <- read_csv(here::here("Results", "8hour_44201_2013_Kriged.csv")) %>%
  select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)
o3_2014 <- read_csv(here::here("Results", "8hour_44201_2014_Kriged.csv")) %>%
  select(GEOID, date, idw_o3 = conc_pred_idw_pwr2, ok_o3 = conc_pred_ok)

o3 <- bind_rows(o3_2011, o3_2012, o3_2013, o3_2014)

ap <- left_join(pm, o3, by = c("GEOID", "date"))

summary(ap$ok_pm)
summary(ap$ok_o3)

cor(ap$idw_pm, ap$ok_pm, use = "complete.obs")
cor(ap$idw_o3, ap$ok_o3, use = "complete.obs")

cor(ap$idw_pm, ap$idw_o3, use = "complete.obs")
cor(ap$ok_pm, ap$ok_o3, use = "complete.obs")

cor(ap$idw_pm, ap$ok_o3, use = "complete.obs")

library(lubridate)
missing_pm_ok <- filter(ap, is.na(ok_pm)) %>% 
  mutate(month = month(date))
summary(missing_pm_ok)
barplot(table(missing_pm_ok$month))


#' Checking Wande's code
for_wande <- "C:/Users/semarten/OneDrive for Business/Research/For Wande/"

pm_2011 <- read_csv(paste0(for_wande, "daily_88101_2011_Kriged.csv")) %>% 
  select(-conc_var_ok) %>% 
  mutate(conc_pred_ok = as.numeric(conc_pred_ok))

o3_2011 <- read_csv(paste0(for_wande, "daily_44201_2011_Kriged.csv")) %>% 
  select(-conc_var_ok) %>% 
  mutate(conc_pred_ok = as.numeric(conc_pred_ok))


