#' Almond model
#' 
#' data is the data.frame used as input data
#' daily is the time aggregation; default is false (assumed to be monthly)
#' 
#' 
#' 
#' 
#' 
#' 
#' 


library(tidyverse)
climate_data <- read.table("clim.txt", sep = " ", header = T)


# Almond Yield Function
almond_yield_anomaly = function(tmin_feb,
                        total_precip_jan,
                        temp_coef1 = -0.015,
                        temp_coef2 = -0.0046,
                        precip_coef1 = -0.07,
                        precip_coef2 = 0.0043,
                        intercept = 0.28) {
  
  grouped_data <- climate_data %>% 
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  
  tmin_feb <- (grouped_data %>% 
    filter(month == 2))$tmin
  
  total_precip_jan <- (grouped_data %>% 
    filter(month == 1))$total_precip
  
  yield_anomaly = (temp_coef1*tmin_feb + 
                     temp_coef2*(tmin_feb)^2 +
                     precip_coef1*total_precip_jan +
                     precip_coef2*(total_precip_jan)^2 +
                     intercept)
  
  return(yield_anomaly)
}
  

# Run function for each water year in climate data
yield = cbind(unique(climate_data$wy), almond_yield_anomaly(climate_data))










# for reference
grouped_data <- data %>% 
  group_by(year, month) %>% 
  summarize(tmin = min(tmin_c),
            tmax = min(tmax_c),
            total_precip = sum(precip))

temp_min_feb <- (grouped_data %>% 
                   filter(month == 2))$tmin

total_precip_jan <- (grouped_data %>% 
                       filter(month == 1))$total_precip




