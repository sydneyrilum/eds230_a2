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
# define default arguments
almond_yield_anomaly = function(tmin_feb,
                        total_precip_jan,
                        temp_coef1 = -0.015,
                        temp_coef2 = -0.0046,
                        precip_coef1 = -0.07,
                        precip_coef2 = 0.0043,
                        intercept = 0.28) {
  # wrangle data to reference summary statistics, such as the minimum temperatures of all Februarys  
  grouped_data <- climate_data %>%
    # group by month and year to compare temperature and precipitation for the same months over many years
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  print("The data is now wrangled. Starting anomaly calculation.")
  # calculate minimum February temperature over all years
  tmin_feb <- (grouped_data %>% 
    filter(month == 2))$tmin
  # calculate minimum January temperature over all years
  total_precip_jan <- (grouped_data %>% 
    filter(month == 1))$total_precip
  # calculate the almond anomaly for each year within 1989-2010 using the equation provided from Lobell et al. 2006
  yield_anomaly = (temp_coef1*tmin_feb + 
                     temp_coef2*(tmin_feb)^2 +
                     precip_coef1*total_precip_jan +
                     precip_coef2*(total_precip_jan)^2 +
                     intercept)
  # print the dataframe created from all years' almond anomalies
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




