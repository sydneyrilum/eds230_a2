#' Almond Yield Anomaly Model
#' 
#' The model computes annual almond crop yield anomalies from climate input data (precipitation and temperature).
#' 
#' @param data dataframe containing daily precipitation (mm) and minimum and maximum temperatures (C) over a series of months/years
#' @param temp_param_1 coefficient (default = -0.015)
#' @param temp_param_2 coefficient (default = 0.0046)
#' @param precip_param_1 coefficient (default = 0.07)
#' @param precip_param_2 coefficient (default = 0.0043)
#' @param constant (default = 0.28)
#' @return yield anomaly (ton/acre)
#' @authors Juliet Cohen, Shale Hunter, Sydney Rilum
#' @references Model sourced from [Lobell et al. 2006](https://naomitague.github.io/ESM232_course/assignments/lobell.2006.pdf)

# almond yield anomaly function, with parameters as defaults and data to be entered by the user
almond_anomaly = function(data, 
                    temp_param_1 = -0.015, 
                    temp_param_2 = 0.0046, 
                    precip_param_1 = 0.07, 
                    precip_param_2 = 0.0043,
                    constant = 0.28) {
  
  # load necessary packages
  library(tidyverse)
  options(scipen = 999)
  
  # wrangle data to reference summary statistics, such as the minimum temperature in the month of February each year
  grouped_data <- data %>% 
    # remove 1988 for the almond anomaly calculation, which requires January and February data that is not present for 1988
    filter(year != 1988) %>% 
    # group by month and year to compare temperature and precipitation for the same months over many years
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  
  print("The data is now wrangled. Starting anomaly calculation.")
  
  # create an empty dataframe to store the yield anomaly output
  yield_anomaly = data_frame()
 
   # iterate the model over all years 
  for (i in seq.int(from = min(grouped_data$year), to = max(grouped_data$year), by = 1)) {
    
    # calculate the almond anomaly for each year from the equation sourced from Lobell et al. 2006
    yield_i <- ((temp_param_1 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) 
                - (temp_param_2 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]**2) 
                - (precip_param_1 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) 
                + (precip_param_2 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]**2) 
                + constant)
    
    # create a list from the year and the yield for that year
    row = c(i, yield_i)
    
    # append the list as a row in the dataframe `yield anomaly`
    yield_anomaly = rbind(yield_anomaly, row)
  }
  
  # assign column names
  colnames(yield_anomaly) = c("year", "almond_anomaly")
  
  # print the dataframe created from all years' almond anomalies
  return(yield_anomaly)
}  


