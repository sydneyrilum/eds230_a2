# only almond loop

library(tidyverse)
options(scipen = 999)
data <- read.table("clim.txt", sep = " ", header = T)

# create function, with parameters as defaults and data to be entered by the user, with no default data
almond_anomaly = function(data, 
                    temp_param_1 = -0.015, 
                    temp_param_2 = 0.0046, 
                    precip_param_1 = 0.07, 
                    precip_param_2 = 0.0043,
                    constant = 0.28) {
  # wrangle data to reference summary statistics, such as the minimum temperatures of all Februarys
  grouped_data <- data %>% 
    # remove 1988 for the almond anomaly calculation, which requires January and February data that is not present for 1988
    filter(year != 1988) %>% 
    # group by month and year to compare temperature and precipitation for the same months over many years
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  print("The data is now wrangled. Starting anomaly calculation.")
  # create empty dataframe which will hold the output yield anoomaly
  yield_anomaly = data_frame()
  # iterate the model over all years 
  for (i in seq.int(from = min(grouped_data$year), to = max(grouped_data$year), by = 1)) {
    # calculate the almond anomaly for each year from the equation provided from Lobell et al. 2006
    yield_i <- ((temp_param_1 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) 
                - (temp_param_2 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]**2) 
                - (precip_param_1 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) 
                + (precip_param_2 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i] 
                   * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) 
                + constant)
    # create a list from the year and the yield for that year
    row = c(i, yield_i)
    # append the list as a row in the dataframe `yield anomaly`
    yield_anomaly = rbind(yield_anomaly, row)
  }
  # assign column names
  colnames(yield_anomaly) = c("year", "almond_anomaly")
  # print the daatframe created from all years' almond anomalies
  return(yield_anomaly)
}  

almond_anomaly(data = data)


