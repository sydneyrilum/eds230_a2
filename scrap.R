
library(tidyverse)
data <- read.table("clim.txt", sep = " ",header = T)


aanomaly = function(data, daily = FALSE, crop) {
  yield_anomaly = list()
  
 
    grouped_data <- data %>% 
      group_by(month, year) %>% 
      summarize(mean_tmin = mean(tmin_c),
                mean_tmax = mean(tmax_c),
                mean_precip = mean(precip))
  

    for (i in seq.int(from = min(unique(grouped_data$year)), to = max(unique(grouped_data$year)), by = 1)) {
      
    yield_anomaly[i] <- ((-0.015 * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i]) - 
                             (0.0046 * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i] * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i]) -
                             (0.07 * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i]) +
                             (0.0043 * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i] * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i]) +
                             0.28
      )
      print(paste0("yield anomaly for ", i, " is ", yield_anomaly[i]))
    }

  return(yield_anomaly)
}


# Testing
#data = read.csv()
aanomaly(data = data, crop = "almonds")



# test group data
grouped_data_test <- data %>% 
  group_by(month, year) %>% 
  summarize(mean_tmin = mean(tmin_c),
            mean_tmax = mean(tmax_c),
            mean_precip = mean(precip))
