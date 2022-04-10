# only almond loop

library(tidyverse)
data <- read.table("clim.txt", sep = " ", header = T)

aanomaly = function(data) {
  
  grouped_data <- data %>% 
    filter(year != 1988) %>% 
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  print("data is clean")
  
  yield_anomaly = data_frame()
  # yield_anomaly = data.frame(min(grouped_data$year):max(grouped_data$year), NA)
  
  for (i in seq.int(from = min(grouped_data$year), to = max(grouped_data$year), by = 1)) {
    
    yield_i <- ((-0.015 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) - (0.0046 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i] * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) - (0.07 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) + (0.0043 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i] * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) + 0.28)
    row = c(i, yield_i)
    yield_anomaly = rbind(yield_anomaly, row)
    
  }
  
  colnames(yield_anomaly) = c("year", "anomaly")
  return(yield_anomaly)
}  

aanomaly(data = data)
