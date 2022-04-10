# only almond loop

library(tidyverse)
data <- read.table("clim.txt", sep = " ", header = T)

aanomaly = function(data) {
  yield_anomaly = data.frame(1989:2010, NA)
  colnames(yield_anomaly) = c("year", "anomaly")
  
  grouped_data <- data %>% 
    filter(year != 1988) %>% 
    group_by(year, month) %>% 
    summarize(tmin = min(tmin_c),
              tmax = min(tmax_c),
              total_precip = sum(precip))
  print(paste0("data is clean"))
  
  for (i in seq.int(from = min(yield_anomaly$year), to = max(yield_anomaly$year), by = 1)) {
    
    yield_anomaly$anomaly[yield_anomaly$year == i] <- ((-0.015 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) - (0.0046 * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i] * grouped_data$tmin[grouped_data$month == 2 & grouped_data$year == i]) - (0.07 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) + (0.0043 * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i] * grouped_data$total_precip[grouped_data$month == 1 & grouped_data$year == i]) + 0.28)
    print(paste0("yield anomaly for ", i, " is ", yield_anomaly[i]))
  }
}  

aanomaly(data = data)
