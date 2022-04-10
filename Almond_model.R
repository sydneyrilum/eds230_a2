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
#data <- read.csv("clim.txt", )
library(tidyverse)
data <- read.table("clim.txt", sep = " ",header = T)
colnames(data)
unique(data$year)


aanomaly = function(data, daily = FALSE, crop) {
  yield_anomaly = list()
  
  if (daily == TRUE) {
    grouped_data <- data %>% 
      group_by(month, year) %>% 
      summarize(mean_tmin = mean(tmin_c),
                mean_tmax = mean(tmax_c),
                mean_precip = mean(precip))
  }
  else (grouped_data <- data)
  
  if (!crop %in% c("wine grapes", "almonds", "table grapes", "oranges", "walnuts", "avocados")) {
    stop("Invalid crop. Crop must be one of: wine grapes, almonds, table grapes, oranges, walnuts, avocados.")
  }
  else if (crop == "wine grapes") {
#    yield_anomaly <- (2.65 * grouped_data$mean_tmin[grouped_data$month == 4])
  }
  else if (crop == "almonds") {
    for (i in seq.int(from = min(unique(grouped_data$year)), to = max(unique(grouped_data$year)), by = 1)) {
    
    yield_anomaly[i] <- ((-0.015 * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i]) - 
      (0.0046 * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i] * grouped_data$mean_tmin[grouped_data$month == 2 & grouped_data$year == i]) -
      (0.07 * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i]) +
      (0.0043 * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i] * grouped_data$mean_precip[grouped_data$month == 1 & grouped_data$year == i]) +
      0.28
      )
    print(paste0("yield anomaly for ", i, " is ", yield_anomaly[i]))
    }
  }
  else if (crop == "table grapes") {
    
  }
  else if (crop == "oranges") {
    
  }
  else if (crop == "walnuts") {
    
  }
  else if (crop == "avocados") {
    
  }
  
  return(yield_anomaly)
}


# Testing
#data = read.csv()
aanomaly(data = data, daily = TRUE, crop = "almonds")













# test group data
grouped_data_test <- data %>% 
  group_by(month, year) %>% 
  summarize(mean_tmin = mean(tmin_c),
            mean_tmax = mean(tmax_c),
            mean_precip = mean(precip))












# checking for loop 
for (i in 1:12) {
  data %>% 
    group_by(year) %>% 
    summarize(mean_tmin = mean(data$tmin_c[data$month == i]),
              mean_tmax = mean(data$tmax_c[data$month == i]),
              mean_precip = mean(data$precip[data$month == i]))
  
}


# checking grouping
grouped_means <- data %>% 
  group_by(month, year) %>% 
  summarize(mean_tmin = mean(tmin_c),
            mean_tmax = mean(tmax_c),
            mean_precip = mean(precip))






