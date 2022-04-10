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

aanomaly = function(data, daily = FALSE, crop) {
  if (daily = TRUE) {
    for (i in 1:12) {
    data[[month_tmin_c]] <- mean(data$tmin_c[data$month == i])
    data[[month_tmax_c]] <- mean(data$tmax_c[data$month == i])
    data[[month_precip]] <- mean(data$precip[data$month == i])
    }
    return(data)
  }
  
  if (!crop %in% c("wine grapes", "almonds", "table grapes", "oranges", "walnuts", "avocados")) {
    stop("Invalid crop. Crop must be one of: wine grapes, almonds, table grapes, oranges, walnuts, avocados.")
  }
  else if (crop = "wine grapes") {
    
  }
  else if (crop = "almonds") {
    
  }
  else if (crop = "table grapes") {
    
  }
  else if (crop = "oranges") {
    
  }
  else if (crop = "walnuts") {
    
  }
  else if (crop = "avocados") {
    
  }
  
  return(data)
}


# Testing
data = read.csv()
aanomaly(data = data, )