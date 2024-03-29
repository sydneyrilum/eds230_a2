#' Profit Model for Almond Yield Anomaly
#'  
#' This model computes the profit anomaly for almond yield anomalies each year in the range of years inputted by the user as well as the total profit anomaly summed over all years.
#'  
#' @param almond_anom dataframe output from the almond_yield() function with column headers `year` and `almond_anomaly` (units = tons/acre)
#' @param acres number of acres on almond farm 
#' @param price cost of each unit of almond yield anomaly (units = US dollars/ton of almonds, default = 2.00)
#' @param discount discount rate (units = %, default = 0.12)
#' @return profit for almond yield anomalies summed across all years (units = US dollars)
#' @authors Sydney Rilum, Juliet Cohen, Shale Hunter

# define the function arguments, setting default values for `price` and `discount` 
profit <- function(df, acres, price = 2.00, discount = 0.12) {
  
  # pull data from the columns in the dataframe output from almond_anomaly function, called 'almond_anom', or from user's almond anomaly dataframe with the same column names
  year = (seq_along(df$year))
  time = (year - year[1])
  yield = (df$almond_anomaly)
  
  # calculate value as a vector 
  value = price*yield*acres
  
  # net present value: account for discount rate with inflation
  df$profit = value / (1 + discount)**time
  # profit is a list, representing the profit for each year inputted
  
  return(list(year_profit_df = df[, c("year","profit")], 
              sum = sum(df$profit)))
}







