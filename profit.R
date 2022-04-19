
#' Profit Model for Almond Yield Anomaly
#'  
#' This model computes the profit anomaly for almond yield anomalies for each year in the range of years given and the total profit anomaly summed over all years
#'  
#' @param almond_anom dataframe output from the almond_yield() function with column headers `year` and `almond_anomaly` (units = tons/acre)
#' @param acres number of acres on almond farm 
#' @param price cost of each unit of almond yield anomaly (units = US dollars/ton of almonds) (default = 2.00)
#' @param discount discount rate (units = US dollars) (default = 0.12)
#' 
#' @return profit for almond yield anomalies summed across all years (units = US dollars)
#' 
#' @authors Sydney Rilum, Juliet Cohen, Shale Hunter
#'
#' @examples
#' 

# define the function arguments, setting default values for `price` and `discount` 
profit <- function(df, acres, price = 2.00, discount = 0.12) {
  
  # pull data from the columns in the dataframe output from almond_anomaly function, called 'almond_anom', or from a user's almond anomaly dataframe with the same column names
  year = (seq_along(df$year))
  time = (year - year[1])
  yield = (df$almond_anomaly)
  
  # calculate value as a vector 
  value = price*yield*acres
  
  # net present value: account for discount rate with inflation
  df$profit = value / (1 + discount)**time
  # profit is a list of 22 values, representing the profit for each year
  
  # calculate total profit for all years
  #tot_profit <- sum(profit)
  
  #return(profit)
  #return(tot_profit)
  return(list(year_profit_df = df[, c("year","profit")], sum = sum(df$profit)))
}







