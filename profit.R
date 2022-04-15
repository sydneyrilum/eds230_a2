
#' Profit Model for Almond Yield Anomaly
#'  
#'  This model computes the price anomaly for a yield of almonds
#'  
#' @param almond_anom dataframe output from the almond_yield() function with column headers `year` and `almond_anomaly` (tons/acre)
#' @param acres number of acres on almond farm 
#' @param price cost of each unit of almond yield anomaly (US dollars/ton of almonds) (default = 2.00)
#' @param discount discount rate (default = 0.12)
#' 
#' @return profit in US dollars for almond yield anomalies summed across all years 
#' 
#' @authors Sydney Rilum, Juliet Cohen, Shale Hunter
#'
#' @examples
#' 


profit <- function(almond_anom, acres, price = 2.00, discount = 0.12) {
 
  # pull data from columns in dataframe output from almond_anomaly function, or from dataframe the user inputs with the same columns
  year = (seq_along(almond_anom$year))
  yield = (almond_anom$almond_anomaly)
  
  # calculate value as a vector 
  value = price*yield*acres
  
  # net present value: account for discount rate with inflation
  profit = value / (1 + discount)**year
  
  # calculate total profit for all years
  #total_profit <- sum(profit)
  
  return(profit)
}







