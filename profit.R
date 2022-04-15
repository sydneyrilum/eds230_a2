
#' Profit Model for Almond Yield Anomaly
#'  
#'  This model computes the price anomaly for a yield of almonds
#'  
#' @param almond_anom dataframe output from the almond_yield() function with column headers `year` and `almond_anomaly` (tons/acre)
#' @param price cost of each unit of almond_anomaly (US dollars/ton/acre) (default = 2.00)
#' @param discount discount rate (default = 0.12)
#' 
#' @return profit estimate (US dollars)
#' @authors Sydney Rilum, Juliet Cohen, Shale Hunter
#'
#' @examples
#' 


profit <- function(almond_anom, price = 2.00, default = 0.12) {
 
  # pull data from columns in dataframe output from almond_anomaly function, or from dataframe the user inputs with the same columns
  year = (seq_along(almond_anom$year))
  yield = (almond_anom$almond_anomaly)
  
  # calculate price as a vector 
  #value = ()
  value = price*yield
  
  # net present value: account for discount rate with inflation
  profit = value / (1 + discount)**year
  
  # calculate total profit for all years
  total_profit <- sum(profit)
  
  return(total_profit)
}


# test function outside

#almond_anom <- almond_anomaly(data = user_data)
price = 2.00
discount = 0.12
year = (seq_along(almond_anom$year))
year
yield = (almond_anom$almond_anomaly)

# calculate price as a vector 
#value = ()
value = price*yield
value
# net present value: account for discount rate with inflation
profit = value / (1 + discount)**year # might need to reverse years, ask other groups
sum(profit)






