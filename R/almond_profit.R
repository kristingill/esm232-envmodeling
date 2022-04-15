#' Almond Yield
#'
#' This function computes profit from selling almonds based on almond yield, price, discount rate, and acres.
#' @param almond_yield (ton/acre)
#' @param discount_rate (default = 0.12)
#' @param price ($/ton)
#' @param acres
#' @param year
#' @author Yutian Fang, Kristin Gill, and Grace Lewin
#' @return almond profit ($)

compute_profit_almond = function(yield, year, price, discount=0.12, acres, base_almonds) {
  
  scen = seq(from=1, to=length(yield))
  
  yearprofit <- data.frame(scen=scen, yield=yield, year=year, base_almonds=base_almonds) %>% 
    mutate(total_almonds = base_almonds + yield)
  
  yearprofit$net <-  yearprofit$total_almonds*price
  
  Yearprofit <- yearprofit %>% 
    mutate(netpre = compute_NPV(value=net, time=year-year[1], discount=discount ))
  
  return(yearprofit)
}
