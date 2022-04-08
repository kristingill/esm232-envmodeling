#' Almond Yield
#'
#' This function computes almond yield based on climate variables
#' @param Tn minimum temperature in February (degrees C)
#' @param p precipitation (mm) in January
#' @param a (default = -0.015)
#' @param b (default = -0.0046)
#' @param c (default = -0.07)
#' @param d (default = 0.0043)
#' @param e (default = 0.28)
#' @param year
#' @author Yutian Fang, Kristin Gill, and Grace Lewin
#' @return almond yield (ton/acre)


almond_yield = function(year, Tn, p, a = 0.015, b = 0.0046, c = 0.07, d = 0.0043, e = 0.28){
  
  for (i in year) {
    almond_yield = -a*Tn - b*Tn^2 - c*p + d*p^2 + e
  }
  
  almond_yield <- as.data.frame(almond_yield) %>% 
    mutate(year = year)
  
  return(almond_yield)
}

