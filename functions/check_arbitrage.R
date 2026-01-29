check_arbitrage <- function(odd1, odd2){
  
  rhs = odd1/(odd1 - 1)
  
  ifelse(odd2 > rhs, T, F)
}
