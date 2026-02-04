#' @export
check_arbitrage <- function(odd1, odd2){
  
  stopifnot(
    "Odd 1 must be numeric." = is.numeric(odd1) && !is.na(odd1),
    "Odd 2 must be numeric." = is.numeric(odd2) && !is.na(odd2)
  )
  
  stopifnot(
    "Odd 1 must be >= 0." = odd1 >= 0,
    "Odd 2 must be >= 0." = odd2 >= 0
  )
  
  
  rhs = odd1/(odd1 - 1)
  
  ifelse(odd2 > rhs, T, F)
}
