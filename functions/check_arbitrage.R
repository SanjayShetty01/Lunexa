box::use(./utils)

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
  
  impl_prob_sum <- utils$calculate_impl_prob_sum(odd1 = odd1, odd2 = odd2)
  
  ifelse(impl_prob_sum < 1 , T, F)
}
