box::use(./utils)

#' Check whether an arbitrage opportunity exists
#'
#' Validates two decimal odds, computes the sum of their implied
#' probabilities via \code{utils$calculate_impl_prob_sum()}, and
#' returns \code{TRUE} if the sum is less than 1 (arbitrage exists),
#' otherwise \code{FALSE}.
#'
#' @param odd1 Numeric, first decimal odd (must be non-negative).
#' @param odd2 Numeric, second decimal odd (must be non-negative).
#'
#' @return Logical scalar: \code{TRUE} if arbitrage exists, else \code{FALSE}.
#'
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
