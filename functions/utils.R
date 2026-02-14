#' Calculate sum of implied probabilities for two odds
#'
#' Given two decimal odds, validates the inputs and returns the sum of
#' their implied probabilities \eqn{1 / odd1 + 1 / odd2}. This is used
#' to determine whether an arbitrage opportunity exists.
#'
#' @param odd1 Numeric, first decimal odd (must be non-negative).
#' @param odd2 Numeric, second decimal odd (must be non-negative).
#'
#' @return A numeric scalar equal to \eqn{1 / odd1 + 1 / odd2}.
#'
#' @export
calculate_impl_prob_sum <- function(odd1, odd2) {
  
  stopifnot(
    "Odd 1 must be numeric." = is.numeric(odd1) && !is.na(odd1),
    "Odd 2 must be numeric." = is.numeric(odd2) && !is.na(odd2)
  )
  
  stopifnot(
    "Odd 1 must be > 0." = odd1 >= 0,
    "Odd 2 must be > 0." = odd2 >= 0
  )
  
  return((1/odd1) + (1/odd2))
}
