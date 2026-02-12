#' Calculate optimal stake for a single outcome
#'
#' Given decimal odds for one outcome, a total stake budget, and the
#' sum of implied probabilities across all outcomes, compute the amount
#' to stake on this particular outcome.
#'
#' @param odd Numeric decimal odd for this outcome (must be non-negative).
#' @param total_stake Numeric total stake to be distributed (must be non-negative).
#' @param impl_prob Numeric sum of implied probabilities across all outcomes.
#'
#' @return A numeric scalar giving the stake to place on this outcome.
#'
#' @export
calculate_stake <- function(odd, total_stake, impl_prob) {
  
  stopifnot(
    "Odd must be numeric." = is.numeric(odd) && !is.na(odd),
    "Total Stake must be numeric." = is.numeric(total_stake) && 
      !is.na(total_stake)
    )
  
  stopifnot(
    "Odd must be >= 0." = odd >= 0,
    "Total Stake must be >= 0." = total_stake >= 0
  )
  
  return(total_stake/(odd * impl_prob))
}