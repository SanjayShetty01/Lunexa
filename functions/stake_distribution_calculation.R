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