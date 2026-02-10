#' @export
max_hedge_stake <- function(stake_1, odd_1) {
  stake_1 * (odd_1 - 1)
}

#' @export
min_hedge_odds <- function(stake_1, stake_2) {
  (stake_1 + stake_2) / stake_2
}

# Max hedge stake allowed for profit profit
#' @export
max_hedge_stake_for_profit <- function(stake_1, odd_1, profit) {
  stake_1 * (odd_1 - 1) - profit
}

# Minimum hedge odds for profit profit (given stake)
#' @export
min_hedge_odds_for_profit <- function(stake_1, stake_2, profit) {
  1 + (stake_1 + profit) / stake_2
}

# Minimum hedge stake for profit profit (given odds)
#' @export
min_hedge_stake_for_profit <- function(stake_1, odd_2, profit) {
  (stake_1 + profit) / (odd_2 - 1)
}

# Absolute minimum hedge odds for profit profit
#' @export
min_hedge_odds_absolute <- function(stake_1, odd_1, profit) {
  (stake_1 * odd_1) / (stake_1 * (odd_1 - 1) - profit)
}



max_hedge_stake_pct <- function(stake_1, odd_1, p) {
  if (odd_1 <= 1 + p) stop("Profit % impossible")
  stake_1 * (odd_1 - 1 - p) / (1 + p)
}

min_hedge_odds_pct <- function(stake_1, stake_2, p) {
  (1 + p) * (stake_1 + stake_2) / stake_2
}

min_hedge_odds_pct_absolute <- function(odd_1, p) {
  if (odd_1 <= 1 + p) stop("Profit % impossible")
  (1 + p) * odd_1 / (odd_1 - 1 - p)
}

