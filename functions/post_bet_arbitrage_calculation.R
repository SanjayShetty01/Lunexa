#' Maximum hedge stake for breakeven
#'
#' Given the original stake and decimal odds of the first bet, compute
#' the maximum amount that can be placed on the hedge bet such that the
#' worst-case outcome is breakeven.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param odd_1 Numeric decimal odd of the original bet.
#'
#' @return Numeric maximum hedge stake.
#'
#' @export
max_hedge_stake <- function(stake_1, odd_1) {
  stake_1 * (odd_1 - 1)
}

#' Minimum hedge odds for breakeven
#'
#' Given the stakes on the original and hedge bets, compute the minimum
#' decimal odds required on the hedge bet so that the overall position
#' is breakeven.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param stake_2 Numeric stake of the hedge bet.
#'
#' @return Numeric minimum decimal odds for the hedge bet.
#'
#' @export
min_hedge_odds <- function(stake_1, stake_2) {
  (stake_1 + stake_2) / stake_2
}

#' Maximum hedge stake for a target profit
#'
#' Compute the maximum hedge stake that still allows achieving at least
#' a given absolute profit, given the original bet stake and odds.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param odd_1 Numeric decimal odd of the original bet.
#' @param profit Numeric desired profit amount.
#'
#' @return Numeric maximum hedge stake compatible with the target profit.
#'
#' @export
max_hedge_stake_for_profit <- function(stake_1, odd_1, profit) {
  stake_1 * (odd_1 - 1) - profit
}

#' Minimum hedge odds for a target profit (given stake)
#'
#' Compute the minimum decimal odds required on the hedge bet in order
#' to lock in a given absolute profit, assuming the hedge stake is fixed.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param stake_2 Numeric stake of the hedge bet.
#' @param profit Numeric desired profit amount.
#'
#' @return Numeric minimum decimal odds for the hedge bet.
#'
#' @export
min_hedge_odds_for_profit <- function(stake_1, stake_2, profit) {
  1 + (stake_1 + profit) / stake_2
}

#' Minimum hedge stake for a target profit (given odds)
#'
#' Compute the minimum hedge stake required to lock in a given absolute
#' profit, assuming the hedge odds are known.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param odd_2 Numeric decimal odd of the hedge bet.
#' @param profit Numeric desired profit amount.
#'
#' @return Numeric minimum hedge stake.
#'
#' @export
min_hedge_stake_for_profit <- function(stake_1, odd_2, profit) {
  (stake_1 + profit) / (odd_2 - 1)
}

#' Absolute minimum hedge odds for a target profit
#'
#' Compute the absolute minimum decimal odds the hedge bet must have to
#' lock in a given profit, based on the original stake and odds.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param odd_1 Numeric decimal odd of the original bet.
#' @param profit Numeric desired profit amount.
#'
#' @return Numeric minimum decimal odds for the hedge bet.
#'
#' @export
min_hedge_odds_absolute <- function(stake_1, odd_1, profit) {
  (stake_1 * odd_1) / (stake_1 * (odd_1 - 1) - profit)
}

#' Maximum hedge stake for a target profit percentage
#'
#' Compute the maximum hedge stake allowed to achieve at least a given
#' profit percentage on the original stake.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param odd_1 Numeric decimal odd of the original bet.
#' @param p Numeric profit percentage (e.g. 0.1 for 10\%).
#'
#' @return Numeric maximum hedge stake.
#'
#' @export
max_hedge_stake_pct <- function(stake_1, odd_1, p) {
  if (odd_1 <= 1 + p) stop("Profit % impossible")
  stake_1 * (odd_1 - 1 - p) / (1 + p)
}

#' Minimum hedge odds for a target profit percentage
#'
#' Compute the minimum hedge odds required to achieve a given profit
#' percentage, for a specified hedge stake.
#'
#' @param stake_1 Numeric stake of the original bet.
#' @param stake_2 Numeric stake of the hedge bet.
#' @param p Numeric profit percentage (e.g. 0.1 for 10\%).
#'
#' @return Numeric minimum decimal odds for the hedge bet.
#'
#' @export
min_hedge_odds_pct <- function(stake_1, stake_2, p) {
  (1 + p) * (stake_1 + stake_2) / stake_2
}

#' Minimum hedge odds (absolute) for a target profit percentage
#'
#' Compute the minimum possible hedge odds that still allow achieving a
#' given profit percentage, based only on the original odds and target
#' percentage.
#'
#' @param odd_1 Numeric decimal odd of the original bet.
#' @param p Numeric profit percentage (e.g. 0.1 for 10\%).
#'
#' @return Numeric minimum decimal odds for the hedge bet.
#'
#' @export
min_hedge_odds_pct_absolute <- function(odd_1, p) {
  if (odd_1 <= 1 + p) stop("Profit % impossible")
  (1 + p) * odd_1 / (odd_1 - 1 - p)
}

