#' @export
breakeven_odds <- function(odds) {
  odds/(odds - 1)
}

#' @export
hedge_stakes <- function(stake_1, odd_1, odd_2) {
  (stake_1 * (odd_1 - 1)) / odd_2
}

#' @export
profit_odds <- function(stake_1, odd_1, G) {
  (stake_1 * (odd_1 - 1)) / (stake_1 + G)
}

