library(testthat)
box::use(../../functions/post_bet_arbitrage_calculation)

test_that("max_hedge_stake computes breakeven hedge stake", {
  expect_equal(
    post_bet_arbitrage_calculation$max_hedge_stake(stake_1 = 100, odd_1 = 2),
    100
  )
})

test_that("min_hedge_odds computes breakeven hedge odds", {
  expect_equal(
    post_bet_arbitrage_calculation$min_hedge_odds(stake_1 = 100, stake_2 = 50),
    (100 + 50) / 50
  )
})

test_that("max_hedge_stake_pct enforces impossible profit percentages", {
  expect_error(
    post_bet_arbitrage_calculation$max_hedge_stake_pct(stake_1 = 100, odd_1 = 1.5, p = 0.6),
    "Profit % impossible",
    fixed = TRUE
  )
})

test_that("min_hedge_odds_pct_absolute enforces impossible profit percentages", {
  expect_error(
    post_bet_arbitrage_calculation$min_hedge_odds_pct_absolute(odd_1 = 1.5, p = 0.6),
    "Profit % impossible",
    fixed = TRUE
  )
})

