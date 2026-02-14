library(testthat)
box::use(../../functions/check_arbitrage)

test_that("check_arbitrage validates numeric, non-negative odds", {

  expect_error(
    check_arbitrage$check_arbitrage("not numeric", 2),
    "Odd 1 must be numeric",
    fixed = TRUE
  )

  expect_error(
    check_arbitrage$check_arbitrage(2, "not numeric"),
    "Odd 2 must be numeric",
    fixed = TRUE
  )

  expect_error(
    check_arbitrage$check_arbitrage(-1, 2),
    "Odd 1 must be > 0",
    fixed = TRUE
  )

  expect_error(
    check_arbitrage$check_arbitrage(2, -1),
    "Odd 2 must be > 0",
    fixed = TRUE
  )
})

test_that("check_arbitrage correctly identifies arbitrage opportunities", {
  # Implied probability sum < 1 -> arbitrage exists
  expect_true(check_arbitrage$check_arbitrage(odd1 = 2.1, odd2 = 2.1))

  # Implied probability sum > 1 -> no arbitrage
  expect_false(check_arbitrage$check_arbitrage(odd1 = 1.5, odd2 = 1.5))
})

