library(testthat)
box::use(../../functions/stake_distribution_calculation)

test_that("calculate_stake validates inputs", {

  expect_error(
    stake_distribution_calculation$calculate_stake("not numeric", 100, 1),
    "Odd must be numeric",
    fixed = TRUE
  )

  expect_error(
    stake_distribution_calculation$calculate_stake(2, "not numeric", 1),
    "Total Stake must be numeric",
    fixed = TRUE
  )

  expect_error(
    stake_distribution_calculation$calculate_stake(-1, 100, 1),
    "Odd must be >= 0",
    fixed = TRUE
  )

  expect_error(
    stake_distribution_calculation$calculate_stake(2, -100, 1),
    "Total Stake must be >= 0",
    fixed = TRUE
  )
})

test_that("calculate_stake distributes stake correctly", {
  total_stake <- 100
  odd <- 2
  impl_prob <- 1

  expect_equal(
    stake_distribution_calculation$calculate_stake(
      odd = odd,
      total_stake = total_stake,
      impl_prob = impl_prob
    ),
    50,
    tolerance = 1e-12
  )
})

