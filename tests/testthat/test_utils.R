library(testthat)
box::use(../../functions/utils)

test_that("calculate_impl_prob_sum validates numeric, non-negative odds", {

  expect_error(
    utils$calculate_impl_prob_sum("not numeric", 2),
    "Odd 1 must be numeric",
    fixed = TRUE
  )

  expect_error(
    utils$calculate_impl_prob_sum(-1, 2),
    "Odd 1 must be >= 0",
    fixed = TRUE
  )
})

test_that("calculate_impl_prob_sum returns correct sum of implied probabilities", {

  expect_equal(
    utils$calculate_impl_prob_sum(2, 2),
    1.0,
    tolerance = 1e-12
  )

  expect_equal(
    utils$calculate_impl_prob_sum(4, 4),
    0.5,
    tolerance = 1e-12
  )
})

