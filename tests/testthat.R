if (!requireNamespace("testthat", quietly = TRUE)) {
  install.packages("testthat")
}

library(testthat)

# Run all tests in the tests/testthat directory when this file is sourced.
testthat::test_dir("tests/testthat")

