library(testthat)

context("checking lag")

test_that("does the right thing for vectors", {
  expect_identical(lag(c(1, 2, 3), 2), c(NA, NA, 1))
  expect_identical(lag(c("a", "bc", "efg"), 1), c(NA, "a", "bc"))
  expect_identical(lag(c(TRUE, FALSE, FALSE, TRUE), 2), c(NA, NA, TRUE, FALSE))
  expect_identical(is.na(lag(c(234.34, 543.54), 1)), c(TRUE, FALSE))
})

test_that("produces errors", {
  expect_error(lag(c(11, 1.1), 2.2)) # n not a count
  expect_error(lag(list(x = c(1, 2), y = c("nice", "try")))) # for lists
  expect_error(lag(matrix(c(1, 2, 3, 4), ncol = 2))) # for matrices
  expect_error(lag(data.frame(c(1, 2), c("a", "bc")))) # for data.frames
  expect_error(lag(array(c(1, 4, 5, 3, 5, 3, 6, 34), dim = c(2, 2, 2)))) # for arrays
  expect_error(lag(c())) # for NULL
  expect_error(lag(1, 3)) # lag length greater than vector length
})
