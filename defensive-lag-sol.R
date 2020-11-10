library(tidyverse)
library(checkmate)
library(testthat)

# produces a lag of length n (positions filled with NAs) at the beginning of a vector
# input: vector 'x', lag length 'n'
# output: lagged vector
lag <- function(x, n = 1L) {
  assert_atomic_vector(x, min.len = 1) # checks if x an atomic vector
  assert_count(n) # checks if n a count
  xlen <- length(x)
  if (xlen < n) # didn't use assert_count bc. more informative error message needed
    stop("Lag length greater than vector length")
  c(rep(NA, n), x[seq_len(xlen - n)])
}

path_to_lag <- paste0(here::here(), "/defensive-lag-sol.R")
checklist::checklist(path_to_lag)

testthat::test_file("test-defensive-lag.R")