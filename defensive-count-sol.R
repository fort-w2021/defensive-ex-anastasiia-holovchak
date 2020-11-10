library(tidyverse)
library(checkmate)
library(testthat)

count_them <- function(supposedly_a_count) {
  checkmate::assert_number(supposedly_a_count, lower = 0, finite = TRUE)
  if (!checkmate::test_count(supposedly_a_count)) {
    warning(
      "rounding ", supposedly_a_count,
      " to the nearest integer."
    )
    if (0.5 - abs(supposedly_a_count - round(supposedly_a_count)) < .Machine$double.eps) {
      warning("Possible rounding error: machine precision may be undercut.
              Check if the number in the first warning equivalent to your input.")
    }
    supposedly_a_count <- round(supposedly_a_count)
  }
  
  return(as.integer(supposedly_a_count))
}

testthat::test_file("test-defensive-count.R")

