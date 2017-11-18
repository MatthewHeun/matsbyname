# Contains tests for the CompletingMatrices.R file in the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(byname)
library(testthat)


###########################################################
context("utilities")
###########################################################
m <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
#' 
#' 
#' m <- matrix(1:4, nrow = 2)
#' l <- list(m, m+100)
#' make_list(l, n = 4)
#' make_list(l, n = 1) # Warning because l is trimmed.
#' make_list(l, n = 5) # Warning because length(l) (i.e., 2) not evenly divisible by 5
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2) # Confused by x being a list
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2, lenx = 1) # Fix by setting lenx = 1

test_that("make_list", {
  expect_equal(make_list(m, n = 1), list(m))
  expect_equal(make_list(m, n = 2), list(m, m))
  expect_equal(make_list(m, n = 5), list(m, m, m, m, m))
  l <- list(c(1,2), c(3,4))
  # Expect c(1,2), c(3,4), c(1,2), c(3,4)
  expect_equal(make_list(l, n = 4), c(l, l))
  # Expect [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)]
  expect_equal(make_list(l, n = 4, lenx = 1), list(l, l, l, l))
  # Expect a warning, because length isn't a multiple
  expect_warning(make_list(l, n = 3), "n not evenly divisible by length\\(x\\)")
})
  
