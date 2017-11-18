# Contains tests for the CompletingMatrices.R file in the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expected.

library(dplyr)
library(byname)
library(testthat)


###########################################################
context("utilities")
###########################################################
m <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))

test_that("make_list works as expected", {
  expect_equal(make_list(m, n = 1), list(m))
  expect_equal(make_list(m, n = 2), list(m, m))
  expect_equal(make_list(m, n = 5), list(m, m, m, m, m))
  l1 <- list(c(1,2), c(3,4))
  # Expect c(1,2), c(3,4), c(1,2), c(3,4)
  expect_equal(make_list(l1, n = 4), c(l1, l1))
  # Expect [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)]
  expect_equal(make_list(l1, n = 4, lenx = 1), list(l1, l1, l1, l1))
  # Expect a warning, because length isn't a multiple
  expect_warning(make_list(l1, n = 3), "n not evenly divisible by length\\(x\\)")
  
  m1 <- matrix(1:4, nrow = 2)
  m2 <- m + 100
  l2 <- list(m1, m2)
  expect_equal(make_list(l2, n = 4), c(l2, l2))
  expect_warning(make_list(l2, n = 1), "n not evenly divisible by length\\(x\\)")
  expect_warning(make_list(l2, n = 5), "n not evenly divisible by length\\(x\\)")
  
  l3 <- list(c("r10", "r11"), c("c10", "c11"))
  expect_equal(make_list(l3, n = 2), l3) # Confused by x being a list
  expect_equal(make_list(l3, n = 2, lenx = 1), list(l3, l3)) # Fix by setting lenx = 1
})
  
