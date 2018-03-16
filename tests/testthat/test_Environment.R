

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(parallel)
library(matsbyname)
library(magrittr)
library(testthat)


###########################################################
context("Environment")
###########################################################

test_that("getting and setting mc.cores works as expected", {
  # mc.cores should be 1 out of the box.
  expect_equal(get_mc.cores(), 1)
  # Try setting the value to something else.
  set_mc.cores(4)
  expect_equal(get_mc.cores(), 4)
  # Set back to default number
  set_mc.cores(1)
})


