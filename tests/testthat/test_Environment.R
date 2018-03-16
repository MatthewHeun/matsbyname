library(dplyr)
library(parallel)
library(matsbyname)
library(magrittr)
library(testthat)


###########################################################
context("Environment")
###########################################################

test_that("getting and setting mc.cores works as expected", {
  # Out of the box, mc.cores should be 1.
  expect_equal(get_mc.cores(), 1)
  # Try setting to 0
  expect_error(set_mc.cores(0), "mc.cores must be >= 1.")
  # Try setting negative
  expect_error(set_mc.cores(-10), "mc.cores must be >= 1.")
  # Try setting the value to something else.
  set_mc.cores(4)
  expect_equal(get_mc.cores(), 4)
  # Set back to default value.
  set_mc.cores(1)
  expect_equal(get_mc.cores(), 1)
})


