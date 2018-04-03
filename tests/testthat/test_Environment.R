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
  # If, for some reason, mc.cores is set to something other than 1, 
  # this test will fail.  
  # If that failure occurs, the problem is likely in 
  # the following section of code in Environment.R:

  # # Use this line for local testing only. 
  # # NEVER submit to CRAN with this line uncommented.
  # # In fact, one test will fail if this line is uncommented.
  # set_mc_cores(detectCores(logical = FALSE))
  expect_equal(get_mc_cores(), 1L)
  
  # Try setting to 0
  expect_error(set_mc_cores(0L), "mc.cores must be >= 1L")
  
  # Try setting negative
  expect_error(set_mc_cores(-10L), "mc.cores must be >= 1L")
  
  # Try setting the value to something else.
  set_mc_cores(4L)
  expect_equal(get_mc_cores(), 4L)
  
  # Set back to default value.
  set_mc_cores(1L)
  expect_equal(get_mc_cores(), 1)
})


