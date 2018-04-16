

library(testthat)


###########################################################
context("Creating a mat_byname")
###########################################################

test_that("mat_byname works as expected", {
  expect_error(mat_byname(NULL), "'data' must be of a vector type, was 'NULL'")
  expect_true(is.na(mat_byname(NA)))
  expect_equal(class(mat_byname(matrix(1:2))), c("matrix", "mat_byname"))
  mbn <- mat_byname(c("a", "b"), nrow = 2, ncol = 1)
  expect_equal(mbn[1,1], "a")
  expect_equal(mbn[2,1], "b")
})


###########################################################
context("Adding mat_bynames")
###########################################################

# test_that("adding two mat_bynames works as expected", {
#   m1 <- matrix()
# })
  

