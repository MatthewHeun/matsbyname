

library(testthat)


###########################################################
context("Creating a mat_byname")
###########################################################

test_that("mat_byname works as expected", {
  expect_error(mat_byname(NULL), "'data' must be of a vector type, was 'NULL'")
  expect_true(is.na(mat_byname(NA)))
  expect_equal(class(mat_byname(matrix(1:2))), c("matrix", "mat_byname"))
  expect_error(mat_byname(list("a", "b")), "A matrix is required in mat_byname.")
})


###########################################################
context("Adding mat_bynames")
###########################################################

# test_that("adding two mat_bynames works as expected", {
#   m1 <- matrix()
# })
  

