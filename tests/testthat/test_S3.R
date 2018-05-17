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
  expect_true(is.mat_byname(mbn))
  expect_false(is.mat_byname(matrix(1:2)))
  expect_true(is.mat_byname(as.mat_byname(matrix(1:2))))
})


###########################################################
context("Adding mat_bynames")
###########################################################

test_that("adding two mat_bynames works as expected", {
  m1 <- mat_byname(c(1:4), 
                  nrow = 2, ncol = 2, byrow = TRUE, 
                  dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                  rowtype = "row", coltype = "col")
  m2 <- mat_byname(c(11:14), 
                   nrow = 2, ncol = 2, byrow = TRUE, 
                   dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                   rowtype = "row", coltype = "col")
})
  

