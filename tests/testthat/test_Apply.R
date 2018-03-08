# Contains tests for apply_byname functions in the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(Hmisc)
library(dplyr)
library(parallel)
library(matsbyname)
library(magrittr)
library(testthat)
# library(matsindf)
library(tidyr)


###########################################################
# context("Unary apply")
###########################################################

test_that("unaryapply_byname works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(unaryapply_byname(FUN = `-`, a = U, rowcoltypes = "all"), difference_byname(0, U))
})


###########################################################
# context("Binary apply")
###########################################################

test_that("binaryapply_byname works as expected", {
  expect_equal(binaryapply_byname(FUN = sum, a = list(1, 2, 3), b = list(4,5,6)), 
               list(5, 7, 9))
})


###########################################################
# context("Cumulative apply")
###########################################################

test_that("cumapply_byname works as expected", {
  expect_equal(cumapply_byname(FUN = `sum`, m = list(1, 2, 3)), list(1, 3, 6))
  expect_equal(cumapply_byname(FUN = `prod`, m = list(1, 2, 3)), list(1, 2, 6))
})
