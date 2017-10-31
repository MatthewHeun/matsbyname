# Contains tests for the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(parallel)
library(byname)
library(testthat)

context("Sums")

# Define some matrices with commodity and industry names and types
# These matrices will be used in the tests below.
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, commoditynames)) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
Z <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")

UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
  setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
UminusZ <- matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
  setrowtype(rowtype(U)) %>% setcoltype(coltype(U))

Uplus100 <- U + 100

test_that("sums of constants", {
  # Simple sum of constants
  expect_equal(sum_byname(2, 3), 5)
  
  # If summed against NULL, return the item.
  expect_equal(sum_byname(NULL, 1), 1)
  expect_equal(sum_byname(2, NULL), 2)
  expect_equal(sum_byname(list(NULL, 1), list(1, 1)), list(1, 2))
  # If summed against NA, return NA
  expect_equal(sum_byname(2, NA), NA_integer_)
})

test_that("sums of matrices", {
  # If only one argument, return it.
  expect_equal(sum_byname(U), U)
  
  # This is a non-sensical test.  Row and column names are not respected. 
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U + Y, 
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  # Now, row and column names are respected.
  expect_equal(sum_byname(U, Y), UplusY)
  
  expect_equal(sum_byname(U, 100), U + 100)
  expect_equal(sum_byname(200, Y), 200 + Y %>% sort_rows_cols() %>% 
                                               setrowtype(rowtype(Y)) %>% 
                                               setcoltype(coltype(Y)))
  
  # This is a non-sensical test.  Row and column names are not respected.
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U + V,
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  
  # We should not be able to add U and V, because their row and column types differ.
  # Would like to test for entire error message which is 
  # "rowtype(a) == rowtype(b) is not TRUE"
  # However, it seems that the testthat package has trouble dealing with "(" in error messages.
  # So, we'll just test for the first word.
  expect_error(sum_byname(U, V), "rowtype")
})

test_that("sums of matrices in lists and data frames", {
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  
  # sum_byname should also work with lists.
  expect_equal(sum_byname(list(U,U), list(Y, Y)), list(UplusY, UplusY))
  expect_equal(sum_byname(list(U,U), list(100,100)), list(Uplus100, Uplus100))
  expect_equal(sum_byname(list(U,U), as.list(rep_len(100, 2))), list(Uplus100, Uplus100))
  
  # sum_byname also should work with data frames, as they are lists.
  expect_equal(sum_byname(DF$U, DF$Y), list(UplusY, UplusY))
  expect_equal(DF %>% mutate(sums = sum_byname(U, Y)), DF %>% mutate(sums = list(UplusY, UplusY)))
})

context("Differences")

test_that("differences of constants", {
  # Simple difference of constants
  expect_equal(difference_byname(100, 50), 50)
  
  # If differenced against NULL, return the item.
  expect_equal(difference_byname(NULL, 1), -1)
  expect_equal(difference_byname(2, NULL), 2)
  expect_equal(difference_byname(list(NULL, 1), list(1, 1)), list(-1, 0))
  # If summed against NA, return NA
  expect_equal(difference_byname(2, NA), NA_integer_)
})

test_that("differences of matrices", {
  # If only one argument, return it.
  expect_equal(difference_byname(U), U)
  
  # This is a non-sensical test.  Row and column names are not respected. 
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U - Z, matrix(c(-3, -1, 1, 3), nrow = 2, dimnames = dimnames(U)) %>% 
                      setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
               
  # Row and column names respected! Should be all zeroes.
  expect_equal(difference_byname(U, Z), matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
                                        setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  expect_equal(difference_byname(100, U), matrix(c(99, 98, 97, 96), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  # difference_byname should sort the rows and column names.
  # So we expect the dimnames of the difference to be the same as the dimnames of U (which has sorted dimnames).
  expect_equal(difference_byname(10, Z), matrix(c(9, 8, 7, 6), ncol = 2, dimnames = dimnames(U)) %>% 
                                        setrowtype(rowtype(Z)) %>% setcoltype(coltype(Z)))
  # When subtrahend is missing, return minuend (in this case, Z).
  expect_equal(difference_byname(Z), Z)
  # When minuend is missing, return - subtrahend (in this case, -Z)
  expect_equal(difference_byname(subtrahend = Z), elementproduct_byname(-1, Z))
})
  
test_that("differences of matrices in lists and data frames", {
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Z = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z

  expect_equal(difference_byname(list(100, 100), list(50, 50)), list(50, 50))
  expect_equal(difference_byname(list(U, U), list(Z, Z)), list(UminusZ, UminusZ))
  expect_equal(difference_byname(DF$U, DF$Z), list(UminusZ, UminusZ))
  expect_equal(difference_byname(DF %>% mutate(diffs = difference_byname(U, Z))), DF %>% mutate(diffs = list(UminusZ)))
  
})