# This file contains tests for functions in Utilities.R.

library(magrittr)
library(parallel)
library(testthat)
library(rlang)
library(dplyr)
library(Matrix)


###########################################################
context("Multi-core mutate")
###########################################################

test_that("mcmutate works as expected", {
  df1 <- data.frame(a = c(1,2), b = c(3,4))
  res1 <- df1 %>% 
    mcmutate(mc.cores = 2L,
      c = a + b
    )
  expect_equal(res1$c, c(4, 6))
  
  df2 <- data.frame(a = c(1,2,3), b = c(4,5,6))
  res2 <- df2 %>% 
    mcmutate(mc.cores = 2L,
      c = a + b
    )
  expect_equal(res2$c, c(5, 7, 9))
  
  # Make a large data frame, fill it, and test with 1-2 cores.
  size <- 100
  df3 <- matrix(runif(2*size), nrow = size, ncol = 2, dimnames = list(NULL, c("a", "b"))) %>% 
    as.data.frame()
  expected3 <- df3 %>% 
    mutate(
      c = a + b
    )
  ncores <- 1:2
  res3 <- lapply(ncores, FUN = function(cores){
    df3 %>% 
      mcmutate(mc.cores = cores,
        c = a + b
      )
  })
  for (i in ncores) {
    expect_equal(res3[[i]], expected3)
  }
})

test_that("mcmutate works with programming", {
  my_mcmutate <- function(df, newvarname = "c", operand1 = "a", operand2 = "b", mc.cores = 2L){
    df %>% 
      mcmutate(
        !!as.name(newvarname) := !!as.name(operand1) + !!as.name(operand2), 
        mc.cores = mc.cores
      )
  }
  df <- data.frame(a = c(1,2), b = c(3,4))
  res <- df %>% 
    my_mcmutate()
  expect_equal(res$c, c(4,6))
})

###########################################################
context("Selecting rows and columns")
###########################################################

test_that("an error is generated when no retain or remove patterns are default", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_error(m %>% select_rows_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
  expect_error(m %>% select_cols_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
})

test_that("selecting rows and columns works even when there is a NULL situation", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% select_rows_byname(retain_pattern = "r1"), 
               matrix(c(1, 3), ncol = 2, dimnames = list("r1", c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_rows_byname(retain_pattern = "r3"))
  # Try with columns
  expect_equal(m %>% select_cols_byname(retain_pattern = "c1"), 
               matrix(1:2, nrow = 2, dimnames = list(c("r1", "r2"), "c1")) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_cols_byname(retain_pattern = "c3"))
})

test_that("setting row and column names works even when there is a NULL situation", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% setrownames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setrownames_byname(NULL, c("a", "b")))
  # Try with columns
  expect_equal(m %>% setcolnames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("a", "b"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setcolnames_byname(NULL, c("a", "b")))
})
  
