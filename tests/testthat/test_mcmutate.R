# Contains tests for mcmutate function in the byname package.

library(parallel)
library(testthat)


###########################################################
context("Multi-core mutate")
###########################################################

test_that("mcmutate works as expected", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  res <- df %>% 
    mcmutate(
      c = a + b
    )
  expect_equal(res$c, list(4, 6))
})


