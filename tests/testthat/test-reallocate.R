test_that("reallocate_byname() errors as expected", {
  expect_error(
    matsbyname::reallocate_byname(a = 42, margin = c(1, 2, 3)), 
    "margin must have length 1 or 2 in matsbyname::reallocate_byname\\(\\)"
  )
  expect_error(
    matsbyname::reallocate_byname(a = 42, margin = 4), 
    "margin must be 1, 2, or c\\(1, 2\\) in matsbyname::reallocate_byname\\(\\)"
  )
  expect_error(
    matsbyname::reallocate_byname(a = 42, margin = c(1, 1)), 
    "margin must contain unique integers in matsbyname::reallocate_byname\\(\\)"
  )
})


test_that("reallocate_byname() works with row redistribution", {
  a <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2")))
  
  res <- matsbyname::reallocate_byname(a, "r3", margin = 1)
  
  expected <- matrix(c(1 + 1/4*5, 2 + 2/6*6, 
                       3 + 3/4*5, 4 + 4/6*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
  
  # Redistribute 2 rows into 1, essentially providing a summation
  res2 <- matsbyname::reallocate_byname(a, c("r1", "r3"), margin = 1)
  
  expected2 <- matrix(c(9, 12), 
                      nrow = 1, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r2"), 
                                      c("c1", "c2")))
  expect_equal(res2, expected2)
  
  # Test with a list
  res3 <- matsbyname::reallocate_byname(list(a, a), "r3", margin = 1)
  expected3 <- list(expected, expected)
  expect_equal(res3, expected3)
})


test_that("reallocate_byname() works with column redistribution", {
  a <- matrix(c(1, 2, 3,
                4, 5, 6), 
              nrow = 2, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3")))
  
  res <- matsbyname::reallocate_byname(a, "c3", margin = 2)
  
  expected <- matrix(c(1 + 1/3*3, 2 + 2/3*3, 
                       4 + 4/9*6, 5 + 5/9*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
  
  # Redistribute 2 columns into 1, essentially providing a summation
  res2 <- matsbyname::reallocate_byname(a, c("c1", "c2"), margin = 2)
  
  expected2 <- matrix(c(6, 
                        15), 
                      nrow = 2, ncol = 1, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c3")))
  expect_equal(res2, expected2)
  
  # Test with a list
  res3 <- matsbyname::reallocate_byname(list(a, a, a), "c3", margin = 2)
  expected3 <- list(expected, expected, expected)
  expect_equal(res3, expected3)
})


test_that("reallocate_byname() works with a 0 column", {
  # Try reallocating a 0 column.
  a <- matrix(c(1, 2, 3, 0,
                4, 5, 6, 0), 
              nrow = 2, ncol = 4, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3", "c4")))
  
  res <- matsbyname::reallocate_byname(a, "c4", margin = 2)
  expect_equal(res, matsbyname::select_cols_byname(a, remove_pattern = "c4", fixed = TRUE))
  
  # Try redistributing a non-zero value
  # when all remaining values in a column are zero
  a2 <- matrix(c(1, 0,
                 2, 0, 
                 3, 6), 
               nrow = 3, ncol = 2, byrow = TRUE, 
               dimnames = list(c("r1", "r2", "r3"), 
                               c("c1", "c2")))
  res2 <- matsbyname::reallocate_byname(a2, "r3", margin = 1) |> 
    expect_error("The following cannot be reallocated due to all zero values in the receiving rows: c2")

  expected3 <- matrix(c(2, 0,
                        4, 0), 
                      nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c1", "c2")))
  res3 <- matsbyname::reallocate_byname(a2, "r3", margin = 1, .zero_behaviour = "warning") |> 
    expect_equal(expected3) |> 
    expect_warning("The following cannot be reallocated due to all zero values in the receiving rows: c2")
  

})


# Test when row or column is missing
# Test with row col name pieces
# Test with different row and col notation

