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


test_that("reallocate_byname() works with row reallocation", {
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


test_that("reallocate_byname() works with column reallocation", {
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


test_that("reallocate_byname() works when allocating multiple rows", {
  a <- matrix(c(1, 2, 
                3, 4, 
                5, 6, 
                7, 8), 
              nrow = 4, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3", "r4"), 
                              c("c1", "c2")))
  expected <- matrix(c(1 + 1/4*12, 2 + 2/6*14, 
                       3 + 3/4*12, 4 + 4/6*14), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), c("c1", "c2")))
  
  res <- matsbyname::reallocate_byname(a, c("r3", "r4"), margin = 1)
  expect_equal(res, expected)
})


test_that("reallocate_byname() works when allocating multiple columns", {
  a <- matrix(c(1, 2, 3, 5, 
                5, 6, 7, 9), 
              nrow = 2, ncol = 4, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3", "c4")))
  expected <- matrix(c(1 + 1/6*5, 5 + 5/6*5, 
                       5 + 5/14*13, 9 + 9/14*13), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), c("c1", "c4")))
  
  res <- matsbyname::reallocate_byname(a, c("c2", "c3"), margin = 2)
  expect_equal(res, expected)
})


test_that("reallocate_byname() works as expected with a 0 column, a degenerate case", {
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
  
  # Same result as res3, but no warning.
  res4 <- matsbyname::reallocate_byname(a2, "r3", margin = 1, .zero_behaviour = "zeroes") |> 
    expect_equal(expected3)
  
  # Allocate equally when only zeroes are present.
  expected5 <- matrix(c(2, 3,
                        4, 3), 
                      nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c1", "c2")))
  res5 <- matsbyname::reallocate_byname(a2, "r3", margin = 1, .zero_behaviour = "allocate equally") |> 
    expect_equal(expected5)
})


test_that("reallocate_byname() works in a data frame and with Matrix objects", {
  a <- Matrix(c(1, 2, 
                5, 6, 
                10, 11), byrow = TRUE, nrow = 3, ncol = 2,
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expectedUSA <- Matrix(c(1 + 1/11*5, 2 + 2/13*6, 
                          10 + 10/11*5, 11 + 11/13*6), byrow = TRUE, nrow = 2, ncol = 2, 
                        dimnames = list(c("r1", "r3"), c("c1", "c2")))
  expectedGHA <- Matrix(c(6 + 6/21*10, 7 + 7/23*11,
                          15 + 15/21*10, 16 + 16/23*11), byrow = TRUE, nrow = 2, ncol = 2, 
                        dimnames = list(c("r1", "r3"), c("c1", "c2")))
  df <- tibble::tribble(~Country, ~a_mat, 
                        "USA", a, 
                        "GHA", a + 5)
  res <- df |> 
    dplyr::mutate(
      a_reallocated = matsbyname::reallocate_byname(a_mat, "r2", margin = 1)
    )
})


# Test when row or column is missing
# Test with row col name pieces
# Test with different row and col notation
# Test in a data frame

