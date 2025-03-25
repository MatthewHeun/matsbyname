test_that("reallocate_byname() errors with invalid margin argument", {
  a <- matrix(1)
  expect_error(
    reallocate_byname(a, margin = 3), 
    "margin must be 1 or 2 in matsbyname::reallocate_byname\\(\\)"
  )
  expect_error(
    reallocate_byname(a, margin = c(1, 2)), 
    "margin must have length 1 in matsbyname::reallocate_byname\\(\\)"
  )
})


test_that("reallocate_byname() works with row reallocation", {
  a <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2")))
  
  res <- reallocate_byname(a, rownames = "r3", margin = 1)
  
  expected <- matrix(c(1 + 1/4*5, 2 + 2/6*6, 
                       3 + 3/4*5, 4 + 4/6*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
  
  # Redistribute 2 rows into 1, essentially providing a summation
  res2 <- reallocate_byname(a, rownames = c("r1", "r3"), margin = 1)
  
  expected2 <- matrix(c(9, 12), 
                      nrow = 1, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r2"), 
                                      c("c1", "c2")))
  expect_equal(res2, expected2)
  
  # Test with a list
  res3 <- reallocate_byname(list(a, a), rownames = "r3", margin = 1)
  expected3 <- list(expected, expected)
  expect_equal(res3, expected3)
})


test_that("reallocate_byname() works with column reallocation", {
  a <- matrix(c(1, 2, 3,
                4, 5, 6), 
              nrow = 2, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3")))
  
  res <- reallocate_byname(a, colnames = "c3", margin = 2)
  
  expected <- matrix(c(1 + 1/3*3, 2 + 2/3*3, 
                       4 + 4/9*6, 5 + 5/9*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
  
  # Redistribute 2 columns into 1, essentially providing a summation
  res2 <- reallocate_byname(a, colnames = c("c1", "c2"), margin = 2)
  
  expected2 <- matrix(c(6, 
                        15), 
                      nrow = 2, ncol = 1, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c3")))
  expect_equal(res2, expected2)
  
  # Test with a list
  res3 <- reallocate_byname(list(a, a, a), colnames = "c3", margin = 2)
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
  
  res <- reallocate_byname(a, rownames = c("r3", "r4"), margin = 1)
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
  
  res <- reallocate_byname(a, colnames = c("c2", "c3"), margin = 2)
  expect_equal(res, expected)
})


test_that("reallocate_byname() works as expected with a 0 column, a degenerate case", {
  # Try reallocating a 0 column.
  a <- matrix(c(1, 2, 3, 0,
                4, 5, 6, 0), 
              nrow = 2, ncol = 4, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3", "c4")))
  
  res <- reallocate_byname(a, colnames = "c4", margin = 2)
  expect_equal(res, select_cols_byname(a, remove_pattern = "c4", fixed = TRUE))
  
  # Try redistributing a non-zero value
  # when all remaining values in a column are zero
  a2 <- matrix(c(1, 0,
                 2, 0, 
                 3, 6), 
               nrow = 3, ncol = 2, byrow = TRUE, 
               dimnames = list(c("r1", "r2", "r3"), 
                               c("c1", "c2")))
  res2 <- reallocate_byname(a2, rownames = "r3", margin = 1) |> 
    expect_error("r3 cannot be reallocated due to all zero values remaining on the other margin: c2")

  expected3 <- matrix(c(2, 0,
                        4, 0), 
                      nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c1", "c2")))
  res3 <- reallocate_byname(a2, rownames = "r3", margin = 1, .zero_behaviour = "warning") |> 
    expect_equal(expected3) |> 
    expect_warning("r3 cannot be reallocated due to all zero values remaining on the other margin: c2")
  
  # Same result as res3, but no warning.
  res4 <- reallocate_byname(a2, rownames = "r3", margin = 1, .zero_behaviour = "zeroes") |> 
    expect_equal(expected3)
  
  # Allocate equally when only zeroes are present.
  expected5 <- matrix(c(2, 3,
                        4, 3), 
                      nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), 
                                      c("c1", "c2")))
  res5 <- reallocate_byname(a2, rownames = "r3", margin = 1, .zero_behaviour = "allocate equally") |> 
    expect_equal(expected5)
})


test_that("reallocate_byname() works in a data frame and with Matrix objects", {
  a <- Matrix(c(1, 2, 
                5, 6, 
                10, 11), byrow = TRUE, nrow = 3, ncol = 2,
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  df <- tibble::tribble(~Country, ~a_mat, 
                        "USA", a, 
                        "GHA", a + 5)
  res <- df |> 
    dplyr::mutate(
      a_reallocated = reallocate_byname(a_mat, rownames = "r2", margin = 1)
    )

  expectedUSA <- Matrix(c(1 + 1/11*5, 2 + 2/13*6, 
                          10 + 10/11*5, 11 + 11/13*6), byrow = TRUE, nrow = 2, ncol = 2, 
                        dimnames = list(c("r1", "r3"), c("c1", "c2")))
  expectedGHA <- Matrix(c(6 + 6/21*10, 7 + 7/23*11,
                          15 + 15/21*10, 16 + 16/23*11), byrow = TRUE, nrow = 2, ncol = 2, 
                        dimnames = list(c("r1", "r3"), c("c1", "c2")))

  expected <- df |> 
    dplyr::mutate(
      a_reallocated = list(expectedUSA, expectedGHA)
    )
  expect_equal(res, expected)
})


test_that("reallocate_byname() works when choosing by pieces of column names", {
  a <- matrix(c(1, 2, 3, 4,
                5, 6, 7, 8), byrow = TRUE, nrow = 2, ncol = 4, 
              dimnames = list(c("r1", "r2"), c("a [from b]", "a [from c]", "d [from b]", "e")))
  # Infers notation
  a_reallocated <- reallocate_byname(a, margin = 2, colnames = "a", piece_colnames = "noun")
  expected <- matrix(c(3 + 3/7*3, 4 + 4/7*3, 
                       7 + 7/15*11, 8 + 8/15*11), byrow = TRUE, nrow = 2, ncol = 2, 
                     dimnames = list(c("r1", "r2"), c("d [from b]", "e")))
  expect_equal(a_reallocated, expected)

  # Specify a piece
  a_reallocated2 <- reallocate_byname(a, colnames = "b", margin = 2, piece_colnames = "from")
  expected2 <- matrix(c(2 + 2/6*4, 4 + 4/6*4,
                        6 + 6/14*12, 8 + 8/14*12), byrow = TRUE, nrow = 2, ncol = 2,
                      dimnames = list(c("r1", "r2"), c("a [from c]", "e")))
  expect_equal(a_reallocated2, expected2)
  
  # Specify notation as bracket_notation.
  # Specifying from_notation will not work.
  a_reallocated3 <- reallocate_byname(a, 
                                      colnames = "b",
                                      margin = 2,, 
                                      piece_colnames = "from", 
                                      notation_colnames = RCLabels::bracket_notation)
  expected3 <- matrix(c(2 + 2/6*4, 4 + 4/6*4,
                        6 + 6/14*12, 8 + 8/14*12), byrow = TRUE, nrow = 2, ncol = 2,
                      dimnames = list(c("r1", "r2"), c("a [from c]", "e")))
  expect_equal(a_reallocated3, expected3)
})


test_that("reallocate_byname() works when the row or column to be reallocated is missing", {
  a <- Matrix(c(1, 2, 
                5, 6, 
                10, 11), byrow = TRUE, nrow = 3, ncol = 2,
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  res <- reallocate_byname(a, rownames = "r4", margin = 2)
  expect_equal(res, a)
})


test_that("reallocate_byname() works with different row and column name notations", {
  a <- matrix(c(1, 2, 3, 4,
                5, 6, 7, 8), byrow = TRUE, nrow = 2, ncol = 4, 
              dimnames = list(c("r1", "r2"), c("a [to b]", "a [to c]", "d [to b]", "e")))
  # Infers notation
  a_reallocated <- reallocate_byname(a, colnames = "b", margin = 2, piece_colnames = "to")
  expected <- matrix(c(2 + 2/6*4, 4 + 4/6*4,
                       6 + 6/14*12, 8 + 8/14*12), byrow = TRUE, nrow = 2, ncol = 2,
                     dimnames = list(c("r1", "r2"), c("a [to c]", "e")))
  expect_equal(a_reallocated, expected)

  a_arrow <- matrix(c(1, 2, 3, 4,
                      5, 6, 7, 8), byrow = TRUE, nrow = 2, ncol = 4, 
                    dimnames = list(c("a -> b", "c -> d"), c("a -> b", "a -> c", "d -> b", "e")))
  a_arrow_reallocated <- reallocate_byname(a_arrow, rownames = "b", margin = 1, piece_rownames = "suff")
  a_arrow_expected <- matrix(c(6, 8, 10, 12), byrow = TRUE, nrow = 1, ncol = 4, 
                             dimnames = list("c -> d", c("a -> b", "a -> c", "d -> b", "e")))
  expect_equal(a_arrow_reallocated, a_arrow_expected)
})


test_that("reallocate_byname() works with row reallocation and column filtering", {
  a <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2")))
  # Reallocate r3 but only c2
  res_a <- reallocate_byname(a, rownames = "r3", colnames = "c2", margin = 1)
  
  expected_a <- matrix(c(1, 2 + 2/6*6, 
                         3, 4 + 4/6*6, 
                         5, 0), 
                       nrow = 3, ncol = 2, byrow = TRUE, 
                       dimnames = list(c("r1", "r2", "r3"), 
                                       c("c1", "c2")))
  expect_equal(res_a, expected_a)
  
  
  b <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), 
              nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2", "c3")))
  res_b <- reallocate_byname(b, rownames = "r1", colnames = c("c1", "c3"), margin = 1)
  
  expected_b <- matrix(c(0, 2, 0, 
                         4 + 4/11*1, 5, 6 + 6/15*3, 
                         7 + 7/11*1, 8, 9 + 9/15*3), 
                       nrow = 3, ncol = 3, byrow = TRUE, 
                       dimnames = list(c("r1", "r2", "r3"), 
                                       c("c1", "c2", "c3")))
  expect_equal(res_b, expected_b)
})


test_that("reallocate_byname() works with column reallocation and row filtering", {
  a <- matrix(c(1, 2, 3,
                4, 5, 6,
                7, 8, 9), 
              nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2", "c3")))
  res_a <- reallocate_byname(a, rownames = c("r2", "r3"), colnames = c("c1", "c2"), margin = 2)
  
  expected_a <- matrix(c(1, 2, 3, 
                         0, 0, 15, 
                         0, 0, 24), 
                       nrow = 3, ncol = 3, byrow = TRUE, 
                       dimnames = list(c("r1", "r2", "r3"), 
                                       c("c1", "c2", "c3")))
  expect_equal(res_a, expected_a)

  
  res_b <- reallocate_byname(a, rownames = "r1", colnames = "c3", margin = 2)
  expected_b <- matrix(c(2, 4, 0, 
                         4, 5, 6, 
                         7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
                       dimnames = list(c("r1", "r2", "r3"), 
                                       c("c1", "c2", "c3")))
  expect_equal(res_b, expected_b)
    
})




