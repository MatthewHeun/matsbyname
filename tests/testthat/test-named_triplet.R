test_that("to_triplet() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3"), 
                          indices = as.integer(c(4, 3, 100)))
  # Try with a single data frame
  indices <- dplyr::bind_rows(r_indices, c_indices)
  expected <- tibble::tribble(~i, ~j, ~x, 
                              9, 3, 1, 
                              7, 3, 2, 
                              5, 3, 3, 
                              9, 4, 4, 
                              7, 4, 5, 
                              5, 4, 6) |> 
    setrowtype("rows") |> setcoltype("cols")
  # This should error, because we need a list of 2 or more
  expect_error(to_triplet(m, indices), regexp = "index_map must be a list and not a data frame")
  
  # Try with 2 unnamed data frames
  indices2 <- list(r_indices, c_indices)
  expect_equal(to_triplet(m, indices2), expected)
  
  # Try with the index data frames in the wrong order.
  indices3 <- list(c_indices, r_indices)
  expect_error(to_triplet(m, indices3))
  
  # Try with named data frames (and add a third to test the code)
  unused_rindices <- data.frame(rnames = c("r3u", "r1u", "r2u", "r0u"),
                                rindices = as.integer(c(5, 9, 7, 100))) 
  unused_cindices <- data.frame(cnames = c("c2u", "c1u", "c3u"), 
                                cindices = as.integer(c(4, 3, 100)))
  indices4 <- list(unusedr = unused_rindices, 
                   cols = c_indices, 
                   rows = r_indices, 
                   unusedc = unused_cindices)
  expect_equal(to_triplet(m, indices4), expected)
})


test_that("to_triplet() fails when index_map contains repeated information", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0", "r1"),
                          indices = as.integer(c(5, 9, 7, 100, 101))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3", "c2"), 
                          indices = as.integer(c(4, 3, 100, 101)))
  expect_error(to_triplet(m, list(r_indices, c_indices)), 
               regexp = "All indices and names must be unique in to_triplet")
})


test_that("to_triplet() fails when the correct named index_map is not supplied", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0", "r1"),
                          indices = as.integer(c(5, 9, 7, 100, 101))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3", "c2"), 
                          indices = as.integer(c(4, 3, 100, 101)))
  unused_rindices <- data.frame(rnames = c("r3u", "r1u", "r2u", "r0u"),
                                rindices = as.integer(c(5, 9, 7, 100))) 
  unused_cindices <- data.frame(cnames = c("c2u", "c1u", "c3u"), 
                                cindices = as.integer(c(4, 3, 100)))
  indices_missing_rows <- list(unusedr = unused_rindices, 
                               cols = c_indices, 
                               # Wrong name. It should be called "rows".
                               bogus = r_indices, 
                               unusedc = unused_cindices)
  expect_error(to_triplet(m, indices_missing_rows), regexp = "Suitable index map for row type 'rows' not found")
  indices_missing_cols <- list(unusedr = unused_rindices, 
                               # Wrong name. It should be called "cols".
                               bogus = c_indices, 
                               rows = r_indices, 
                               unusedc = unused_cindices)
  expect_error(to_triplet(m, indices_missing_cols), regexp = "Suitable index map for column type 'cols' not found")
})


test_that("to_triplet() works with lists", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  m_list <- list(m, m, m)
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3"), 
                          indices = as.integer(c(4, 3, 100)))
  expected <- tibble::tribble(~i, ~j, ~x, 
                              9, 3, 1, 
                              7, 3, 2, 
                              5, 3, 3, 
                              9, 4, 4, 
                              7, 4, 5, 
                              5, 4, 6) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  
  # Try with 2 unnamed data frames
  indices <- list(r_indices, c_indices)
  expect_equal(to_triplet(m_list, indices), 
               list(expected, expected, expected))
})


test_that("to_named() works as expected", {
  triplet <- tibble::tribble(~i, ~j, ~x, 
                             9, 3, 1, 
                             7, 3, 2, 
                             5, 3, 3, 
                             9, 4, 4, 
                             7, 4, 5, 
                             5, 4, 6) |> 
    setrowtype("rows") |> setcoltype("cols")

  r_indices <- data.frame(names = paste0("r", 1:101),
                          indices = 1:101)
  c_indices <- data.frame(names = paste0("c", 1:101),
                          indices = 1:101)
  indices <- list(r_indices, c_indices)
  expected <- matrix(c(1, 2,
                       3, 4,
                       5, 6),
                     nrow = 3, ncol = 2,
                     dimnames = list(c("r9", "r7", "r5"),
                                     c("c3", "c4"))) |>
    sort_rows_cols() |> 
    setrowtype("rows") |>
    setcoltype("cols")

  expect_equal(to_named(triplet, indices), expected)
  # Try with a Matrix object returned
  expect_true(matsbyname:::equal_matrix_or_Matrix(to_named(triplet, indices, matrix_class = "Matrix"),
                                                  expected))
  
  # Try with a list
  expect_equal(to_named(list(triplet, triplet), indices), list(expected, expected))
})


test_that("to_named() fails when only one index_map is supplied", {
  triplet <- tibble::tribble(~i, ~j, ~x, 
                             9, 3, 1, 
                             7, 3, 2, 
                             5, 3, 3, 
                             9, 4, 4, 
                             7, 4, 5, 
                             5, 4, 6) |> 
    setrowtype("rows") |> setcoltype("cols")
  
  r_indices <- data.frame(names = paste0("r", 1:101),
                          indices = 1:101)
  indices <- list(r_indices)
  expect_error(to_named(triplet, indices), 
               regexp = "Incorrectly formatted index_map in matsbyname::get_row_col_index_maps")
})
  
