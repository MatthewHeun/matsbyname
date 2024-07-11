test_that("to_triplet() works as expected", {
  # Create a sparse matrix, with only non-zero rows and cols
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r5", "r9", "r7"), 
                              c("c4", "c3"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = c("r5", "r9", "r7", "r100"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c4", "c3", "c100"), 
                          indices = as.integer(c(4, 3, 100)))
  # Try with a single data frame
  indices <- dplyr::bind_rows(r_indices, c_indices)
  expected <- tibble::tribble(~i, ~j, ~value, 
                              9, 3, 5, 
                              7, 3, 6, 
                              5, 3, 4, 
                              9, 4, 2, 
                              7, 4, 3, 
                              5, 4, 1) |> 
    setrowtype("rows") |> setcoltype("cols")
  # This should error, because we need a list of 2 or more
  expect_error(to_triplet(m, indices), regexp = "All indices and names must be unique in to_triplet")
  
  # Try with 2 unnamed data frames
  indices2 <- list(r_indices, c_indices)
  expect_equal(to_triplet(m, indices2) |> 
                 dplyr::arrange(i, j),
               expected |> 
                 dplyr::arrange(i, j))
  
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
  expect_equal(to_triplet(m, indices4) |> 
                 dplyr::arrange(i, j),
               expected |> 
                 dplyr::arrange(i, j))
  # Try again, with a non-sparse matrix
  m5 <- matrix(c(0, 0, 0, 0, 
                 0, 0, 0, 0, 
                 0, 0, 0, 0, 
                 0, 0, 0, 0, 
                 0, 0, 4, 1, 
                 0, 0, 0, 0, 
                 0, 0, 6, 3, 
                 0, 0, 0, 0, 
                 0, 0, 5, 2), 
               nrow = 9, ncol = 4, byrow = TRUE,
               dimnames = list(c("r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9"), 
                               c("c1", "c2", "c3", "c4"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  expect_equal(to_triplet(m5, indices4) |> 
                 dplyr::arrange(i, j),
               expected |> 
                 dplyr::arrange(i, j))
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
  expected <- tibble::tribble(~i, ~j, ~value, 
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

test_that("to_triplet() works with NULL", {
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3"), 
                          indices = as.integer(c(4, 3, 100)))
  # Try with 2 unnamed data frames
  indices <- list(r_indices, c_indices)
  expect_null(to_triplet(NULL, indices))
  
  
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  m_list <- list(m, NULL, m)
  expected <- tibble::tribble(~i, ~j, ~value, 
                              9, 3, 1, 
                              7, 3, 2, 
                              5, 3, 3, 
                              9, 4, 4, 
                              7, 4, 5, 
                              5, 4, 6) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  expect_equal(to_triplet(m_list, indices), 
               list(expected, NULL, expected))
})


test_that("to_triplet() error messages are informative", {
  # Create a sparse matrix, with only non-zero rows and cols
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r5", "r9", "r7"), 
                              c("c4", "c3"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  # Create the row indices so that r5 and r9 are missing,
  # which will trigger NA in the triplet data frame
  r_indices <- data.frame(names = c("r7", "r100"),
                          indices = as.integer(c(7, 100))) 
  c_indices <- data.frame(names = c("c4", "c3", "c100"), 
                          indices = as.integer(c(4, 3, 100)))
  # Try with a single data frame
  indices <- list(r_indices, c_indices)
  expect_error(to_triplet(m, indices), regexp = "Unmatched row names in")

  # Create the row indices so that c3 and c4 are missing,
  # which will trigger NA in the triplet data frame
  r_indices2 <- data.frame(names = c("r5", "r9", "r7", "r100"),
                           indices = as.integer(c(5, 9, 7, 100))) 
  c_indices2 <- data.frame(names = c("c100"), 
                           indices = as.integer(c(100)))
  # Try with a single data frame
  indices2 <- list(r_indices2, c_indices2)
  expect_error(to_triplet(m, indices2), regexp = "Unmatched column names in")
})


test_that("to_triplet() retains zero matrix structure when asked", {
  m <- matrix(c(0, 0, 
                0, 0, 
                0, 0), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3"), 
                          indices = as.integer(c(4, 3, 100)))
  # Try without retaining zero matrix structure
  expect_equal(to_triplet(m, list(r_indices, c_indices)),
               # Expect a zero-row data frame with correct row and column types
               data.frame(i = as.integer(0), j = as.integer(0), value = 3.1415926) |> 
                 dplyr::filter(FALSE) |> 
                 tibble::as_tibble() |> 
                 matsbyname::setrowtype("rows") |> 
                 matsbyname::setcoltype("cols"))
  # Now retain zero matrix structure
  expect_equal(to_triplet(m, list(r_indices, c_indices), retain_zero_structure = TRUE),
               # Expect a zero-row data frame with correct row and column types
               data.frame(               # r1 r2 r3 r1 r2 r3
                                         # c1 c1 c1 c2 c2 c2
                          i = as.integer(c(9, 7, 5, 9, 7, 5)),
                          j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                          value = 0) |> 
                 tibble::as_tibble() |> 
                 matsbyname::setrowtype("rows") |> 
                 matsbyname::setcoltype("cols"))
})


test_that("to_named_matrix() works as expected", {
  triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
                        j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                        value = c(1, 2, 3, 4, 5, 6)) |> 
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

  expect_equal(to_named_matrix(triplet, indices), expected)
  # Try with a Matrix object returned
  expect_true(matsbyname:::equal_matrix_or_Matrix(to_named_matrix(triplet, indices, matrix_class = "Matrix"),
                                                  expected))
  
  # Try with a list
  expect_equal(to_named_matrix(list(triplet, triplet), indices), list(expected, expected))
})


test_that("to_named_matrix() fails when only one index_map is supplied in a list", {
  triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
                        j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                        value = c(1, 2, 3, 4, 5, 6)) |> 
    setrowtype("rows") |> setcoltype("cols")
  
  r_indices <- data.frame(names = paste0("r", 1:101),
                          indices = 1:101)
  indices <- list(r_indices)
  expect_error(to_named_matrix(triplet, indices), 
               regexp = "Incorrectly formatted index_map in matsbyname::get_row_col_index_maps")
})


test_that("to_named_matrx() works when a single data frame is supplied", {
  triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
                        j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                        value =        c(1, 2, 3, 4, 5, 6)) |> 
    setrowtype("rows") |> setcoltype("cols")
  
  indices <- data.frame(names = c(paste0("r", 1:101), paste0("c", 1:101)),
                          indices = 1:202)
  expect_equal(to_named_matrix(triplet, indices), 
               matrix(c(3, 6, 
                        2, 5, 
                        1, 4), byrow = TRUE, nrow = 3, 
                      dimnames = list(c("r5", "r7", "r9"), c("r3", "r4"))) |> 
                 matsbyname::setrowtype("rows") |> matsbyname::setcoltype("cols"))
})


test_that("to_named_matrix() is reversible", {
  # Start with a triplet.
  triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
                        j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                        value = c(1, 2, 3, 4, 5, 6)) |> 
    setrowtype("rows") |> setcoltype("cols")
  
  r_indices <- data.frame(names = paste0("r", 1:101),
                          indices = 1:101)
  c_indices <- data.frame(names = paste0("c", 1:101),
                          indices = 1:101)
  indices <- list(r_indices, c_indices)
  expected_named <- matrix(c(1, 2,
                             3, 4,
                             5, 6),
                           nrow = 3, ncol = 2,
                           dimnames = list(c("r9", "r7", "r5"),
                                           c("c3", "c4"))) |>
    sort_rows_cols() |> 
    setrowtype("rows") |>
    setcoltype("cols")
  # Convert to named
  named <- to_named_matrix(triplet, indices)
  expect_equal(named, expected_named)  
  
  # Now reverse the process
  triplet2 <- to_triplet(named, indices)
  expect_equal(triplet2 |> 
                 as.data.frame() |> 
                 dplyr::arrange(i, j), 
               triplet |> 
                 dplyr::arrange(i, j))
})  


test_that("to_triplet() is reversible", {
  m <- matrix(c(0, 0, 0, 0, 
                0, 0, 0, 0, 
                0, 0, 0, 0, 
                0, 0, 0, 0, 
                0, 0, 4, 1, 
                0, 0, 0, 0, 
                0, 0, 6, 3, 
                0, 0, 0, 0, 
                0, 0, 5, 2), 
              nrow = 9, ncol = 4, byrow = TRUE,
              dimnames = list(c("r1", "r2", "r3", "r4", "r5", "r6", "r7", "r8", "r9"), 
                              c("c1", "c2", "c3", "c4"))) |> 
    setrowtype("rows") |> 
    setcoltype("cols")
  r_indices <- data.frame(names = paste0("r", 1:101),
                          indices = 1:101)
  c_indices <- data.frame(names = paste0("c", 1:101),
                          indices = 1:101)
  # Try with a single data frame
  indices <- list(r_indices, c_indices)
  
  triplet <- to_triplet(m, indices)
  expected <- tibble::tribble(~i, ~j, ~value, 
                              9, 3, 5, 
                              7, 3, 6, 
                              5, 3, 4, 
                              9, 4, 2, 
                              7, 4, 3, 
                              5, 4, 1) |> 
    setrowtype("rows") |> setcoltype("cols")
  expect_equal(triplet |> 
                 dplyr::arrange(i, j), 
               expected |> 
                 dplyr::arrange(i, j))
  m2 <- to_named_matrix(triplet, indices)
  expect_equal(m2, matrix(c(4, 1, 
                            6, 3, 
                            5, 2), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("r5", "r7", "r9"), 
                                          c("c3", "c4"))) |> 
                 setrowtype("rows") |> setcoltype("cols"))
})


test_that("to_named_matrix() works with names already replacing integers", {
  triplet <- tibble::tribble(~i, ~j, ~value, 
                             "r9", "c3", 5, 
                             "r7", "c3", 6, 
                             "r5", "c3", 4, 
                             "r9", "c4", 2, 
                             "r7", "c4", 3, 
                             "r5", "c4", 1) |> 
    setrowtype("rows") |> setcoltype("cols")
  res <- triplet |> 
    to_named_matrix()
  
  expect_equal(res, matrix(c(4, 1, 
                             6, 3, 
                             5, 2), byrow = TRUE, 
                           nrow = 3, ncol = 2, 
                           dimnames = list(c("r5", "r7", "r9"), 
                                           c("c3", "c4"))) |> 
                 setrowtype("rows") |> setcoltype("cols"))
})


test_that("to_named_matrix() fails when neither integer triplet nor character triplet", {
  triplet <- tibble::tribble(~i, ~j, ~value, 
                             9, "c3", 5, 
                             7, "c3", 6, 
                             5, "c3", 4, 
                             9, "c4", 2, 
                             7, "c4", 3, 
                             5, "c4", 1) |> 
    setrowtype("rows") |> setcoltype("cols")
  res <- triplet |> 
    to_named_matrix() |> 
    expect_error("`row_index_colname` and `col_index_colname` must both be all integer or all character in to_named_matrix")
})


test_that("create_triplet() correctly retains zero structure", {
  m <- matrix(c(0, 0, 0, 
                0, 0, 0), nrow = 2, dimnames = list(c("r1", "r2"), 
                                                    c("c1", "c2", "c3")))
  expect_equal(matsbyname:::create_triplet(m),
               data.frame(i = as.integer(0), j = as.integer(0), value = 3.1415926) |> 
                 dplyr::filter(FALSE) |> 
                 tibble::as_tibble())
  expect_equal(matsbyname:::create_triplet(m, retain_zero_structure = TRUE),
               tibble::tribble(~i, ~j, ~value, 
                               1, 1, 0, 
                               2, 1, 0, 
                               1, 2, 0, 
                               2, 2, 0, 
                               1, 3, 0, 
                               2, 3, 0))
})


test_that("to_named_matrx() returns zero rows and columns when requested", {
  zero_triplet <- data.frame(i = as.integer(c(1, 2, 1, 2, 1, 2)), 
                             j = as.integer(c(1, 1, 2, 2, 3, 3)), 
                             value = c(0, 0, 0, 0, 0, 0)) |> 
    setrowtype("rows") |> setcoltype("cols")
  rindices <- data.frame(i = as.integer(c(1, 2)), 
                         rownames = c("r10", "r20"))
  cindices <- data.frame(j = as.integer(c(1, 2, 3)), 
                         colnames = c("c1", "c2", "c3"))
  index_map <- list(rindices, cindices)
  expected <- matrix(c(0, 0, 0, 
                       0, 0, 0), byrow = TRUE, 
                     nrow = 2, ncol = 3,
                     dimnames = list(c("r10", "r20"), c("c1", "c2", "c3"))) |> 
    setrowtype("rows") |> setcoltype("cols")
  res <- to_named_matrix(zero_triplet, 
                         index_map = index_map, 
                         matrix_class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(res, expected)
  
  # Try again with row and column names already characters
  zero_triplet_char <- data.frame(i = c("r10", "r20", "r10", "r20", "r10", "r20"), 
                                  j = c("c1", "c1", "c2", "c2", "c3", "c3"), 
                                  value = c(0, 0, 0, 0, 0, 0))
  res_char <- to_named_matrix(zero_triplet_char,
                              index_map = index_map, 
                              matrix_class = "Matrix") |> 
    setrowtype("rows") |> setcoltype("cols")
  matsbyname:::expect_equal_matrix_or_Matrix(res_char, expected)
})







