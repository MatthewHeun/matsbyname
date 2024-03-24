test_that("to_indexed() works as expected", {
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
                              5, 4, 6)
  # This should error, because we need a list of 2 or more
  expect_error(to_indexed(m, indices), regexp = "index_map must be a list and not a data frame")
  
  # Try with 2 unnamed data frames
  indices2 <- list(r_indices, c_indices)
  expect_equal(to_indexed(m, indices2), expected)
  
  # Try with the index data frames in the wrong order.
  indices3 <- list(c_indices, r_indices)
  expect_error(to_indexed(m, indices3))
  
  # Try with named data frames (and add a third to test the code)
  unused_rindices <- data.frame(rnames = c("r3u", "r1u", "r2u", "r0u"),
                                rindices = as.integer(c(5, 9, 7, 100))) 
  unused_cindices <- data.frame(cnames = c("c2u", "c1u", "c3u"), 
                                cindices = as.integer(c(4, 3, 100)))
  indices4 <- list(unusedr = unused_rindices, 
                   cols = c_indices, 
                   rows = r_indices, 
                   unusedc = unused_cindices)
  expect_equal(to_indexed(m, indices4), expected)
})


test_that("to_indexed() fails when index_map contains repeated information", {
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
  expect_error(to_indexed(m, list(r_indices, c_indices)), 
               regexp = "All indices and names must be unique in to_indexed")
})

test_that("to_indexed() works with lists", {
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
                              5, 4, 6)
  
  # Try with 2 unnamed data frames
  indices <- list(r_indices, c_indices)
  expect_equal(to_indexed(m_list, indices), 
               list(expected, expected, expected))
  
})
  