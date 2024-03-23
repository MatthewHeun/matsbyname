test_that("to_indexed() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2")))
  r_indices <- data.frame(names = c("r3", "r1", "r2", "r0"),
                          indices = as.integer(c(5, 9, 7, 100))) 
  c_indices <- data.frame(names = c("c2", "c1", "c3"), 
                          indices = as.integer(c(4, 3, 100)))
  indices <- dplyr::bind_rows(r_indices, c_indices)
  
  # Try with a single 
  res <- to_indexed(m, indices)
  expect_equal(res, tibble::tribble(~i, ~j, ~x, 
                                    9, 3, 1, 
                                    7, 3, 2, 
                                    5, 3, 3, 
                                    9, 4, 4, 
                                    7, 4, 5, 
                                    5, 4, 6))
})
