test_that("byname functions are slower than standard functions", {
  # Create a 10x10 matrix with names
  a <- matrix(1:100, byrow = TRUE,
              nrow = 10,
              ncol = 10, 
              dimnames = list(paste0("r", 1:10), paste0("c", 1:10))) |> 
    matsbyname::setrowtype("rows") |> 
    matsbyname::setcoltype("cols")
  # Create a similar matrix. 
  b <- a
  rownames(b) <- paste0("r", 10:1)
  colnames(b) <- paste0("c", 10:1)
  
  # Type "a" at the console to see the a matrix.
  # Type "b" at the console to see the b matrix.
  # Type "a + b" in the console to see the regular sum.
  # Type sum_byname(a, b) to see the sum that respects row and column names.
  
  
  n_reps <- 1000
  
  # Do the sum both ways and time the results.
  regular_time <- bench::mark(a + b, iterations = n_reps)
  byname_time <- bench::mark(sum_byname(a, b), iterations = n_reps)
  
  # Type "regular_time" at the console.  
  # I get a median of 1.02 microseconds
  # Type "byname_time" at the console. 
  # I get a median of 1.59 milliseconds. 
  # So the byname time is 1559x slower than the regular time.
  # That's what I want to speed up.  
  expect_true(byname_time$median > regular_time$median)
  
  # It would be great if you could profile 
  # sum_byname(a, b)
  # to find out where it is spending so much time.
})
