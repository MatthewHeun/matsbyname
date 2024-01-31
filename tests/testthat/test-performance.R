# This file contains performance tests (speed and memory)
# for different kinds of matrices.

test_that("calculation speed", {
  
  # Don't run these tests on continuous integration systems
  skip_on_ci()
  
  min_time <- 0.01 # seconds, default is 0.5 seconds
  small_mat_dimnames <- list(paste("My big long row name [from a suffix]", 1:3), 
                             paste("My big long col name [from a suffix]", 1:2))
  small_mat_dns <- matrix(1:6, nrow = 3, ncol = 2, dimnames = small_mat_dimnames)
  small_mat_nodns <- matrix(small_mat_dns, nrow = 3, ncol = 2, dimnames = NULL)
  t_small_mat_dns <- t(small_mat_dns)
  t_small_mat_nodns <- t(small_mat_nodns)
  
  small_ijx_dns <- Matrix::Matrix(small_mat_dns, sparse = TRUE)
  small_ijx_nodns <- Matrix::Matrix(small_mat_nodns, sparse = TRUE)
  t_small_ijx_dns <- Matrix::t(small_ijx_dns)
  t_small_ijx_nodns <- Matrix::t(small_ijx_nodns)
  
  
  big_mat_dimnames <- list(paste("My big long row name [from a suffix]", 1:100), 
                           paste("My big long col name [from a suffix]", 1:100))
  big_mat_dns <- matrix(1:(100*100), nrow = 100, ncol = 100, dimnames = big_mat_dimnames) * 0
  big_mat_dns[ , 1] <- 1:100
  big_mat_nodns <- matrix(big_mat_dns, dimnames = NULL)
  t_big_mat_dns <- t(big_mat_dns)
  t_big_mat_nodns <- t(big_mat_nodns)
  big_ijx_dns <- Matrix::Matrix(big_mat_dns, sparse = TRUE)
  big_ijx_nodns <- Matrix::Matrix(big_mat_nodns, sparse = TRUE)
  t_big_ijx_dns <- Matrix::t(big_ijx_dns)
  t_big_ijx_nodns <- Matrix::t(big_ijx_nodns)
  
  suppressWarnings(
    current_sum_small <- bench::mark(matsbyname::sum_byname(small_ijx_dns, small_ijx_dns), time_unit = "ms", min_time = min_time) |> 
      magrittr::extract2("median"))
  future_sum_small = bench::mark(small_ijx_nodns + small_ijx_nodns, time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  speedup_sum_small <- current_sum_small/future_sum_small # 7.4
  expect_true(speedup_sum_small > 1)

  current_prod_small <- bench::mark(matsbyname::matrixproduct_byname(small_ijx_dns, t_small_ijx_dns), time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  future_prod_small = bench::mark(big_ijx_nodns %*% t_big_ijx_nodns, time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  speedup_prod_small <- current_prod_small/future_prod_small
  expect_true(speedup_prod_small > 1) # 15.4
  
  current_sum_big <- bench::mark(matsbyname::sum_byname(big_ijx_dns, big_ijx_dns), time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  future_sum_big = bench::mark(big_ijx_nodns + big_ijx_nodns, time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  speedup_sum_big <- current_sum_big/future_sum_big # 8.6
  # expect_true(speedup_sum_big > 1) # Fails on GitHub actions for macOS

  current_prod_big <- bench::mark(matsbyname::matrixproduct_byname(big_ijx_dns, t_big_ijx_dns), time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  future_prod_big = bench::mark(big_ijx_nodns %*% t_big_ijx_nodns, time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  speedup_prod_big <- current_prod_big/future_prod_big # 22.9
  expect_true(speedup_prod_big > 1)
  
  # compare an object without dimnames going through the matsbyname function to one that does not.
  current_sum_big_nodns <- bench::mark(matsbyname::sum_byname(big_ijx_nodns, big_ijx_nodns), time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  future_sum_big_nodns <- bench::mark(big_ijx_nodns + big_ijx_nodns, time_unit = "ms", min_time = min_time) |> 
    magrittr::extract2("median")
  speedup_sum_big_nodns <- current_sum_big_nodns/future_sum_big_nodns # 3.5
  expect_true(speedup_sum_big_nodns > 1)
})


test_that("object sizes", {
  
  small_mat_dimnames <- list(paste("My big long row name [from a suffix]", 1:3), 
                             paste("My big long col name [from a suffix]", 1:2))
  size_small_mat_dimnames <- object.size(small_mat_dimnames) # 688 bytes
  
  small_mat <- matrix(1:6, nrow = 3, ncol = 2, dimnames = small_mat_dimnames)
  size_small_mat_dns <- object.size(small_mat) # 1048 bytes
  size_small_mat_nodns <- object.size(matrix(small_mat, dimnames = NULL)) # 248 bytes
  expect_true(size_small_mat_nodns < size_small_mat_dns)  
  expect_true(size_small_mat_nodns + size_small_mat_dimnames < size_small_mat_dns) # 248 bytes + 68 bytes = 936 bytes < 1048 bytes

  
  df_small_ijx <- tibble::tribble(~i, ~j, ~x, 
                                  1, 1, 1, 
                                  1, 2, 4, 
                                  2, 1, 2, 
                                  2, 2, 5, 
                                  3, 1, 3, 
                                  3, 2, 6)
  small_ijx_dns <- Matrix::sparseMatrix(i = df_small_ijx$i, j = df_small_ijx$j, x = df_small_ijx$x, 
                                        dimnames = small_mat_dimnames)
  size_small_ijx_dns <- object.size(small_ijx_dns) # 2216 bytes
  small_ijx_nodns <- Matrix::sparseMatrix(i = df_small_ijx$i, j = df_small_ijx$j, x = df_small_ijx$x)
  size_small_ijx_nodns <- object.size(small_ijx_nodns) # 1592 bytes
  expect_true(size_small_ijx_nodns + size_small_mat_dimnames > size_small_ijx_dns) # 1592 bytes + 688 bytes = 2280 bytes > 2216 bytes
  
  
  big_mat_dimnames <- list(paste("My big long row name [from a suffix]", 1:100), 
                           paste("My big long col name [from a suffix]", 1:100))
  size_big_mat_dimnames <- object.size(big_mat_dimnames) # 20960 bytes
  big_mat_dns <- matrix(1:(100*100), nrow = 100, ncol = 100, dimnames = big_mat_dimnames) * 0
  big_mat_dns[ , 1] <- 1:100
  size_big_mat_dns <- object.size(big_mat_dns) # 101288 bytes
  size_big_mat_nodns <- object.size(matrix(big_mat_dns, dimnames = NULL)) # 80216 bytes
  
  df_big_ijx <- data.frame(i = 1:100, j = 1, x = 1:100)
  big_ijx_dns <- Matrix::sparseMatrix(i = df_big_ijx$i, 
                                      j = df_big_ijx$j, 
                                      x = df_big_ijx$x, 
                                      dims = c(100, 100), 
                                      dimnames = big_mat_dimnames)
  size_big_ijx_dns <- object.size(big_ijx_dns) # 24000 bytes
  
  big_ijx_nodns <- Matrix::sparseMatrix(i = df_big_ijx$i, 
                                        j = df_big_ijx$j, 
                                        x = df_big_ijx$x, 
                                        dims = c(100, 100))
  size_big_ijx_nodns <- object.size(big_ijx_nodns) # 3104 bytes

  expect_true(size_big_ijx_nodns < size_big_ijx_dns) # 3104 bytes < 24000 bytes, factor of 7.7 improvement
  
  expect_true(size_big_ijx_nodns < size_big_mat_dns) # 3104 bytes < 101288 bytes, factor of 32 improvement
})


test_that("operations on sparse matrices work in dplyr::mutate()", {
  small_mat <- Matrix::Matrix(c(11, 21, 31, 12, 22, 32), nrow = 3, ncol = 2, sparse = TRUE)
  small_mat_list <- list(small_mat, small_mat, small_mat)
  m1str <- "m1"
  m2str <- "m2"
  m3str <- "m3"
  m4str <- "m4"
  df <- tibble::tibble(m1 = small_mat_list, m2 = small_mat_list)
  res <- df |> 
    dplyr::mutate(
      "{m3str}" := Map(`+`, .data[[m1str]], .data[[m2str]]), 
      "{m4str}" := Map(`%*%`, .data[[m2str]], .data[[m2str]] |> lapply(FUN = Matrix::t))
    )
  expected_m3 <- mapply(`+`, df$m1, df$m2)  
  expect_equal(res$m3, expected_m3)
  expected_m4 <- Map(f = `%*%`, df$m2, df$m2 |> lapply(FUN = Matrix::t))
  expect_equal(res$m4, expected_m4)
  # Check that results are correct
  small_mat_prod <- small_mat %*% Matrix::t(small_mat)
  for (i in 1:nrow(df)) {
    expect_equal(res$m4[[i]], small_mat_prod)  
  }
})
