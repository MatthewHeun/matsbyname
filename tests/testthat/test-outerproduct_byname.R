test_that("outerproduct_byname() works as expected with 1x1 matrices", {
  A <- matrix(2, nrow = 1, ncol = 1, dimnames = list("Ar1", "Ac1"))
  B <- matrix(10, nrow = 1, ncol = 1, dimnames = list("Br1", "Bc1"))
  expected <- outer(A, B)
  res <- outerproduct_byname(A, B)
  expect_equal(res, expected)
  
  # Try in lists
  res_list <- outerproduct_byname(list(A, A), list(B, B))
  expect_equal(res_list, list(expected, expected))
  
  # Try with row and column types.
  # These should be ignored and stripped off of the result.
  C <- A |> setrowtype("rows") |> setcoltype("cols")
  D <- B |> setrowtype("rows") |> setcoltype("cols")
  res_no_types <- outerproduct_byname(C, D)
  expect_null(rowtype(res_no_types))
  expect_null(coltype(res_no_types))
})


test_that("outerproduct_byname() works as expected with 2x1 and 1x1 matrices", {
  A <- matrix(c(2, 3), nrow = 2, ncol = 1, dimnames = list(c("Ar1", "Ar2"), "Ac1"))
  B <- matrix(10, nrow = 1, ncol = 1, dimnames = list("Br1", "Bc1"))
  expectedAB <- outer(A, B)
  res <- outerproduct_byname(A, B)
  expect_equal(res, expectedAB)

  # Switch the order of arguments
  res_switched <- outerproduct_byname(B, A)
  
  # Try in lists
  C <- matrix(20, nrow = 1, ncol = 1, dimnames = list("Cr1", "Cc1"))
  expectedAC <- outer(A, C)
  res_list <- outerproduct_byname(list(A, A), list(B, C))
  expect_equal(res_list, list(expectedAB, expectedAC))
  
  # Try in a data frame
  df <- tibble::tibble(Acol = list(A, A, A), 
                       Ccol = list(C, C, C))
  res_df <- df |> 
    dplyr::mutate(
      AC = outerproduct_byname(Acol, Ccol)
    )
  expect_true("AC" %in% colnames(res_df))
  expect_equal(res_df$AC, list(expectedAC, expectedAC, expectedAC))
})


test_that("outerproduct_byname() works with vectors", {
  A <- c(r1 = 1, r2 = 2, r3 = 3)
  B <- c(c1 = 1, c2 = 2, c3 = 3, c4 = 4)
  # Build a multiplication table
  expected <- outer(A, B)
  res <- outerproduct_byname(A, B)
  expect_equal(res, expected)
})
