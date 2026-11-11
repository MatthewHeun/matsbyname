test_that("outerproduct_byname() works as expected with 1x1 matrices", {
  A <- matrix(2, nrow = 1, ncol = 1, dimnames = list("Ar1", "Ac1"))
  B <- matrix(10, nrow = 1, ncol = 1, dimnames = list("Br1", "Bc1"))
  expected <- outer(A, B)
  res <- outerproduct_byname(A, B)
  expect_equal(res, expected)
  
  # Try in lists
  res_list <- outerproduct_byname(list(A, A), list(B, B))
  expect_equal(res_list, list(expected, expected))
})


test_that("outerproduct_byname() works as expected with 2x1 and 1x1 matrices", {
  A <- matrix(c(2, 3), nrow = 2, ncol = 1, dimnames = list(c("Ar1", "Ar2"), "Ac1"))
  B <- matrix(10, nrow = 1, ncol = 1, dimnames = list("Br1", "Bc1"))
  expectedAB <- outer(A, B)
  res <- outerproduct_byname(A, B)
  expect_equal(res, expectedAB)
  
  # Try in lists
  C <- matrix(20, nrow = 1, ncol = 1, dimnames = list("Cr1", "Cc1"))
  expectedAC <- outer(A, C)
  res_list <- outerproduct_byname(list(A, A), list(B, C))
  expect_equal(res_list, list(expectedAB, expectedAC))
})

