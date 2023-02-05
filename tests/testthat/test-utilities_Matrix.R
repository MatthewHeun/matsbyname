
test_that("is_matrix_or_Matrix() works correctly", {
  expect_false(matsbyname:::is_matrix_or_Matrix(42))
  expect_false(matsbyname:::is_matrix_or_Matrix("42"))
  
  expect_true(matsbyname:::is_matrix_or_Matrix(matrix(1)))
  expect_true(matsbyname:::is_matrix_or_Matrix(Matrix::Matrix(1)))
})


test_that("t.matrix_or_Matrix() works correctly", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  res_m <- matsbyname:::t_matrix_or_Matrix(a)
  expected_m <- t(a)
  expect_equal(res_m, expected_m)
    
  # Try with a Matrix object
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  res_M <- matsbyname:::t_matrix_or_Matrix(A)
  expected_M <- Matrix::t(A)
  expect_equal(res_M, expected_M)
})


test_that("cbind_matrix_or_Matrix() works correctly", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c3"))  
  res1 <- matsbyname:::cbind_matrix_or_Matrix(a, b)
  expected1 <- matrix(c(0, 0, 1,
                        0, 0, 1), nrow = 2, ncol = 3, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(res1, expected1)
  
  # Try with Matrix.
  A <- Matrix::Matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))  
  B <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c2", "c3")))
  res2 <- matsbyname:::cbind_matrix_or_Matrix(A, B)
  expected2 <- Matrix::Matrix(c(1, 0, 0,
                                1, 0, 0), nrow = 2, ncol = 3, byrow = TRUE, 
                              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(res2, expected2)
  
})


test_that("rbind_matrix_or_Matrix() works correctly", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matrix(1, nrow = 1, ncol = 2, dimnames = list("r3", c("c1", "c2")))  
  res1 <- matsbyname:::rbind_matrix_or_Matrix(a, b)
  expected1 <- matrix(c(0, 0,
                        0, 0, 
                        1, 1), nrow = 3, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(res1, expected1)
  
  # Try with Matrix.
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  B <- Matrix::Matrix(1, nrow = 1, ncol = 2, dimnames = list("r3", c("c1", "c2")))  
  res2 <- matsbyname:::rbind_matrix_or_Matrix(A, B)
  expected2 <- Matrix::Matrix(c(0, 0,
                                0, 0, 
                                1, 1), nrow = 3, ncol = 2, byrow = TRUE, 
                              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(res2, expected2)
  
})
