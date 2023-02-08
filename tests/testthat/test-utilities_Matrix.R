test_that("Matrix class is usable with matsbyname", {
  m <- Matrix::Matrix(c(1, 2, 
                        2, 3), byrow = TRUE, nrow = 2, ncol = 2)
  # Works
  expect_true(Matrix::isSymmetric(m))
  
  m2 <- m
  dimnames(m2) <- list(c("r1", "r2"), c("c1", "c2"))
  # Fails, because rownames are wrong
  expect_equal(dimnames(m2), list(c("r1", "r2"), c("c1", "c2"))) 
  
  # Try to set the rownames  
  rownames(m2) <- c("r1", "r2")
  # Fails, because the rownames are actually the column names
  expect_equal(rownames(m2), c("r1", "r2"))
  
  # Works
  expect_true(Matrix::isSymmetric(m2))
  
  m3 <- Matrix::Matrix(c(1, 2, 
                         2, 3), byrow = TRUE, nrow = 2, ncol = 2, 
                       dimnames = list(c("r1", "r2"), c("c1", "c2")))
  # Works
  expect_equal(rownames(m3), c("r1", "r2"))
  # Fails. The matrix itself is symmetric. 
  # The row names are not symmetric.
  # I think isSymmetric(m3) should return TRUE, but it returns FALSE.
  expect_true(Matrix::isSymmetric(m3))
})


test_that("I can create a non-symmetric sparse matrix", {
  # I want to create a sparse matrix if half or more elements are zero ...
  m <- Matrix::Matrix(c(1, 0, 2, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  # so that when I adjust its dimnames ...
  dimnames(m) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  # I get back what I assigned.
  expect_equal(dimnames(m), list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
})


test_that("A calculated symmetric Matrix doesn't lose row and column name information", {
  m <- Matrix::Matrix(c(1, 0, 3, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  dimnames(m) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  subtrahend <- Matrix::Matrix(c(0, 0, 1, 
                                 0, 0, 0, 
                                 0, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  m - subtrahend
})


test_that("inverting result in a sparse Matrix (or not)", {
  m <- Matrix::Matrix(c(1, 0, 3, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  invertedm <- solve(m)
  expect_true(inherits(invertedm, "matrix"))
  invertedM <- Matrix::Matrix(invertedm)
  expect_true(inherits(invertedM, "Matrix"))
  expect_true(inherits(invertedM, "dgCMatrix"))
})


test_that("I can create a sparse Matrix", {
  m <- Matrix::Matrix(c(1, 2, 3, 
                        4, 5, 6, 
                        7, 8, 9), byrow = TRUE, nrow = 3, ncol = 3)
  subtrahend <- Matrix::Matrix(c(0, 2, 3, 
                                 4, 5, 6, 
                                 7, 8, 9), byrow = TRUE, nrow = 3, ncol = 3)
  m - subtrahend
  Matrix::Matrix(m - subtrahend)
})


test_that("Constructon order doesn't matter for a symmetric matrix", {
  m1 <- Matrix::Matrix(c(1, 0, 2, 
                         0, 3, 0, 
                         2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3, 
                       dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  
  m2 <- Matrix::Matrix(c(1, 0, 2, 
                         0, 3, 0, 
                         2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  dimnames(m2) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  
  # These matrices should be identical  
  expect_equal(m1, m2)
})


test_that("Changing entries and dimnames makes a symmetric matrix asymmetric", {
  # This is symmetric Matrix
  m <- Matrix::Matrix(c(1, 0, 2, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  expect_true(inherits(m, "dsCMatrix"))
  # Change an entry to make it asymmetric, and the underlying class changes,
  # as expected.
  m2 <- m
  m2[1, 3] <- 42
  expect_true(inherits(m2, "dgCMatrix"))
  
  # Try the same procedure with dimnames.
  # As a reminder, Mikael said 
  # "Symmetry of dimnames(<symmetricMatrix>) is enforced"
  m3 <- m
  expect_true(inherits(m3, "dsCMatrix"))
  # Change the dimnames to make m3 asymmetric, 
  # and the underlying class does NOT change.
  # Seems like if "symmetry of dimnames is enforced" and 
  # if changing an element makes a dsCMatrix into a dgCMatrix, 
  # the next line should change the underlying class to dgCMatrix, too.
  # Alternatively, an error chould be emitted, something like
  # "Setting asymmetric dimnames is not permitted on a symmatric Matrix of class dsCMatrix".
  # That way, information loss will be avoided. 
  # But the user should not need to know about the underlying class.
  # I would prefer changing the underlying class from 
  # dsCMatrix to dgCMatrix.
  dimnames(m3) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  # Both of these expectations fail, because the underlying class has NOT changed.
  expect_false(inherits(m3, "dsCMatrix"))
  expect_true(inherits(m3, "dgCMatrix"))
})


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


test_that("equal_matrix_or_Matrix() works as expected", {
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_true(matsbyname:::equal_matrix_or_Matrix(A, A))

  B <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                      sparse = FALSE, doDiag = FALSE)
  expect_true(matsbyname:::equal_matrix_or_Matrix(A, B))
  
  C <- Matrix::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, C))
  
  D <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r3"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, D))
})


test_that("expect_equal_matrix_or_Matrix() works as expected", {
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(A, A)
  
  B <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                      sparse = FALSE, doDiag = FALSE)
  matsbyname:::expect_equal_matrix_or_Matrix(A, B)
  
  # Try with a matrix and a Matrix
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(a, B)

  b <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(A, b)
  
  # Try with 2 matrix objects
  matsbyname:::expect_equal_matrix_or_Matrix(a, b)
})


test_that("equal_matrix_or_Matrix() works with row and col types", { 
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  matsbyname:::expect_equal_matrix_or_Matrix(A, A)
  
  B <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, B))
  expect_false(matsbyname:::equal_matrix_or_Matrix(B, A))
  
  D <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, D))
  
  E <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("bogus")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, E))
  
  G <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("bogus") %>% setcoltype("cols")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, G))
})


