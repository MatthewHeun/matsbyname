test_that("Matrix class is usable with matsbyname", {
  md <- Matrix::Matrix(c(1, 2, 
                         2, 3), byrow = TRUE, nrow = 2, ncol = 2)
  expect_true(Matrix::isSymmetric(md))
  expect_true(inherits(md, "dsyMatrix"))
  
  m <- matsbyname::Matrix(c(1, 2, 
                            2, 3), byrow = TRUE, nrow = 2, ncol = 2)
  expect_true(Matrix::isSymmetric(m))
  expect_true(inherits(m, "dgeMatrix"))
  
  m2 <- md
  dimnames(m2) <- list(c("r1", "r2"), c("c1", "c2"))
  # Not identical, because rownames are wrong for a symmetric matrix.
  expect_false(identical(dimnames(m2), list(c("r1", "r2"), c("c1", "c2"))))
  
  # Try to set the rownames  
  rownames(m2) <- c("r1", "r2")
  # Fails, because rownames and colnames MUST be same for a symmetric matrix.
  # This leads to information loss, unfortunately. 
  expect_false(identical(dimnames(m2), list(c("r1", "r2"), c("c1", "c2"))))
  
  # Use the workaround provided by Mikael Jagen (author of the Matrix package)
  # Internally, matsbyname::Matrix uses as(m2, "generalMatrix")
  m2_gen <- matsbyname::Matrix(m2)
  # m2_gen is symmetric, because its row and column names are still same
  expect_true(Matrix::isSymmetric(m2_gen))
  expect_true(inherits(m2_gen, "dgeMatrix"))
  rownames(m2_gen) <- c("r1", "r2")
  # Now setting the dimnames works, because m2_gen is no longer a symmetric dsyMatrix.
  # Row and column name equality is not enforced on a dgeMatrix.
  expect_equal(dimnames(m2_gen), list(c("r1", "r2"), c("c1", "c2")))
  # m2_gen is not symmetric, because its row and column names are not same
  expect_false(Matrix::isSymmetric(m2_gen))
  
  m3 <- matsbyname::Matrix(c(1, 2, 
                             2, 3), byrow = TRUE, nrow = 2, ncol = 2, 
                           dimnames = list(c("r1", "r2"), c("c1", "c2")))
  # Works
  expect_equal(rownames(m3), c("r1", "r2"))
  # The rownames are part of determining symmetry, 
  # so we expect m3 to be asymmetric.
  expect_false(Matrix::isSymmetric(m3))
})


test_that("I can create a non-symmetric sparse matrix", {
  # I want to create a sparse matrix if half or more elements are zero ...
  m <- matsbyname::Matrix(c(1, 0, 2, 
                            0, 3, 0, 
                            2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  # so that when I adjust its dimnames ...
  dimnames(m) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  # I get back what I assigned.
  expect_equal(dimnames(m), list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
})


test_that("inverting results in a sparse Matrix (or not)", {
  m <- matsbyname::Matrix(c(1, 0, 3, 
                            0, 3, 0, 
                            2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  invertedm <- Matrix::solve(m)
  expect_true(inherits(invertedm, "Matrix"))
  invertedM <- Matrix::Matrix(invertedm)
  expect_true(inherits(invertedM, "Matrix"))
  expect_true(inherits(invertedM, "dgCMatrix"))
})


test_that("I can create a sparse Matrix", {
  m <- matsbyname::Matrix(c(1, 2, 3, 
                            4, 5, 6, 
                            7, 8, 9), byrow = TRUE, nrow = 3, ncol = 3)
  subtrahend <- matsbyname::Matrix(c(0, 2, 3, 
                                     4, 4, 6, 
                                     7, 8, 6), byrow = TRUE, nrow = 3, ncol = 3)
  res <- m - subtrahend
  # It is a symmetric matrix
  expect_true(Matrix::isSymmetric(res))
  # But the underlying class has not jumped to a symmetric class
  expect_true(inherits(res, "dgeMatrix"))
  # Now push it through the Matrix creation process again.
  res2 <- Matrix::Matrix(res)
  # The matrix is sparse and symmetric, but not a symmetric subclass
  expect_true(Matrix::isSymmetric(res2))
  expect_true(is(res2, "sparseMatrix"))
  
  # Try with non-diagonal elements
  subtrahend2 <- Matrix::Matrix(c(0, 2, 2, 
                                  4, 5, 6, 
                                  6, 8, 8), byrow = TRUE, nrow = 3, ncol = 3)
  res3 <- m - subtrahend2
  # It is a symmetric matrix
  expect_true(Matrix::isSymmetric(res3))
  # But the underlying class has not jumped to a symmetric class
  expect_true(inherits(res3, "dgeMatrix"))
  # Now push it through the Matrix creation process again.
  res4 <- Matrix::Matrix(res3)
  # The matrix is sparse and symmetric, but not a symmetric subclass
  expect_true(Matrix::isSymmetric(res4))
  expect_true(is(res4, "sparseMatrix"))
})


test_that("Constructon order doesn't matter for a symmetric matrix", {
  m1 <- Matrix::Matrix(c(1, 0, 2, 
                         0, 3, 0, 
                         2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3, 
                       dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  
  m2 <- Matrix::Matrix(c(1, 0, 2, 
                         0, 3, 0, 
                         2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3) %>% 
    # This is the important bit.
    # If we convert it to a "generalMatrix", 
    # an underlying symmetric matrix class won't be used,
    # and dimname assignment WILL work.
    as("generalMatrix")
  dimnames(m2) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))

  # These matrices are equal, but only because
  # we were careful to convert to a generalMatrix, 
  # thereby avoiding creating a symmetric matrix subclass.  
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
  
  # Need to change to a generalMatrix
  # before dimnames will stick.
  # Mikael said "Symmetry of dimnames(<symmetricMatrix>) is enforced"
  # for symmetric matrix classes.
  m3 <- as(m, "generalMatrix")
  expect_true(inherits(m3, "dgCMatrix"))
  # Now assign dimnames.
  dimnames(m3) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  # Both of these expectations work, 
  # because the underlying class changed 
  # when we did as(m, "generalMatrix")
  expect_false(inherits(m3, "dsCMatrix"))
  expect_true(inherits(m3, "dgCMatrix"))
})


test_that("A sparse matrix cannot be coerced to a symmetric matrix", {
  x <- Matrix::Matrix(c(1, 0, 2, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3L, ncol = 3L)
  expect_true(inherits(x, "dsCMatrix"))
  y <- as(x, "generalMatrix")
  expect_true(inherits(y, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when summed with the 0 Matrix
  z <- y + Matrix::Matrix(0, nrow = 3, ncol = 3)
  # y is not coerced to a symmetric Matrix.  Good!
  expect_true(inherits(z, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when element-multiplied by the unity Matrix
  a <- y * Matrix::Matrix(1, nrow = 3, ncol = 3)
  # y is not coerced to a symmetric Matrix.  Good!
  expect_true(inherits(a, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when matrix-multiplied by the identity Matrix
  b <- y * Matrix::Matrix(c(1, 0, 0,
                            0, 1, 0, 
                            0, 0, 1), nrow = 3, ncol = 3)
  # y is not coerced to a symmetric Matrix.  Good!
  expect_true(inherits(b, "dgCMatrix"))
  
  # Try summing with a symmetric matrix.
  d <- y + x
  expect_true(inherits(d, "dgCMatrix"))
  e <- x + y
  expect_true(inherits(e, "dgCMatrix"))
  
  # Try with a non-trivial operation
  m <- Matrix::Matrix(c(1, 0, 3, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  subtrahend <- Matrix::Matrix(c(0, 0, 1, 
                                 0, 0, 0, 
                                 0, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  # The result is symmetric
  res <- m - subtrahend
  # But not represented by the dsCMatrix class.
  expect_true(inherits(res, "dgCMatrix"))
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


