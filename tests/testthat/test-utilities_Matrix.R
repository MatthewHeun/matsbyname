# The first few tests work toward understanding
# how the Matrix package works.

test_that("Constructon order doesn't matter for a symmetric matrix", {
  # The assignment of m1 works (without information loss), because
  # asymmetric dimnames are assigned when the matrix is created.
  # Thus, m1 is created as a dgCMatrix (genericMatrix),
  # which allows separate row and column names.
  m1 <- Matrix::Matrix(c(1, 0, 2, 
                         0, 3, 0, 
                         2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3, 
                       dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  
  m2 <- matsbyname::Matrix(c(1, 0, 2, 
                             0, 3, 0, 
                             2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  # matsbyname::Matrix() converts to a "generalMatrix". 
  # An underlying symmetric matrix class won't be used,
  # and dimname assignment WILL work.
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


test_that("A sparse Matrix cannot be coerced to a symmetric Matrix", {
  x <- Matrix::Matrix(c(1, 0, 2, 
                        0, 3, 0, 
                        2, 0, 0), byrow = TRUE, nrow = 3L, ncol = 3L)
  expect_true(inherits(x, "dsCMatrix"))
  y <- as(x, "generalMatrix")
  expect_true(inherits(y, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when summed with the 0 Matrix (a ddiMatrix)
  z <- y + Matrix::Matrix(0, nrow = 3, ncol = 3)
  # y is not coerced to a symmetric Matrix.  Good!
  expect_true(inherits(z, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when element-multiplied by the unity Matrix
  a <- y * Matrix::Matrix(1, nrow = 3, ncol = 3)
  # y is not coerced to a symmetric Matrix.  Good!
  expect_true(inherits(a, "dgCMatrix"))
  
  # See what happens to a general sparse Matrix when matrix-multiplied by the identity Matrix
  # (a ddiMatrix)
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
  expect_true(Matrix::isSymmetric(res))
  # But not represented by the dsCMatrix class.
  expect_true(inherits(res, "dgCMatrix"))
})


# The following tests determine if matsbyname::Matrix() 
# is usable with matsbyname functions.

test_that("matsbyname::Matrix() is usable with matsbyname", {
  # Matrix::Matrix() makes a symmetric Matrix, which we do not want.
  ms <- Matrix::Matrix(c(1, 2, 
                         2, 3), byrow = TRUE, nrow = 2, ncol = 2)
  expect_true(Matrix::isSymmetric(ms))
  expect_true(inherits(ms, "dsyMatrix"))
  # matsbyname::Matrix() creates a generalMatrix, which is useable in matsbyname.
  m <- matsbyname::Matrix(c(1, 2, 
                            2, 3), byrow = TRUE, nrow = 2, ncol = 2)
  expect_true(Matrix::isSymmetric(m))
  expect_true(inherits(m, "dgeMatrix"))
  
  m2 <- ms
  dimnames(m2) <- list(c("r1", "r2"), c("c1", "c2"))
  # Make sure we have not changed the class of m2 by trying to set its
  # dimnames by be asymmetric.
  expect_true(inherits(m2, "dsyMatrix"))
  # Not identical, because rownames are wrong for a symmetric matrix.
  # The next test passes on my mac and 4/5 platforms in the GitHub actions test suite.
  # It fails on ubuntu-latest (oldrel-1)
  # So I'm commenting it for now.
  # expect_false(identical(dimnames(m2), list(c("r1", "r2"), c("c1", "c2"))))
  # In fact, m2 retains the column names for its row names.
  # The next test passes on my mac and 4/5 platforms in the GitHub actions test suite.
  # It fails on ubuntu-latest (oldrel-1)
  # So I'm commenting it for now.
  # expect_equal(dimnames(m2), list(c("c1", "c2"), c("c1", "c2")))
  
  # Try to set the rownames  
  rownames(m2) <- c("r1", "r2")
  # Fails, because rownames and colnames MUST be same for a symmetric matrix.
  # This leads to information loss, unfortunately. 
  # The next test passes on my mac and 4/5 platforms in the GitHub actions test suite.
  # It fails on ubuntu-latest (oldrel-1)
  # So I'm commenting it for now.
  # expect_false(identical(dimnames(m2), list(c("r1", "r2"), c("c1", "c2"))))
  
  # Use the workaround provided by Mikael Jagen (author of the Matrix package)
  # Internally, matsbyname::Matrix uses as(m2, "generalMatrix")
  m2_gen <- matsbyname::Matrix(m2)
  # m2_gen is symmetric, because its row and column names are still same
  # The next test passes on my mac and 4/5 platforms in the GitHub actions test suite.
  # It fails on ubuntu-latest (oldrel-1)
  # So I'm commenting it for now.
  # expect_true(Matrix::isSymmetric(m2_gen))
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


test_that("is.Matrix() works as expected", {
  expect_true(is.Matrix(Matrix(42)))
  expect_false(is.Matrix(matrix(42)))
})


test_that("matsbyname::Matrix() creates a sparse Matrix", {
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
  res2 <- matsbyname::Matrix(res)
  # The matrix is sparse and symmetric, but not a symmetric subclass
  expect_true(Matrix::isSymmetric(res2))
  expect_true(is(res2, "sparseMatrix"))
  
  # Try with non-diagonal elements
  subtrahend2 <- matsbyname::Matrix(c(0, 2, 2, 
                                      4, 5, 6, 
                                      6, 8, 8), byrow = TRUE, nrow = 3, ncol = 3)
  res3 <- m - subtrahend2
  # It is a symmetric matrix
  expect_true(Matrix::isSymmetric(res3))
  # But the underlying class has not jumped to a symmetric class
  expect_true(inherits(res3, "dgeMatrix"))
  # Now push it through the Matrix creation process again.
  res4 <- matsbyname::Matrix(res3)
  # The matrix is sparse and symmetric, but not a symmetric subclass
  expect_true(Matrix::isSymmetric(res4))
  expect_true(is(res4, "sparseMatrix"))
})


test_that("matsbyname::Matrix() creates a non-symmetric sparse matrix", {
  # I want to create a sparse matrix if half or more elements are zero ...
  m <- matsbyname::Matrix(c(1, 0, 2, 
                            0, 3, 0, 
                            2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  # so that when I adjust its dimnames ...
  dimnames(m) <- list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))
  # I get back what I assigned.
  expect_equal(dimnames(m), list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
})


test_that("matsbyname::Matrix() picks up row and col type from matrix objects", {
  m <- matrix(42, dimnames = list("r1", "r2")) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  M <- matsbyname::Matrix(m)
  expect_equal(rowtype(M), "rows")
  expect_equal(coltype(M), "cols")
  
  # Check that it picks up argument values as overrides
  m3 <- matrix(42, dimnames = list("r1", "r2")) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  M3 <- matsbyname::Matrix(m3, rowtype = "rows3", coltype = "cols3")
  expect_equal(rowtype(M3), "rows3")
  expect_equal(coltype(M3), "cols3")
})


test_that("matsbyname::Matrix() correctly deals with override dimnames", {
  m <- matrix(c(1, 0, 3, 
                0, 3, 0, 
                2, 0, 0), 
              byrow = TRUE, nrow = 3, ncol = 3, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  M <- matsbyname::Matrix(m, dimnames = list(c("a", "b", "c"), c("d", "e", "f")))
  expect_equal(dimnames(M), list(c("a", "b", "c"), c("d", "e", "f")))
})


test_that("matsbyname::Matrix() is vectorized", {
  m <- matrix(c(1, 0, 3, 
                0, 3, 0, 
                2, 0, 0), 
              byrow = TRUE, nrow = 3, ncol = 3, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  M <- matsbyname::Matrix(m)
  m_list <- list(m, m)
  M_list <- matsbyname::Matrix(m_list)
  expect_equal(M_list, list(M, M))
  df <- tibble::tibble(m = list(m, m, m))
  dfM <- df |> 
    dplyr::mutate(
      M = matsbyname::Matrix(m)
    )
  expect_equal(dfM$M[[1]], M)
  expect_equal(dfM$M[[2]], M)
  expect_equal(dfM$M[[3]], M)
})


test_that("Matrix::solve() results in a sparse Matrix (or not)", {
  m <- matsbyname::Matrix(c(1, 0, 3, 
                            0, 3, 0, 
                            2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
  invertedm <- Matrix::solve(m)
  expect_true(is.Matrix(invertedm))
  invertedM <- matsbyname::Matrix(invertedm)
  expect_true(is.Matrix(invertedM))
})


test_that("is_matrix_or_Matrix() works correctly", {
  expect_false(matsbyname:::is_matrix_or_Matrix(42))
  expect_false(matsbyname:::is_matrix_or_Matrix("42"))
  
  expect_true(matsbyname:::is_matrix_or_Matrix(matrix(1)))
  expect_true(matsbyname:::is_matrix_or_Matrix(Matrix::Matrix(1)))
  expect_true(matsbyname:::is_matrix_or_Matrix(matsbyname::Matrix(1)))
})


test_that("t.matrix_or_Matrix() works correctly", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  res_m <- matsbyname:::t_matrix_or_Matrix(a)
  expected_m <- t(a)
  expect_equal(res_m, expected_m)

  # Try with a Matrix object
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  res_M <- matsbyname:::t_matrix_or_Matrix(A)
  expected_M <- Matrix::t(A)
  expect_equal(res_M, expected_M)
})


test_that("t.matrix_or_Matrix() works with spaces in rownames", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r 1", "r 2"), c("c 1", "c 2")))
  expected_m <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("c 1", "c 2"), c("r 1", "r 2")))
  res_m <- matsbyname:::t_matrix_or_Matrix(a)
  expect_equal(res_m, expected_m)
  
  # Try with a Matrix object
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r 1", "r 2"), c("c 1", "c 2")))
  expected_A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("c 1", "c 2"), c("r 1", "r 2")))
  res_M <- matsbyname:::t_matrix_or_Matrix(A)
  expect_equal(res_M, expected_A)
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
  A <- matsbyname::Matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))  
  B <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c2", "c3")))
  res2 <- matsbyname:::cbind_matrix_or_Matrix(A, B)
  expected2 <- matsbyname::Matrix(c(1, 0, 0,
                                1, 0, 0), nrow = 2, ncol = 3, byrow = TRUE, 
                              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(res2, expected2)
})


test_that("rbind_matrix_or_Matrix() works correctly", {
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  b <- matrix(1, nrow = 1, ncol = 2, dimnames = list("r3", c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res1 <- matsbyname:::rbind_matrix_or_Matrix(a, b)
  expected1 <- matrix(c(0, 0,
                        0, 0, 
                        1, 1), nrow = 3, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(res1, expected1)
  
  # Try with Matrix.
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  B <- matsbyname::Matrix(1, nrow = 1, ncol = 2, dimnames = list("r3", c("c1", "c2")))  
  res2 <- matsbyname:::rbind_matrix_or_Matrix(A, B)
  expected2 <- matsbyname::Matrix(c(0, 0,
                                    0, 0, 
                                    1, 1), nrow = 3, ncol = 2, byrow = TRUE, 
                                  dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(res2, expected2)
})


test_that("equal_matrix_or_Matrix() works as expected", {
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_true(matsbyname:::equal_matrix_or_Matrix(A, A))

  B <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                      sparse = FALSE, doDiag = FALSE)
  expect_true(matsbyname:::equal_matrix_or_Matrix(A, B))
  
  C <- matsbyname::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, C))
  
  D <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r3"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, D))
})


test_that("expect_equal_matrix_or_Matrix() works as expected", {
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(A, A)
  
  B <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
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
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), rowtype = "rows", coltype = "cols")
  matsbyname:::expect_equal_matrix_or_Matrix(A, A)
  
  B <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, B))
  expect_false(matsbyname:::equal_matrix_or_Matrix(B, A))
  
  D <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, D))
  
  E <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("bogus")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, E))
  
  G <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("bogus") %>% setcoltype("cols")
  expect_false(matsbyname:::equal_matrix_or_Matrix(A, G))
})


test_that("rowSums_matrix_or_Matrix() works as expected", {
  m <- matrix(c(10, 20, 30, 
                40, 50, 60), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res1 <- matsbyname:::rowSums_matrix_or_Matrix(m)
  expect_equal(res1, matrix(c(60, 
                              150), byrow = TRUE, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "cols")) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  
  M <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")), 
                          rowtype = "rows", coltype = "cols")
  res2 <- matsbyname:::rowSums_matrix_or_Matrix(M)
  expect_equal(res2, matsbyname::Matrix(c(3, 
                                          7, 
                                          11), byrow = TRUE, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "cols"), 
                                        rowtype = "rows", coltype = "cols"))
})


test_that("colsSums_matrix_or_Matrix() works as expected", {
  m <- matrix(c(10, 20, 30, 
                40, 50, 60), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res1 <- matsbyname:::colSums_matrix_or_Matrix(m)
  expect_equal(res1, matrix(c(50, 70, 90), byrow = TRUE, nrow = 1, ncol = 3, dimnames = list("rows", c("c1", "c2", "c3"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  
  M <- matsbyname::Matrix(c(1, 2, 3,
                            4, 5, 6), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                          rowtype = "rows", coltype = "cols")
  res2 <- matsbyname:::colSums_matrix_or_Matrix(M)
  expect_equal(res2, matsbyname::Matrix(c(5, 7, 9), byrow = TRUE, nrow = 1, ncol = 3, dimnames = list("rows", c("c1", "c2", "c3")), 
                                        rowtype = "rows", coltype = "cols"))
})


test_that("check_row_col_types() works as expected", {
  a <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r", "c")) %>% 
    setrowtype("row") %>% setcoltype("col")
  b <- matsbyname::Matrix(42, nrow = 1, ncol = 1, dimnames = list("r", "c"))
  res <- matsbyname:::check_row_col_types(a, b)
  expect_true(rowtype(res[[1]]) == rowtype(res[[2]]))
  expect_true(coltype(res[[1]]) == coltype(res[[2]]))

  res2 <- matsbyname:::check_row_col_types(b, a)  
  expect_true(rowtype(res2[[1]]) == rowtype(res2[[2]]))
  expect_true(coltype(res2[[1]]) == coltype(res2[[2]]))
  
  C <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r", "c")) %>% 
    setrowtype("row") %>% setcoltype("col")
  D <- matsbyname::Matrix(42, nrow = 1, ncol = 1, dimnames = list("r", "c")) %>% 
    setrowtype("wrongrow") %>% setcoltype("col")
  expect_error(matsbyname:::check_row_col_types(C, D), "Incompatible row types: row and wrongrow")
  E <- matsbyname::Matrix(42, nrow = 1, ncol = 1, dimnames = list("r", "c")) %>% 
    setrowtype("row") %>% setcoltype("wrongcol")
  expect_error(matsbyname:::check_row_col_types(C, E), "Incompatible column types: col and wrongcol")
})
