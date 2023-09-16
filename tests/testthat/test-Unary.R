


test_that("abs_byname() works as expected", {
  expect_equal(abs_byname(1), 1)
  expect_equal(abs_byname(-1), 1)
  m <- matrix(c(-10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
    setrowtype("Industry") %>% setcoltype("Commodity")
  expect_equal(abs_byname(m), abs(m))
  
  # Test with a Matrix object
  M <- matsbyname::Matrix(c(-10,1,1,100), nrow = 2, ncol = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2)), 
                          rowtype = "Industry", coltype = "Commodity")
  expect_equal(abs_byname(M), abs(M))
})


test_that("log_byname() works as expected", {
  expect_equal(log_byname(exp(1)), 1)
  m <- matrix(c(10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:2))) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  
  expect_equal(log_byname(m), 
               matrix(c(2.302585, 0, 
                        0, 4.60517), byrow = TRUE, nrow = 2, ncol = 2,
                      dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
                 setrowtype("Industry") %>% setcoltype("Product"), 
               tolerance = 1e-7)
  expected_log10 <- matrix(c(1, 0, 
                             0, 2), byrow = TRUE, nrow = 2, ncol = 2,
                           dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(log_byname(m, base = 10), expected_log10)
  # Also works with lists
  expect_equal(log_byname(list(m, m), base = 10), list(expected_log10, expected_log10))
})


test_that("log_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(10,1,1,100), nrow = 2, ncol = 2,
                          dimnames = list(paste0("i", 1:2), paste0("p", 1:2)), 
                          rowtype = "Industry", coltype = "Product")
  
  matsbyname:::expect_equal_matrix_or_Matrix(log_byname(M), 
                                             matsbyname::Matrix(c(2.302585, 0, 
                                                                  0, 4.60517), byrow = TRUE, nrow = 2, ncol = 2,
                                                                dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
                                               setrowtype("Industry") %>% setcoltype("Product"), 
                                             tolerance = 1e-6)
  expected_log10 <- matsbyname::Matrix(c(1, 0, 
                                         0, 2), byrow = TRUE, nrow = 2, ncol = 2,
                                       dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  matsbyname:::expect_equal_matrix_or_Matrix(log_byname(M, base = 10), expected_log10)
})


test_that("exp_byname() works as expected", {
  expect_equal(exp_byname(1), exp(1))
  m <- matrix(c(log(10), log(1),
                log(1),  log(100)), 
              byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(paste0("i", 1:2), paste0("p", 1:2))) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  expected <- matrix(c(10, 1,
                        1, 100), byrow = TRUE, nrow = 2, ncol = 2, 
                     dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(exp_byname(m), expected)
  # Also works for lists.
  expect_equal(exp_byname(list(m, m)), list(expected, expected))
})


test_that("exp_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(log(10), log(1),
                        log(1),  log(100), 
                        log(2),  log(42)), 
                      byrow = TRUE, nrow = 3, ncol = 2, 
                      dimnames = list(paste0("i", 1:3), paste0("p", 1:2)), 
                      rowtype = "Industry", coltype = "Product")
  expectedM <- matrix(c(10, 1,
                        1, 100, 
                        2, 42), 
                      byrow = TRUE, nrow = 3, ncol = 2, 
                      dimnames = list(c("i1", "i2", "i3"), c("p1", "p2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  matsbyname:::expect_equal_matrix_or_Matrix(exp_byname(M), expectedM, tolerance = 1e-12)
  
  
  # rownames(M) are same as colnames(M). Why?  Because it is a symmetric matrix!
  # This looks to be a bug in the Matrix package.
  M2 <- matsbyname::Matrix(c(log(10), log(1),
                         log(1),  log(100)), 
                       byrow = TRUE, nrow = 2, ncol = 2, 
                       dimnames = list(paste0("i", 1:2), paste0("p", 1:2)), 
                       rowtype = "Industry", coltype = "Product")
    
  expectedM2 <- matrix(c(10, 1,
                         1, 100), 
                       byrow = TRUE, nrow = 2, ncol = 2, 
                       dimnames = list(c("i1", "i2"), c("p1", "p2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  res2 <- exp_byname(M2)
  matsbyname:::expect_equal_matrix_or_Matrix(res2, expectedM2, tolerance = 1e-12)
})
  
  
test_that("invert_byname() works as expected", {
  # Singular matrix
  sing <- matrix(c(1, 0,
                   42, 0), nrow = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_error(invert_byname(sing), regexp = "Attempt to invert a singular matrix. Zero rows and columns: c2.")
  # Not a matrix
  expect_error(invert_byname("not a matrix"), "'a' must be a numeric matrix")
  
  m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  # For matrix inversion, rows become columns and columns become rows.
  # Furthermore, the types on rows and columns are flipped.
  minv <- matrix(c(0.1, 0, 0, 0.01), nrow = 2, dimnames = list(colnames(m), rownames(m))) %>% 
    setrowtype(coltype(m)) %>% setcoltype(rowtype(m))
  expect_equal(invert_byname(m), minv)
  expect_equal(matrixproduct_byname(m, invert_byname(m)), 
               matrix(c(1,0,0,1), nrow = 2, dimnames = list(rownames(m), rownames(m))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(rowtype(m)))
  expect_equal(matrixproduct_byname(invert_byname(m), m), 
               matrix(c(1,0,0,1), nrow = 2, dimnames = list(colnames(m), colnames(m))) %>% 
                 setrowtype(coltype(m)) %>% setcoltype(coltype(m)))
  # Also works for lists
  expect_equal(invert_byname(list(m,m)), list(minv, minv))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1, "m"]] <- m
  DF[[2, "m"]] <- m
  expect_equal(invert_byname(DF$m), list(minv, minv))
  DF_expected <- data.frame(m = I(list()), minv = I(list()))
  DF_expected[[1, "m"]] <- m
  DF_expected[[2, "m"]] <- m
  DF_expected[[1, "minv"]] <- minv
  DF_expected[[2, "minv"]] <- minv
  # Because DF_expected$minv is created with I(list()), its class is "AsIs".
  # Because DF$minv is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$minv to NULL to get a match.
  attr(DF_expected$minv, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(minv = invert_byname(m)), DF_expected)
})


test_that("invert_byname() works correctly with a tol argument", {
  m <- matrix(c(-3, 4, 
                2, 5), byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(paste0("i", 1:2), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  minv <- matrix(c(-5/23, 4/23, 
                    2/23, 3/23), byrow = TRUE, nrow = 2, ncol = 2,  dimnames = list(colnames(m), rownames(m))) %>% 
    setrowtype(coltype(m)) %>% setcoltype(rowtype(m))
  # Make sure the tol argument is, ahem, tolerated.
  expect_equal(invert_byname(m), minv) # Default value of tol
  expect_equal(invert_byname(m, tol = 1e-7), minv)
  expect_equal(invert_byname(m, tol = 1e-10), minv)
  expect_equal(invert_byname(m, tol = 1e-16), minv)
})


test_that("invert_byname() works correctly with the method argument", {
  m <- matrix(c(4, -2, 1, 
                5, 0, 3, 
                -1, 2, 6), byrow = TRUE, nrow = 3, ncol = 3,
              dimnames = list(paste0("i", 1:3), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  minv <- matrix(c( -3/26,  7/26, -3/26, 
                   -33/52, 25/52, -7/52, 
                     5/26, -3/26,  5/26), byrow = TRUE, nrow = 3, ncol = 3, 
                 dimnames = list(colnames(m), rownames(m))) %>% 
    setrowtype(coltype(m)) %>% setcoltype(rowtype(m))

  expect_error(invert_byname(m, method = "bogus"), regexp = "'arg' should be one of ")
  expect_equal(invert_byname(m), minv)
  expect_equal(invert_byname(m, method = "QR"), minv)
  expect_equal(invert_byname(m, method = "SVD"), minv)
})


test_that("invert_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(4, -2, 1, 
                            5, 0, 3, 
                            -1, 2, 6), byrow = TRUE, nrow = 3, ncol = 3,
                          dimnames = list(paste0("i", 1:3), paste0("p", 1:3)), 
                          rowtype = "Industries", coltype = "Products")
  Minv <- matrix(c( -3/26,  7/26, -3/26, 
                    -33/52, 25/52, -7/52, 
                    5/26, -3/26,  5/26), byrow = TRUE, nrow = 3, ncol = 3, 
                 dimnames = list(colnames(M), rownames(M))) %>% 
    setrowtype(coltype(M)) %>% setcoltype(rowtype(M))
  
  expect_error(invert_byname(M, method = "bogus"), regexp = "'arg' should be one of ")
  matsbyname:::expect_equal_matrix_or_Matrix(invert_byname(M), Minv, tolerance = 1e-15)
  expect_true(is.Matrix(invert_byname(M, method = "solve")))
  expect_false(is.matrix(invert_byname(M, method = "solve")))
  
  resQR <- invert_byname(M, method = "QR")
  matsbyname:::expect_equal_matrix_or_Matrix(resQR, Minv, tolerance = 1e-15)
  expect_true(is.Matrix(resQR))
  
  res_SVD <- invert_byname(M, method = "SVD")
  matsbyname:::expect_equal_matrix_or_Matrix(res_SVD, Minv, tolerance = 1e-15)
  expect_true(is.Matrix(res_SVD))
})


test_that("transpose_byname() works as expected", {
  m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  mT <- matrix(c(11, 12, 21, 22, 31, 32), nrow = 2, dimnames = list(paste0("p", 1:2), paste0("i", 1:3))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(transpose_byname(m), mT)
  # Works for lists
  expect_equal(transpose_byname(list(m,m)), list(mT, mT))
  # Works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(transpose_byname(DF$m), list(mT, mT))
  DF_expected <- data.frame(m = I(list()), mT = I(list()))
  DF_expected[[1, "m"]] <- m
  DF_expected[[2, "m"]] <- m
  DF_expected[[1, "mT"]] <- mT
  DF_expected[[2, "mT"]] <- mT
  # Because DF_expected$mT is created with I(list()), its class is "AsIs".
  # Because DF$mT is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$mT to NULL to get a match.
  attr(DF_expected$mT, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(mT = transpose_byname(m)), DF_expected)
})


test_that("transpose_byname() works with lists of lists", {
  m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  mT <- matrix(c(11, 12, 21, 22, 31, 32), nrow = 2, dimnames = list(paste0("p", 1:2), paste0("i", 1:3))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  
  # Here, we put a list of matrices into subsequent rows of a column of a data frame.
  # The unaryapply_byname function should recursively 
  # work its way down to the point where it finds matrices upon which it operates.
  listofm <- list(a = m, b = m)
  DF <- data.frame(listofm = I(list()))
  DF[[1,"listofm"]] <- listofm
  DF[[2,"listofm"]] <- listofm
  
  res <- DF %>% 
    dplyr::mutate(
      listofmT = transpose_byname(listofm)
    )
  expect_equal(res$listofmT[[1]][[1]], mT)
  expect_equal(res$listofmT[[1]][[2]], mT)
  expect_equal(res$listofmT[[2]][[1]], mT)
  expect_equal(res$listofmT[[2]][[2]], mT)
  
  # Ensure that names of the list are preserved
  expect_equal(names(res$listofmT[[1]]), c("a", "b"))
  expect_equal(names(res$listofmT[[2]]), c("a", "b"))
})


test_that("transpose_byname() correctly handles constants", {
  expect_equal(transpose_byname(0), 0)
  expect_true(!is.matrix(transpose_byname(0)))
  expect_equal(transpose_byname(42), 42)
  expect_equal(transpose_byname(-10), -10)
})


test_that("transpose_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(11,21,31,12,22,32), nrow = 3, ncol = 2, dimnames = list(paste0("i", 1:3), paste0("p", 1:2)), 
                          rowtype = "Industries", coltype = "Products")
  MT <- matsbyname::Matrix(c(11, 12, 21, 22, 31, 32), nrow = 2, ncol = 3, dimnames = list(paste0("p", 1:2), paste0("i", 1:3)), 
                           rowtype = "Products", coltype = "Industries")
  expect_equal(transpose_byname(M), MT)
})


test_that("eigenvalues_byname() works as expected", {
  m <- matrix(c(4, 6, 10, 
                3, 10 , 13, 
                -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
              dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  expected <- c(4, 2, 0)
  expect_equal(eigenvalues_byname(m), expected)
  
  expect_equal(eigenvalues_byname(list(m, 2*m)), list(expected, 2*expected)) 
  
  
  DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
    dplyr::mutate(
      eigen_col = eigenvalues_byname(m_col)
    )
  expect_equal(DF$eigen_col[[1]], expected)
  expect_equal(DF$eigen_col[[2]], 2*expected)
})


test_that("eigenvalues_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(4, 6, 10, 
                            3, 10 , 13, 
                            -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
                          dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  expected <- c(4, 2, 0)
  expect_equal(eigenvalues_byname(M), expected)
})


test_that("eigenvectors_byname() works as expected", {
  m <- matrix(c(4, 6, 10, 
                3, 10 , 13, 
                -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
              dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  expected <- matrix(c(0.457495711, 0.408248290, -0.577350269,
                       0.762492852, -0.816496581, -0.577350269,
                       -0.457495711, 0.408248290, 0.577350269), byrow = TRUE, nrow = 3, ncol = 3)
  expect_equal(eigenvectors_byname(m), expected)
  
  expect_equal(eigenvectors_byname(list(m, 2*m)), list(expected, expected)) 
  
  
  DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
    dplyr::mutate(
      eigen_col = eigenvectors_byname(m_col)
    )
  expect_equal(DF$eigen_col[[1]], expected)
  expect_equal(DF$eigen_col[[2]], expected)
})


test_that("eigenvectors_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(4, 6, 10, 
                            3, 10 , 13, 
                            -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
                          dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  expected <- matrix(c(0.457495711, 0.408248290, -0.577350269,
                       0.762492852, -0.816496581, -0.577350269,
                       -0.457495711, 0.408248290, 0.577350269), byrow = TRUE, nrow = 3, ncol = 3)
  expect_equal(eigenvectors_byname(M), expected)
  
  expect_equal(eigenvectors_byname(list(M, 2*M)), list(expected, expected)) 
})


test_that("svd_byname() works as expected", {
  # Example from https://medium.com/intuition/singular-value-decomposition-svd-working-example-c2b6135673b5
  test_func <- function(A_mat) {
    D <- svd_byname(A_mat)
    expected_D <- diag(c(sqrt(40), sqrt(10)))
    rownames(expected_D) <- rownames(A_mat)
    colnames(expected_D) <- colnames(A_mat)
    expected_D <- expected_D %>% 
      setrowtype(rowtype(A_mat)) %>% 
      setcoltype(coltype(A_mat))
    expect_equal(D, expected_D)
    
    U <- svd_byname(A_mat, which = "u")
    expected_U <- matrix(c(-1/sqrt(5), -2/sqrt(5), 
                           -2/sqrt(5),  1/sqrt(5)), byrow = TRUE, nrow = 2, ncol = 2)
    rownames(expected_U) <- rownames(A_mat)
    colnames(expected_U) <- rownames(A_mat)
    expected_U <- expected_U %>% 
      setrowtype(rowtype(A_mat)) %>% 
      setcoltype(rowtype(A_mat))
    expect_equal(U, expected_U)
    
    V <- svd_byname(A_mat, which = "v")
    expected_V <- matrix(c(-1/sqrt(2), -1/sqrt(2), 
                           1/sqrt(2), -1/sqrt(2)), byrow = TRUE, nrow = 2, ncol = 2)
    rownames(expected_V) <- colnames(A_mat)
    colnames(expected_V) <- colnames(A_mat)
    expected_V <- expected_V %>% 
      setrowtype(coltype(A_mat)) %>% 
      setcoltype(coltype(A_mat))
    expect_equal(V, expected_V)
    
    # Double-check multiplication
    should_be_A <- matrixproduct_byname(U, D) %>% 
      matrixproduct_byname(transpose_byname(V))
    matsbyname:::expect_equal_matrix_or_Matrix(should_be_A, A_mat, tolerance = 1e-14)
  }
  
  # Try with a matrix object
  A <- matrix(c(4, 0, 
               3, -5), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  test_func(A)
  # Try with a Matrix object
  AM <- matsbyname::Matrix(c(4, 0, 
                             3, -5), nrow = 2, ncol = 2, byrow = TRUE, 
                           dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                           rowtype = "rowtype", coltype = "coltype")
  # Does not work at present. 
  # Need to figure out how to do an SVD on a Matrix object
  test_func(AM)
})


test_that("hatize_byname() works as expected", {
  g <- matrix(4, dimnames = list("I", "Products"))
  expect_error(hatize_byname(g), "Unable to determine which names to keep \\(rows or cols\\) in hatize_byname\\(\\). Try setting the 'keep' argument.")
  expect_equal(hatize_byname(g, keep = "rownames"), 
               matrix(4, dimnames = list("I", "I")))
  expect_equal(hatize_byname(g, keep = "colnames"), 
               matrix(4, dimnames = list("Products", "Products")))
  
  v <- matrix(1:3, ncol = 1, dimnames = list(c(paste0("i", 1:3)), c("p1"))) %>%
    setrowtype("Industries") %>% setcoltype(NA)
  # Try to hatize with the wrong keep argument
  expect_error(hatize_byname(v, keep = "colnames"), 'In hatize_byname\\(\\), argument "keep" set to "colnames", but you supplied a column vector. Consider setting keep = "rownames".')
  expect_error(hatize_byname(matrix(v, nrow = 1), keep = "rownames"), 'In hatize_byname\\(\\), argument "keep" set to "rownames", but you supplied a row vector. Consider setting keep = "colnames".')
  # Try to hatize a list.
  v_list <- list(v, v)
  expected_m <- matrix(c(1, 0, 0, 
                         0, 2, 0,
                         0, 0, 3), nrow = 3, byrow = TRUE, 
                       dimnames = list(c("i1", "i2", "i3"), c("i1", "i2", "i3"))) %>% 
    setrowtype("Industries") %>% setcoltype("Industries")
  expect_equal(hatize_byname(v_list, keep = "rownames"), list(expected_m, expected_m))
})


test_that("hatize_byname() works as expected", {
  # Check the absurd situation where a non-vector is sent to hatize()
  supposed_to_be_a_vector <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_error(hatize_byname(supposed_to_be_a_vector, keep = "rownames"), 
               'In hatize_byname\\(\\), matrix v must have at least 1 dimension of length 1.')
  v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("p1"))) %>%
    setrowtype("Industries") %>% setcoltype(NA)
  orderedRowNames <- c("i1", "i10", paste0("i", 2:9))
  v_hat_expected <- matrix(c(1,0,0,0,0,0,0,0,0,0,
                             0,10,0,0,0,0,0,0,0,0,
                             0,0,2,0,0,0,0,0,0,0,
                             0,0,0,3,0,0,0,0,0,0,
                             0,0,0,0,4,0,0,0,0,0,
                             0,0,0,0,0,5,0,0,0,0,
                             0,0,0,0,0,0,6,0,0,0,
                             0,0,0,0,0,0,0,7,0,0,
                             0,0,0,0,0,0,0,0,8,0,
                             0,0,0,0,0,0,0,0,0,9),
                           nrow = 10, 
                           dimnames = list(orderedRowNames, orderedRowNames)) %>% 
    setrowtype(rowtype(v)) %>% setcoltype(rowtype(v))
  r <- matrix(1:5, nrow = 1, dimnames = list("i1", paste0("p", 1:5))) %>%
    setrowtype(NA) %>% setcoltype("Commodities")
  orderedColNames <- paste0("p", 1:5)
  r_hat_expected <- matrix(c(1,0,0,0,0,
                             0,2,0,0,0,
                             0,0,3,0,0,
                             0,0,0,4,0,
                             0,0,0,0,5),
                           nrow = 5, 
                           dimnames = list(orderedColNames, orderedColNames)) %>% 
    setrowtype(coltype(r)) %>% setcoltype(coltype(r))
  expect_equal(hatize_byname(r, keep = "colnames"), r_hat_expected)
  # This also works with lists.
  expect_equal(hatize_byname(list(v, v), keep = "rownames"), list(v_hat_expected, v_hat_expected))
  # And it works with data frames.
  DF <- data.frame(v = I(list()))
  DF[[1,"v"]] <- v
  DF[[2,"v"]] <- v
  expect_equal(hatize_byname(DF$v, keep = "rownames"), list(v_hat_expected, v_hat_expected))
  DF_expected <- data.frame(v = I(list()), v_hat = I(list()))
  DF_expected[[1,"v"]] <- v
  DF_expected[[2,"v"]] <- v
  DF_expected[[1,"v_hat"]] <- v_hat_expected
  DF_expected[[2,"v_hat"]] <- v_hat_expected
  # Because DF_expected$v_hat is created with I(list()), its class is "AsIs".
  # Because DF$v_hat is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$v_hat to NULL to get a match.
  attr(DF_expected$v_hat, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(v_hat = hatize_byname(v, keep = "rownames")), DF_expected)
})


test_that("hatize_byname() works with a simple vector", {
  # I'm running into a bug where hatize doesn't work on a 1x1 vector that lacks a column name
  # Verify that a 2x1 vector works correctly.
  v1 <- matrix(c(1, 
                 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"))) %>% 
    setrowtype("Product -> Industry")
  v1_hat <- hatize_byname(v1, keep = "rownames")
  v1_hat_expected <- matrix(c(1, 0,
                              0, 2), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("r1", "r2"))) %>% 
    setrowtype("Product -> Industry") %>% 
    setcoltype("Product -> Industry")
  expect_equal(v1_hat, v1_hat_expected)
  
  # Now try with a 1x1 column vector
  v2 <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1")) %>% 
    setrowtype("Product -> Industry")
  v2_hat <- hatize_byname(v2, keep = "rownames")
  v2_hat_expected <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "r1")) %>% 
    setrowtype("Product -> Industry") %>% 
    setcoltype("Product -> Industry")
  expect_equal(v2_hat, v2_hat_expected)
  
  # Try with a 1x1 row vector
  v3 <- matrix(42, nrow = 1, ncol = 1, dimnames = list(NULL, "c1")) %>% 
    setcoltype("Industry -> Product")
  v3_hat <- hatize_byname(v3, keep = "colnames")
  v3_hat_expected <- matrix(42, nrow = 1, ncol = 1, dimnames = list("c1", "c1")) %>% 
    setrowtype("Industry -> Product") %>% 
    setcoltype("Industry -> Product")
  expect_equal(v3_hat, v3_hat_expected)
  
  # Try with 1x1 vector with both dimensions named.
  # This should fail, because rownames or colnames must be specified in the keep argument.
  v4 <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
    setrowtype("Product -> Industry") %>% 
    setcoltype("Industry -> Product")
  expect_error(hatize_byname(v4), "Unable to determine which names to keep \\(rows or cols\\) in hatize_byname\\(\\). Try setting the 'keep' argument.")
  expect_equal(hatize_byname(v4, keep = "rownames"), matrix(42, dimnames = list("r1", "r1")) %>% 
                 setrowtype("Product -> Industry") %>% 
                 setcoltype("Product -> Industry"))
  expect_equal(hatize_byname(v4, keep = "colnames"), matrix(42, dimnames = list("c1", "c1")) %>% 
                 setrowtype("Industry -> Product") %>% 
                 setcoltype("Industry -> Product"))
})


test_that("hatize_byname() issues a warning when keep is wrong", {
  v <- matrix(c(1, 2), nrow = 2, dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(hatize_byname(v, keep = "rownames"), matrix(c(1, 0, 
                                                             0, 2), nrow = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("r1", "r2"))))
  expect_equal(hatize_byname(v), matrix(c(1, 0, 
                                          0, 2), nrow = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("r1", "r2"))))
  expect_error(hatize_byname(v, keep = "bogus"), 'In hatize_byname\\(\\), argument "keep" must be one of "colnames" or "rownames".')
  
  r <- matrix(c(1, 2), ncol = 2, dimnames = list("r1", c("c1", "c2")))   
  expect_equal(hatize_byname(r, keep = "colnames"), matrix(c(1, 0, 
                                                             0, 2), nrow = 2, byrow = TRUE, dimnames = list(c("c1", "c2"), c("c1", "c2"))))
  expect_error(hatize_byname(r, keep = "bogus"), 'In hatize_byname\\(\\), argument "keep" must be one of "colnames" or "rownames".')
})


test_that("hatize_byname() works with a Matrix vector", {
  v1 <- matsbyname::Matrix(c(1, 
                             2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), c("c1")), 
                           rowtype = "Product -> Industry")
  v1_hat <- hatize_byname(v1, keep = "rownames")
  v1_hat_expected <- matrix(c(1, 0,
                              0, 2), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("r1", "r2"))) %>% 
    setrowtype("Product -> Industry") %>% 
    setcoltype("Product -> Industry")
  matsbyname:::expect_equal_matrix_or_Matrix(v1_hat, v1_hat_expected)
  expect_true(inherits(v1_hat, "ddiMatrix"))
  
  v2 <- matsbyname::Matrix(1:10, nrow = 10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("p1")), 
                           rowtype = "Industries", coltype = NA)
  orderedRowNames <- c("i1", "i10", paste0("i", 2:9))
  v2_hat_expected <- matsbyname::Matrix(c(1,0,0,0,0,0,0,0,0,0,
                                          0,10,0,0,0,0,0,0,0,0,
                                          0,0,2,0,0,0,0,0,0,0,
                                          0,0,0,3,0,0,0,0,0,0,
                                          0,0,0,0,4,0,0,0,0,0,
                                          0,0,0,0,0,5,0,0,0,0,
                                          0,0,0,0,0,0,6,0,0,0,
                                          0,0,0,0,0,0,0,7,0,0,
                                          0,0,0,0,0,0,0,0,8,0,
                                          0,0,0,0,0,0,0,0,0,9),
                                        nrow = 10, ncol = 10,
                                        dimnames = list(orderedRowNames, orderedRowNames), 
                                        rowtype = "Industries", coltype = "Industries")
  res2 <- hatize_byname(v2)
  matsbyname:::expect_equal_matrix_or_Matrix(res2, v2_hat_expected)
  
  
  r <- matsbyname::Matrix(1:5, nrow = 1, ncol = 5, dimnames = list("i1", paste0("p", 1:5)), 
                          rowtype = NA, coltype = "Industries")
  orderedColNames <- paste0("p", 1:5)
  r_hat_expected <- matsbyname::Matrix(c(1,0,0,0,0,
                                         0,2,0,0,0,
                                         0,0,3,0,0,
                                         0,0,0,4,0,
                                         0,0,0,0,5),
                                       nrow = 5, ncol = 5,
                                       dimnames = list(orderedColNames, orderedColNames), 
                                       rowtype = coltype(r), coltype = coltype(r))
  matsbyname:::expect_equal_matrix_or_Matrix(hatize_byname(r, keep = "colnames"), r_hat_expected)
})


test_that("hatize_byname() tests hit all lines of code", {
  v1 <- matrix(c(42), nrow = 1, ncol = 1)
  expect_error(hatize_byname(v1), "Unable to determine which names to keep \\(rows or cols\\) in hatize_byname\\(\\). Try setting the 'keep' argument.")
  
  v2 <- matrix(c(42), nrow = 1, ncol = 1, dimnames = list("r1"))
  expect_error(hatize_byname(v2, keep = "bogus"), 'In hatize_byname\\(\\), argument "keep" must be one of "colnames" or "rownames".')
  
  expect_error(hatize_byname(NA), "argument is of length zero")
  expect_null(hatize_byname(NULL))
})


test_that("hatinv_byname() works as expected", {
  # Test with a column vector
  v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("p1"))) %>%
    setrowtype("Industries") %>% setcoltype(NA)
  res <- hatinv_byname(v, keep = "rownames")
  expected <- v %>% 
    hatize_byname(keep = "rownames") %>% 
    invert_byname()
  expect_equal(res, expected)
  # Test with a row vector
  r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
    setrowtype(NA) %>% setcoltype("Commodities")
  expect_equal(hatinv_byname(r, keep = "colnames"), r %>% hatize_byname(keep = "colnames") %>% invert_byname())
  # Test with a list
  v_list <- list(v, v)
  expect_equal(hatinv_byname(v_list, keep = "rownames"), v_list %>% hatize_byname(keep = "rownames") %>% invert_byname())
  # Test with a data frame
  DF <- data.frame(v_list = I(list()))
  DF[[1, "v_list"]] <- v
  DF[[2, "v_list"]] <- v
  DF <- DF %>% 
    dplyr::mutate(
      hatinv = hatinv_byname(v_list, keep = "rownames")
    )
  DF_expected <- data.frame(v_list = I(list()), hatinv = I(list()))
  DF_expected[[1, "v_list"]] <- v
  DF_expected[[2, "v_list"]] <- v
  DF_expected[[1, "hatinv"]] <- v %>% hatize_byname(keep = "rownames") %>% invert_byname()
  DF_expected[[2, "hatinv"]] <- v %>% hatize_byname(keep = "rownames") %>% invert_byname()
  # The hatinv column of DF_expected will have matrix_class = 'AsIs', but
  # the hatinv column of DF will have no class attribute.  
  # Eliminate that mismatch.
  attr(DF_expected$hatinv, which = "class") <- NULL
  expect_equal(DF, DF_expected)
  # Test when one of the elements of v is 0.
  v2 <- matrix(0:1, ncol = 1, dimnames = list(c(paste0("i", 0:1)), c("p1"))) %>%
    setrowtype("Industries") %>% setcoltype(NA)
  expect_equal(hatinv_byname(v2, keep = "rownames"), matrix(c(.Machine$double.xmax, 0,
                                                              0, 1), 
                                                            nrow = 2, ncol = 2, byrow = TRUE,
                                                            dimnames = list(c(paste0("i", 0:1)), c(paste0("i", 0:1)))) %>%  
                 setrowtype("Industries") %>% setcoltype("Industries"))
  # Test when we want the 0 element of v to give Inf instead of .Machine$double.xmax.
  expect_equal(hatinv_byname(v2, inf_becomes = NULL, keep = "rownames"), matrix(c(Inf, 0,
                                                                                  0, 1), 
                                                                                nrow = 2, ncol = 2, byrow = TRUE,
                                                                                dimnames = list(c(paste0("i", 0:1)), c(paste0("i", 0:1)))) %>%  
                 setrowtype("Industries") %>% setcoltype("Industries"))
  
  # Test that hatinv works with a 1x1 vector
  g <- matrix(4, dimnames = list("I", "Products"))
  expect_error(hatinv_byname(g), "Unable to determine which names to keep \\(rows or cols\\) in hatize_byname\\(\\). Try setting the 'keep' argument.")
})


test_that("hatinv_byname() works with a Matrix object", {
  # Test with a column vector
  v <- matsbyname::Matrix(1:10, nrow = 10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("p1")), 
                          rowtype = "Industries", coltype = NA) 
  res <- hatinv_byname(v, keep = "rownames")
  expected <- v %>% 
    hatize_byname(keep = "rownames") %>%
    invert_byname()
  expect_equal(res, expected)
  
  # Test with a row vector
  r <- matsbyname::Matrix(1:5, nrow = 1, ncol = 5, dimnames = list(c("r1"), c(paste0("c", 1:5))), 
                          rowtype = NA, coltype = "Commodities")
  expect_equal(hatinv_byname(r, keep = "colnames"), 
               r %>% 
                 hatize_byname(keep = "colnames") %>%
                 invert_byname())
  
  # Test that hatinv works with a 1x1 Matrix
  g <- matsbyname::Matrix(4, dimnames = list("I", "Products"))
  expect_error(hatinv_byname(g), "Unable to determine which names to keep \\(rows or cols\\) in hatize_byname\\(\\). Try setting the 'keep' argument.")
})


test_that("identize_byname() works as expected", {
  # Try first with a single number
  expect_equal(identize_byname(42), 1)
  # Now try with matrices.
  m <- matrix(1:16, ncol = 4, dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  mI_expected <- matrix(c(1,0,0,0,
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1),
                        nrow = 4, 
                        dimnames = dimnames(m)) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  # Test for errors
  expect_error(identize_byname(m, margin = c(1,2,3,4)), "margin should have length 1 or 2 in identize_byname")
  expect_error(identize_byname(m, margin = c(3)), "Unknown margin 3 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m, margin = c(-1)), "Unknown margin -1 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m, margin = c(1,1,2,2)), "margin should have length 1 or 2 in identize_byname")
  
  # Test for column vector
  expect_equal(identize_byname(m, margin = 1), 
               matrix(1, nrow = nrow(m), ncol = 1) %>% 
                 setrownames_byname(rownames(m)) %>% setcolnames_byname(coltype(m)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  
  # Test for row vector
  expect_equal(identize_byname(m, margin = 2), 
               matrix(1, nrow = 1, ncol = ncol(m)) %>% 
                 setrownames_byname(rowtype(m)) %>% setcolnames_byname(colnames(m)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m))) 
  
  # Test for identity matrix
  expect_equal(identize_byname(m), mI_expected)
  expect_equal(identize_byname(m, margin = c(1,2)), mI_expected)
  expect_equal(identize_byname(m, margin = c(2,1)), mI_expected)
  
  # This also works with lists
  expect_equal(identize_byname(list(m, m, m)), list(mI_expected, mI_expected, mI_expected))
  # This also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(identize_byname(DF$m, margin = list(c(1, 2))), list(mI_expected, mI_expected))
  expect_equal(identize_byname(DF$m, margin = list(c(1,2))), list(mI_expected, mI_expected))
  expect_equal(identize_byname(DF$m, margin = list(c(2,1))), list(mI_expected, mI_expected))
  DF_expected <- data.frame(m = I(list()), mI = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"mI"]] <- mI_expected
  DF_expected[[2,"mI"]] <- mI_expected
  # Because DF_expected$mI is created with I(list()), its class is "AsIs".
  # Because DF$mI is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$mI to NULL to get a match.
  attr(DF_expected$mI, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(mI = identize_byname(m, margin = list(c(1, 2)))), DF_expected)
})


test_that("identize_byname() works with a Matrix object", {
  mI_expected <- matrix(c(1,0,0,0,
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1),
                        nrow = 4, 
                        dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>% 
    setrowtype("Industries") %>% setcoltype("Products")
  # Try with matrices.
  m <- matsbyname::Matrix(42, nrow = 4, ncol = 4, 
                          dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4)), 
                          rowtype = "Industries", coltype = "Products")
  res <- identize_byname(m)
  matsbyname:::expect_equal_matrix_or_Matrix(res, mI_expected)
    
  m1 <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4)), 
                           rowtype = "Industries", coltype = "Products")
  mI_expected <- matrix(c(1,0,0,0,
                          0,1,0,0,
                          0,0,1,0,
                          0,0,0,1),
                        nrow = 4,
                        dimnames = dimnames(m1)) %>%
    setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1))
  # Test for errors
  expect_error(identize_byname(m1, margin = c(1,2,3,4)), "margin should have length 1 or 2 in identize_byname")
  expect_error(identize_byname(m1, margin = c(3)), "Unknown margin 3 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m1, margin = c(-1)), "Unknown margin -1 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m1, margin = c(1,1,2,2)), "margin should have length 1 or 2 in identize_byname")

  # Test for column vector
  res <- identize_byname(m1, margin = 1)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res,
                                             matrix(1, nrow = nrow(m1), ncol = 1) %>%
                                               setrownames_byname(rownames(m1)) %>% setcolnames_byname(coltype(m1)) %>%
                                               setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1)))

  # Test for row vector
  matsbyname:::expect_equal_matrix_or_Matrix(identize_byname(m1, margin = 2),
                                             matrix(1, nrow = 1, ncol = ncol(m1)) %>%
                                               setrownames_byname(rowtype(m1)) %>% setcolnames_byname(colnames(m1)) %>%
                                               setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1)))

  # Test for identity matrix
  matsbyname:::expect_equal_matrix_or_Matrix(identize_byname(m1), mI_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(identize_byname(m1, margin = c(1,2)), mI_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(identize_byname(m1, margin = c(2,1)), mI_expected)
})


test_that("vectorize_byname() works as expected", {
  # Try with a square matrix
  m1 <- matrix(c(1, 5,
                 4, 5),
              nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expected1 <- matrix(c(1, 
                        4, 
                        5, 
                        5),
                      nrow = 4, ncol = 1, 
                      dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"))) %>% 
    setrowtype("Products -> Industries") %>% setcoltype(NULL)
  actual1 <- vectorize_byname(m1, notation = RCLabels::arrow_notation)
  expect_equal(actual1, expected1)
  # Try with null notation
  expect_equal(vectorize_byname(m1, notation = NULL), m1)
  # Try with a rectangular matrix
  m2 <- matrix(c(1, 2, 3,
                 4, 5, 6),
               nrow = 2, ncol = 3, byrow = TRUE,
               dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expected2 <- matrix(c(1, 
                        4, 
                        2, 
                        5, 
                        3, 
                        6),
                      nrow = 6, ncol = 1,
                      dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2", "p1 -> i3", "p2 -> i3"))) %>% 
    setrowtype("Products -> Industries") %>% setcoltype(NULL)
  actual2 <- vectorize_byname(m2, notation = RCLabels::arrow_notation)
  expect_equal(actual2, expected2)
  # Try with a single number
  m3 <- 42
  expected3 <- m3
  dim(expected3) <- c(1, 1)
  dimnames(expected3) <- NULL
  actual3 <- vectorize_byname(m3, notation = RCLabels::arrow_notation)
  expect_equal(actual3, expected3)
  # Try with a different separator
  m4 <- matrix(c(1, 5,
                 4, 5),
               nrow = 2, ncol = 2, byrow = TRUE, 
               dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expected4 <- matrix(c(1, 
                        4, 
                        5, 
                        5),
                      nrow = 4, ncol = 1, 
                      dimnames = list(c("p1---i1", "p2---i1", "p1---i2", "p2---i2"))) %>% 
    setrowtype("Products---Industries") %>% setcoltype(NULL)
  actual4 <- vectorize_byname(m4, notation = RCLabels::notation_vec(sep = "---"))
  expect_equal(actual4, expected4)
  # Test with a matrix that is already a column vector
  m5 <- matrix(c(1,
                 2,
                 3),
               nrow = 3, ncol = 1,
               dimnames = list(c("p1", "p2", "p3"), "i1")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  actual5 <- vectorize_byname(m5, notation = RCLabels::notation_vec(sep = "***"))
  expected5 <- matrix(c(1,
                        2,
                        3),
                      nrow = 3, ncol = 1,
                      dimnames = list(c("p1***i1", "p2***i1", "p3***i1"))) %>% 
    setrowtype("Products***Industries") %>% setcoltype(NULL)
  expect_equal(actual5, expected5)
  # Test with NULL. Should get NULL back.
  expect_null(vectorize_byname(NULL, NULL))
  # Test with NA.
  expect_error(vectorize_byname(NA, notation = RCLabels::arrow_notation), "a is not numeric or a Matrix in vectorize_byname")
  # Test with string
  expect_error(vectorize_byname("a", notation = RCLabels::arrow_notation), "a is not numeric or a Matrix in vectorize_byname")
  # Test with a list of matrices
  list6 <- list(m1, m1)
  actual6 <- vectorize_byname(list6, notation = list(RCLabels::arrow_notation))
  expected6 <- list(expected1, expected1)
  expect_equal(actual6, expected6)
})


test_that("vectorize_byname() works with 4 matrices in a list", {
  m <- matrix(c(1, 5,
                4, 5),
              nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  l <- list(m, m, m, m)
  actual <- vectorize_byname(l, notation = list(RCLabels::arrow_notation))
  e <- matrix(c(1, 
                4, 
                5, 
                5),
              nrow = 4, ncol = 1, 
              dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"))) %>% 
    setrowtype("Products -> Industries") %>% setcoltype(NULL)
  expect_equal(actual, list(e, e, e, e))
})


test_that("vectorize_byname() works with Matrix objects", {
  # Try with a square Matrix
  m1 <- matsbyname::Matrix(c(1, 5,
                             4, 5),
                           nrow = 2, ncol = 2, byrow = TRUE, 
                           dimnames = list(c("p1", "p2"), c("i1", "i2")), 
                           rowtype = "Products", coltype = "Industries")
  expected1 <- matrix(c(1, 
                        4, 
                        5, 
                        5),
                      nrow = 4, ncol = 1, 
                      dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"))) %>% 
    setrowtype("Products -> Industries") %>% setcoltype(NULL)
  actual1 <- vectorize_byname(m1, notation = RCLabels::arrow_notation)
  matsbyname:::expect_equal_matrix_or_Matrix(actual1, expected1)
  
  # Try with a rectangular matrix
  m2 <- matsbyname::Matrix(c(1, 2, 3,
                             4, 5, 6),
                           nrow = 2, ncol = 3, byrow = TRUE,
                           dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")), 
                           rowtype = "Products", coltype = "Industries") 
  expected2 <- matrix(c(1, 
                        4, 
                        2, 
                        5, 
                        3, 
                        6),
                      nrow = 6, ncol = 1,
                      dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2", "p1 -> i3", "p2 -> i3"))) %>% 
    setrowtype("Products -> Industries") %>% setcoltype(NULL)
  actual2 <- vectorize_byname(m2, notation = RCLabels::arrow_notation)
  matsbyname:::expect_equal_matrix_or_Matrix(actual2, expected2)
  
  # Test with a Matrix that is already a column vector
  m5 <- matsbyname::Matrix(c(1,
                             2,
                             3),
                           nrow = 3, ncol = 1,
                           dimnames = list(c("p1", "p2", "p3"), "i1"), 
                           rowtype = "Products", coltype = "Industries")
  actual5 <- vectorize_byname(m5, notation = RCLabels::notation_vec(sep = "***"))
  expected5 <- matrix(c(1,
                        2,
                        3),
                      nrow = 3, ncol = 1,
                      dimnames = list(c("p1***i1", "p2***i1", "p3***i1"))) %>% 
    setrowtype("Products***Industries") %>% setcoltype(NULL)
  matsbyname:::expect_equal_matrix_or_Matrix(actual5, expected5)
})


test_that("matricize_byname() works as expected", {
  v1 <- array(dim = c(2, 2, 2))
  expect_error(matricize_byname(v1, notation = RCLabels::arrow_notation), "== 2 in matricize_byname")

  # Try with a column vector  
  v2 <- matrix(c(1,
                 2,
                 3, 
                 4), 
               nrow = 4, ncol = 1, dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"))) %>% 
    setrowtype("Products -> Industries")
  actual2 <- matricize_byname(v2, notation = RCLabels::arrow_notation)
  expected2 <- matrix(c(1, 3,
                        2, 4),
                      nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(actual2, expected2)
                        
  # Try with a row vector
  v3 <- matrix(c(1, 2, 3, 4), 
               nrow = 1, ncol = 4, dimnames = list(NULL, c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"))) %>% 
    setcoltype("Products -> Industries")
  actual3 <- matricize_byname(v3, notation = RCLabels::arrow_notation)
  expected3 <- matrix(c(1, 3,
                        2, 4),
                      nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(actual3, expected3)
  
  # Try with a 1x1 matrix as a column vector.
  v4 <- matrix(42, nrow = 1, ncol = 1, dimnames = list(c("p2 -> i1"))) %>% 
    setrowtype("Products -> Industries")
  actual4 <- matricize_byname(v4, notation = RCLabels::arrow_notation)
  expected4 <- matrix(42, nrow = 1, ncol = 1, dimnames = list("p2", "i1")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(actual4, expected4)
  
  # Try with a 1x1 matrix as a row vector.
  v5 <- matrix(42, nrow = 1, ncol = 1, dimnames = list(NULL, c("p2 -> i1"))) %>% 
    setcoltype("Products -> Industries")
  actual5 <- matricize_byname(v5, notation = RCLabels::arrow_notation)
  expected5 <- matrix(42, nrow = 1, ncol = 1, dimnames = list("p2", "i1")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(actual5, expected5)
  
  # Try with a non-square result
  v6 <- matrix(c(1, 2, 3, 4, 5, 6),
               nrow = 1, ncol = 6, dimnames = list(NULL, c("p1 -> i1", "p1 -> i2", 
                                                           "p2 -> i1", "p2 -> i2",
                                                           "p3 -> i1", "p3 -> i2"))) %>% 
    setcoltype("Products -> Industries")
  actual6 <- matricize_byname(v6, notation = RCLabels::arrow_notation)
  expected6 <- matrix(c(1, 2, 
                        3, 4, 
                        5, 6),
                      nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(actual6, expected6)
})


test_that("matricize_byname() works with Matrix objects", {
  # Try with a column vector that is a Matrix object
  v2 <- matsbyname::Matrix(c(1,
                             2,
                             3, 
                             4), 
                           nrow = 4, ncol = 1, dimnames = list(c("p1 -> i1", "p2 -> i1", "p1 -> i2", "p2 -> i2"), NULL), 
                           rowtype = "Products -> Industries")
  actual2 <- matricize_byname(v2, notation = RCLabels::arrow_notation)
  expected2 <- matrix(c(1, 3,
                        2, 4),
                      nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  matsbyname:::expect_equal_matrix_or_Matrix(actual2, expected2)
  expect_true(is.Matrix(actual2))
})


test_that("vectorize_byname() and matricize_byname() are inverses of each other", {
  m1 <- matrix(c(1, 2, 
                 3, 4, 
                 5, 6),
               nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  v1 <- vectorize_byname(m1, notation = RCLabels::arrow_notation)
  m2 <- matricize_byname(v1, notation = RCLabels::arrow_notation)
  expect_equal(m2, m1)
  v3 <- transpose_byname(v1)
  m4 <- matricize_byname(v3, notation = RCLabels::arrow_notation)
  expect_equal(m4, m1)
  
  # Try with a Matrix object.
  M1 <- matrix(c(1, 2, 
                 3, 4, 
                 5, 6),
               nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  V1 <- vectorize_byname(M1, notation = RCLabels::arrow_notation)
  M2 <- matricize_byname(V1, notation = RCLabels::arrow_notation)
  expect_equal(M2, M1)
  V3 <- transpose_byname(V1)
  M4 <- matricize_byname(V3, notation = RCLabels::arrow_notation)
  expect_equal(M4, M1)
})


test_that("fractionze_byname() works as expected", {
  M <- matrix(c(1, 5,
                4, 5),
              nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expectedM_rows <- matrix(c(1/6, 5/6,
                             4/9, 5/9),
                           nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expectedM_cols <- matrix(c(1/5, 5/10,
                             4/5, 5/10),
                           nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expectedM_sumall <- matrix(c(1/15, 5/15,
                               4/15, 5/15),
                             nrow = 2, ncol = 2, byrow = TRUE,
                             dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  
  # Test for errors
  expect_error(fractionize_byname(M, margin = c(2,2,1,1,0)), "margin should contain unique integers in fractionize_byname")
  expect_error(fractionize_byname(M, margin = c(2,2,1,1)), "margin should contain unique integers in fractionize_byname")
  expect_error(fractionize_byname(M, margin = c(1,1)), "margin should contain unique integers in fractionize_byname")
  expect_error(fractionize_byname(M, margin = c(2,2)), "margin should contain unique integers in fractionize_byname")
  expect_error(fractionize_byname(M, margin = c(1,2,3)), "margin should have length 1 or 2 in fractionize_byname")
  expect_error(fractionize_byname(M, margin = 3), "Unknown margin 3 in fractionize_byname")
  expect_error(fractionize_byname(M, margin = c(3,4)), "Unknown margin")
  expect_error(fractionize_byname(M, margin = -1), "Unknown margin")
  
  # Test with a single number
  expect_equal(fractionize_byname(2, margin = 1), 1) 
  expect_equal(fractionize_byname(-1, margin = 2), 1) 
  expect_equal(fractionize_byname(-5000, margin = c(1,2)), 1) 
  expect_equal(fractionize_byname(0, margin = 1), .Machine$double.xmax)
  
  # Test dividing by row sums
  expect_equal(fractionize_byname(M, margin = 1), expectedM_rows)
  
  # Test dividing by column sums
  expect_equal(fractionize_byname(M, margin = 2), expectedM_cols)
  
  # Test dividing by sum of all entries
  expect_equal(fractionize_byname(M, margin = c(1,2)), expectedM_sumall)
  expect_equal(fractionize_byname(M, margin = c(2,1)), expectedM_sumall)
  
  # Should also work for lists
  expect_equal(fractionize_byname(list(M,M), margin = 1), list(expectedM_rows, expectedM_rows))
  
  # Should also work for data frames
  DF <- data.frame(case = I(list()), M = I(list()))
  DF[[1, "case"]] <- 1
  DF[[1, "case"]] <- 2
  DF[[1, "M"]] <- M
  DF[[2, "M"]] <- M
  DF2 <- DF %>% 
    dplyr::mutate(
      F_row = fractionize_byname(M, margin = 1),
      F_col = fractionize_byname(M, margin = 2),
      F_tot = fractionize_byname(M, margin = list(c(2,1)))
    )
  
  expect_equal(DF2$F_row, list(expectedM_rows, expectedM_rows))
  expect_equal(DF2$F_col, list(expectedM_cols, expectedM_cols))
  expect_equal(DF2$F_tot, list(expectedM_sumall, expectedM_sumall))
  
  # Test when a column contains zeroes
  Mzerocol <- matrix(c(1, 0,
                       2, 0),
                     nrow = 2, ncol = 2, byrow = TRUE,
                     dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expect_equal(fractionize_byname(Mzerocol, margin = c(1,2)), 
               matrix(c(1/3, 0,
                        2/3, 0),
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  expect_equal(fractionize_byname(Mzerocol, margin = 1), 
               matrix(c(1, 0,
                        1, 0),
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  # Verify that the zero column now works and gives NaNs.
  expect_equal(fractionize_byname(Mzerocol, margin = 2, inf_becomes = Inf), 
               matrix(c(1/3, 0/0,
                        2/3, 0/0), 
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  # But if we clean the matrix first, we will also get something that makes sense.
  expect_equal(fractionize_byname(clean_byname(Mzerocol, margin = 2), margin = 2),
               matrix(c(1/3,
                        2/3),
                      nrow = 2, ncol = 1, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1"))))
  
  # Test when rows are zero.
  Mzerorow <- matrix(c(0, 0,
                       1, 2),
                     nrow = 2, ncol = 2, byrow = TRUE,
                     dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expect_equal(fractionize_byname(Mzerorow, margin = c(1,2)), 
               matrix(c(0,   0,
                        1/3, 2/3),
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  expect_equal(fractionize_byname(Mzerorow, margin = 1, inf_becomes = NaN), 
               matrix(c(0/0, 0/0,
                        1/3, 2/3),
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  expect_equal(fractionize_byname(Mzerorow, margin = 2), 
               matrix(c(0, 0,
                        1, 1),
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  
  # Test when everything is zero
  Mzero <- matrix(c(0, 0,
                    0, 0),
                  nrow = 2, ncol = 2, byrow = TRUE,
                  dimnames = list(c("p1", "p2"), c("i1", "i2")))
  expect_equal(fractionize_byname(Mzero, margin = 1, inf_becomes = NaN), 
               matrix(c(0/0, 0/0,
                        0/0, 0/0), 
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  expect_equal(fractionize_byname(Mzero, margin = 2, inf_becomes = NaN), 
               matrix(c(0/0, 0/0,
                        0/0, 0/0), 
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
  expect_equal(fractionize_byname(Mzero, margin = c(1,2)), 
               matrix(c(0/0, 0/0,
                        0/0, 0/0), 
                      nrow = 2, ncol = 2, byrow = TRUE,
                      dimnames = list(c("p1", "p2"), c("i1", "i2"))))
})


test_that("fractionize_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(1, 5,
                            4, 5),
                          nrow = 2, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("p1", "p2"), c("i1", "i2")), 
                          rowtype = "Products", coltype = "Industries")
  expectedM_rows <- matsbyname::Matrix(c(1/6, 5/6,
                                         4/9, 5/9),
                                       nrow = 2, ncol = 2, byrow = TRUE,
                                       dimnames = list(c("p1", "p2"), c("i1", "i2")), 
                                       rowtype = "Products", coltype = "Industries")
  expectedM_cols <- matsbyname::Matrix(c(1/5, 5/10,
                                         4/5, 5/10),
                                       nrow = 2, ncol = 2, byrow = TRUE,
                                       dimnames = list(c("p1", "p2"), c("i1", "i2")), 
                                       rowtype = "Products", coltype = "Industries")
  expectedM_sumall <- matsbyname::Matrix(c(1/15, 5/15,
                                           4/15, 5/15),
                                         nrow = 2, ncol = 2, byrow = TRUE,
                                         dimnames = list(c("p1", "p2"), c("i1", "i2")), 
                                         rowtype = "Products", coltype = "Industries")
  
  # Test dividing by row sums
  expect_equal(fractionize_byname(M, margin = 1), expectedM_rows)
  
  # Test dividing by column sums
  expect_equal(fractionize_byname(M, margin = 2), expectedM_cols)
  
  # Test dividing by sum of all entries
  expect_equal(fractionize_byname(M, margin = c(1,2)), expectedM_sumall)
  expect_equal(fractionize_byname(M, margin = c(2,1)), expectedM_sumall)
})


test_that("fractionize_byname() works with a single number 0", {
  expect_equal(fractionize_byname(0, margin = c(1, 2)), .Machine$double.xmax)
})


test_that("rowsums_byname() works as expected", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  expect_error(rowsums_byname("bogus"), "Unknown type for 'a' in rowsums_byname")
  m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  # Note, columns are sorted by name after rowsums_byname
  rowsumsm_expected <- matrix(c(9, 7, 5), nrow = 3, dimnames = list(paste0("i", 1:3), coltype(m))) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  expect_equal(rowsums_byname(m), rowsumsm_expected)
  expect_equal(rowsums_byname(m, "E.ktoe"), rowsumsm_expected %>% setcolnames_byname("E.ktoe"))
  # This also works with lists
  expect_equal(rowsums_byname(list(m, m)), list(rowsumsm_expected, rowsumsm_expected))
  expect_equal(rowsums_byname(list(m, m), "E.ktoe"), 
               list(rowsumsm_expected %>% setcolnames_byname("E.ktoe"), 
                    rowsumsm_expected %>% setcolnames_byname("E.ktoe")))
  rowsum_expected_no_colname <- rowsumsm_expected %>% 
    magrittr::set_colnames(NULL)
  expect_equal(rowsums_byname(list(m, m), NULL), list(rowsum_expected_no_colname, rowsum_expected_no_colname))
  # Also works with data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(rowsums_byname(DF$m), list(rowsumsm_expected, rowsumsm_expected))
  DF_expected <- data.frame(m = I(list()), mi = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"mi"]] <- rowsumsm_expected
  DF_expected[[2,"mi"]] <- rowsumsm_expected
  # Because DF_expected$mi is created with I(list()), its class is "AsIs".
  # Because DF$mi is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$mi to NULL to get a match.
  attr(DF_expected$mi, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(mi = rowsums_byname(m)), DF_expected)
})


test_that("rowsums_byname() works with single numbers and matrices", {
  expect_equal(rowsums_byname(1), 1)
  expect_equal(rowsums_byname(matrix(1)), matrix(1))
  expect_equal(rowsums_byname(list(1, 42)), list(1, 42))
  expect_equal(rowsums_byname(list(matrix(c(1, 42)), matrix(c(2, 43)))), list(matrix(c(1, 42)), matrix(c(2, 43))))
})


test_that("rowsums_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2)), 
                          rowtype = "Industries", coltype = "Products")
  # Note, columns are sorted by name after rowsums_byname()
  rowsumsM_expected <- matrix(c(9, 7, 5), nrow = 3, ncol = 1, dimnames = list(paste0("i", 1:3), coltype(M))) %>% 
    setrowtype(rowtype(M)) %>% setcoltype(coltype(M))
  res <- rowsums_byname(M)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, rowsumsM_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(rowsums_byname(M, "E.ktoe"), rowsumsM_expected %>% setcolnames_byname("E.ktoe"))
})


test_that("colsums_byname() works as expected", {
  expect_error(colsums_byname("bogus"), "Unknown type for 'a' in colsums_byname")
  m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  colsumsm_expected <- matrix(c(6, 15), nrow = 1, dimnames = list(rowtype(m), colnames(m))) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  expect_equal(colsums_byname(m), colsumsm_expected)
  expect_equal(colsums_byname(m, "E.ktoe"), colsumsm_expected %>% setrownames_byname("E.ktoe"))
  # This also works with lists
  expect_equal(colsums_byname(list(m, m)), list(colsumsm_expected, colsumsm_expected))
  expect_equal(colsums_byname(list(m, m), "E.ktoe"), 
               list(colsumsm_expected %>% setrownames_byname("E.ktoe"), 
                    colsumsm_expected %>% setrownames_byname("E.ktoe")))
  colsum_expected_no_colname <- colsumsm_expected %>% 
    magrittr::set_rownames(NULL)
  expect_equal(colsums_byname(list(m, m), NULL), list(colsum_expected_no_colname, colsum_expected_no_colname))
  # Also works with data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(colsums_byname(DF$m), list(colsumsm_expected, colsumsm_expected))
  DF_expected <- data.frame(m = I(list()), iTm = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"iTm"]] <- colsumsm_expected
  DF_expected[[2,"iTm"]] <- colsumsm_expected
  # Because DF_expected$iTm is created with I(list()), its class is "AsIs".
  # Because DF$iTm is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$iTm to NULL to get a match.
  attr(DF_expected$iTm, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(iTm = colsums_byname(m)), DF_expected)
})


test_that("colsums_byname() works with single numbers and matrices", {
  expect_equal(colsums_byname(1), 1)
  expect_equal(colsums_byname(matrix(1)), matrix(1))
  expect_equal(colsums_byname(list(1, 42)), list(1, 42))
  expect_equal(colsums_byname(list(matrix(c(1, 42)), matrix(c(2, 43)))), list(matrix(43), matrix(45)))
})


test_that("colsums_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2)), 
                          rowtype = "Industries", coltype = "Products")
  colsumsM_expected <- matrix(c(6, 15), nrow = 1, dimnames = list(rowtype(M), colnames(M))) %>% 
    setrowtype(rowtype(M)) %>% setcoltype(coltype(M))
  res <- colsums_byname(M)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, colsumsM_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(colsums_byname(M, "E.ktoe"), colsumsM_expected %>% setrownames_byname("E.ktoe"))
})


test_that("sumall_byname() works as expected", {
  expect_error(sumall_byname("bogus"), "Unknown type for 'a' in sumall_byname")
  m <- matrix(2, nrow = 2, ncol = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
    setrowtype("Industry") %>% setcoltype("Commodity")
  expect_equal(sumall_byname(m), 8)
  expect_equal(m %>% rowsums_byname %>% colsums_byname, 
               matrix(8, nrow = 1, ncol = 1, dimnames = list(rowtype(m), coltype(m))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Also works for lists
  expect_equal(sumall_byname(list(m,m)), list(8, 8))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(sumall_byname(DF$m), list(8,8))
  DF_expected <- data.frame(m = I(list()), summ = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m  
  DF_expected[[1,"summ"]] <- 8
  DF_expected[[2,"summ"]] <- 8
  # Because DF_expected$summ is created with I(list()), its class is "AsIs".
  # Because DF$summ is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$summ to NULL to get a match.
  attr(DF_expected$summ, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(summ = sumall_byname(m)), DF_expected)
})


test_that("sumall_byname() works for single numbers", {
  expect_equal(sumall_byname(matrix(1)), 1)
  expect_equal(sumall_byname(list(matrix(1), matrix(42))), list(1, 42))
  expect_equal(sumall_byname(1), 1)
  expect_equal(sumall_byname(1), 1)
})


test_that("sumall_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(2, nrow = 2, ncol = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2)), 
                          rowtype = "Industry", coltype = "Commodity")
  res <- sumall_byname(M)
  expect_equal(res, 8)
  matsbyname:::expect_equal_matrix_or_Matrix(M %>% rowsums_byname %>% colsums_byname, 
               matrix(8, nrow = 1, ncol = 1, dimnames = list(rowtype(M), coltype(M))) %>% 
                 setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
})


test_that("sum_byname() and sumall_byname() behave same with NULL", {
  expect_equal(sum_byname(0, 0), 0)
  expect_equal(sum_byname(1, 0), 1)
  expect_equal(sum_byname(1, NULL), 1)
  # I wish this were 0, but can't figure out how to make it so at the moment.
  # This result is inconsistent with sum_byname(NULL, NULL) returning 0.
  # expect_true(sum_byname(NULL), 0)
  expect_null(sum_byname(NULL))
  expect_equal(sum_byname(NULL, NULL), 0)
  expect_equal(sum_byname(NULL, NULL, NULL), 0)
  
  # Again, it seems like this should return 0, but it does not currently.
  # expect_equal(sumall_byname(NULL), 0)
  expect_null(sumall_byname(NULL))
  # expect_equal(sumall_byname(list(NULL, NULL)), list(0, 0))
  expect_equal(sum_byname(list(NULL, NULL)), list(NULL, NULL))
  
  expect_equal(sumall_byname(matrix(0, nrow = 1, ncol = 1)), 0)
  expect_equal(sumall_byname(matrix(1, nrow = 1, ncol = 1)), 1)
})


test_that("rowprods_byname() works as expected", {
  m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  # Note, columns are sorted by name after rowprods_byname
  rowprodsm_expected <- matrix(c(18, 10, 4), nrow = 3, dimnames = list(paste0("i", 1:3), coltype(m))) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  expect_equal(rowprods_byname(m), rowprodsm_expected)
  expect_equal(rowprods_byname(m, "E.ktoe"), rowprodsm_expected %>% setcolnames_byname("E.ktoe"))
  # This also works with lists
  expect_equal(rowprods_byname(list(m, m)), list(rowprodsm_expected, rowprodsm_expected))
  expect_equal(rowprods_byname(list(m, m), "E.ktoe"), 
               list(rowprodsm_expected %>% setcolnames_byname("E.ktoe"), 
                    rowprodsm_expected %>% setcolnames_byname("E.ktoe")))
  expect_equal(rowprods_byname(list(m, m), NULL), list(rowprodsm_expected, rowprodsm_expected))
  # Also works with data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(rowprods_byname(DF$m), list(rowprodsm_expected, rowprodsm_expected))
  DF_expected <- data.frame(m = I(list()), mi = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"mi"]] <- rowprodsm_expected
  DF_expected[[2,"mi"]] <- rowprodsm_expected
  # Because DF_expected$mi is created with I(list()), its class is "AsIs".
  # Because DF$mi is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$mi to NULL to get a match.
  attr(DF_expected$mi, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(mi = rowprods_byname(m)), DF_expected)
})


test_that("rowprods_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2)), 
                          rowtype = "Industries", coltype = "Products")
  # Note, columns are sorted by name after rowprods_byname
  rowprodsM_expected <- matrix(c(18, 10, 4), nrow = 3, dimnames = list(paste0("i", 1:3), coltype(M))) %>% 
    setrowtype(rowtype(M)) %>% setcoltype(coltype(M))
  res <- rowprods_byname(M)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, rowprodsM_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(rowprods_byname(M, "E.ktoe"), rowprodsM_expected %>% setcolnames_byname("E.ktoe"))
})


test_that("colprods_byname() works as expected", {
  m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  colprodsm_expected <- matrix(c(6, 120), nrow = 1, dimnames = list(rowtype(m), colnames(m))) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  expect_equal(colprods_byname(m), colprodsm_expected)
  expect_equal(colprods_byname(m, "E.ktoe"), colprodsm_expected %>% setrownames_byname("E.ktoe"))
  # This also works with lists
  expect_equal(colprods_byname(list(m, m)), list(colprodsm_expected, colprodsm_expected))
  expect_equal(colprods_byname(list(m, m), "E.ktoe"), 
               list(colprodsm_expected %>% setrownames_byname("E.ktoe"), 
                    colprodsm_expected %>% setrownames_byname("E.ktoe")))
  expect_equal(colprods_byname(list(m, m), NULL), list(colprodsm_expected, colprodsm_expected))
  # Also works with data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(colprods_byname(DF$m), list(colprodsm_expected, colprodsm_expected))
  DF_expected <- data.frame(m = I(list()), iTm = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"iTm"]] <- colprodsm_expected
  DF_expected[[2,"iTm"]] <- colprodsm_expected
  # Because DF_expected$iTm is created with I(list()), its class is "AsIs".
  # Because DF$iTm is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$iTm to NULL to get a match.
  attr(DF_expected$iTm, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(iTm = colprods_byname(m)), DF_expected)
})


test_that("colprods_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(paste0("i", 3:1), paste0("p", 1:2)), 
                          rowtype = "Industries", coltype = "Products")
  colprodsM_expected <- matrix(c(6, 120), nrow = 1, dimnames = list(rowtype(M), colnames(M))) %>% 
    setrowtype(rowtype(M)) %>% setcoltype(coltype(M))
  res <- colprods_byname(M)
  expect_true(is.Matrix(M))
  matsbyname:::expect_equal_matrix_or_Matrix(res, colprodsM_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(colprods_byname(M, "E.ktoe"), colprodsM_expected %>% setrownames_byname("E.ktoe"))
})


test_that("prodall_byname() works as expected", {
  m <- matrix(2, nrow = 2, ncol = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(prodall_byname(m), 16)
  expect_equal(m %>% rowprods_byname %>% colprods_byname, 
               matrix(16, nrow = 1, ncol = 1, dimnames = list(rowtype(m), coltype(m))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Also works for lists
  expect_equal(prodall_byname(list(m,m)), list(16, 16))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(prodall_byname(DF$m), list(16, 16))
  DF_expected <- data.frame(m = I(list()), prodm = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m  
  DF_expected[[1,"prodm"]] <- 16
  DF_expected[[2,"prodm"]] <- 16
  # Because DF_expected$summ is created with I(list()), its class is "AsIs".
  # Because DF$summ is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$summ to NULL to get a match.
  attr(DF_expected$prodm, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(prodm = prodall_byname(m)), DF_expected)
})


test_that("prodall_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(2, nrow = 2, ncol = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2)), 
                          rowtype = "Industry", coltype = "Product")
  expect_equal(prodall_byname(M), 16)
  matsbyname:::expect_equal_matrix_or_Matrix(M %>% rowprods_byname() %>% colprods_byname(), 
                                             matrix(16, nrow = 1, ncol = 1, dimnames = list(rowtype(M), coltype(M))) %>% 
                                               setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
})


test_that("Iminus_byname() works as expected", {
  m <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  Iminus_expected <- matrix(c(11, 12, 
                              21, 22),
                            nrow = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("a", "b"))) %>% 
    setrowtype(rowtype(m)) %>% setcoltype(coltype(m))
  # Rows and columns of m are unsorted
  expect_equal(diag(1, nrow = 2) - m, matrix(c(22, 12, 21, 11), nrow = 2, dimnames = dimnames(m)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Rows and columns of m are sorted prior to subtracting from the identity matrix
  expect_equal(Iminus_byname(m), Iminus_expected)
  # This also works with lists
  expect_equal(Iminus_byname(list(m,m)), list(Iminus_expected, Iminus_expected))
  # Also works with data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(Iminus_byname(DF$m), list(Iminus_expected, Iminus_expected))
  DF_expected <- data.frame(m = I(list()), Iminusm = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"Iminusm"]] <- Iminus_expected
  DF_expected[[2,"Iminusm"]] <- Iminus_expected
  # Because DF_expected$Iminusm is created with I(list()), its class is "AsIs".
  # Because DF$Iminusm is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$Iminusm to NULL to get a match.
  attr(DF_expected$Iminusm, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(Iminusm = Iminus_byname(m)), DF_expected)
  
  # If m is not square before subtracting from I,
  # it will be made square by the function complete_and_sort.
  m2 <- matrix(c(1,2,3,4,5,6), ncol = 2, dimnames = list(c("a", "b", "c"), c("a", "b"))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(Iminus_byname(m2), 
               matrix(c(0, -4, 0, 
                        -2, -4, 0, 
                        -3, -6, 1), 
                      nrow = 3, byrow = TRUE, dimnames = list(c("a", "b", "c"), c("a", "b", "c"))) %>% 
                 setrowtype(rowtype(m2)) %>% setcoltype(coltype(m2)))
})


test_that("iminus_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(-21, -12, -21, -10), nrow = 2, ncol = 2, dimnames = list(c("b", "a"), c("b", "a")), 
                          rowtype = "Industries", coltype = "Products")
  Iminus_expected <- matsbyname::Matrix(c(11, 12, 
                                          21, 22),
                                        nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("a", "b")), 
                                        rowtype = rowtype(M), coltype = coltype(M))
  # Rows and columns of m are unsorted
  expect_equal(matsbyname::Matrix(diag(1, nrow = 2, ncol = 2) - M), matsbyname::Matrix(c(22, 12, 21, 11), nrow = 2, ncol = 2))
  # Rows and columns of m are sorted prior to subtracting from the identity matrix
  expect_equal(Iminus_byname(M), Iminus_expected)
  # This also works with lists
  expect_equal(Iminus_byname(list(M, M)), list(Iminus_expected, Iminus_expected))
})


test_that("clean_byname() works as expected", {
  # Clean on rows
  mat1 <- matrix(c(0,1,0,1), nrow = 2, dimnames = list(c("r (1)", "r (2)"), c("c (1)", "c (2)"))) %>% 
    setrowtype("Rows") %>% setcoltype("Cols")
  # Now clean in rows Should eliminate row 1.
  expect_equal(mat1 %>% clean_byname(margin = 1, clean_value = 0), 
               matrix(1, nrow = 1, ncol = 2, dimnames = list("r (2)", c("c (1)", "c (2)"))) %>% 
                 setrowtype("Rows") %>% setcoltype("Cols"))
  # No column consists of all zeroes. So nothing to clean in columns Should get "mat1" back.
  expect_equal(mat1 %>% clean_byname(margin = 2, clean_value = 0), mat1)
  # Clean on columns
  mat2 <- matrix(c(0,0,1,1), nrow = 2, dimnames = list(c("r (1)", "r (2)"), c("c (1)", "c (2)"))) %>% 
    setrowtype("Rows") %>% setcoltype("Cols")
  # No row consists of all zeroes. So nothing to clean in rows. Should get "mat2" back.
  expect_equal(mat2 %>% clean_byname(margin = 1, clean_value = 0), mat2)
  # Now clean in columns. Should eliminate column 1.
  expect_equal(mat2 %>% clean_byname(margin = 2, clean_value = 0), 
               matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r (1)", "r (2)"), "c (2)")) %>% 
                 setrowtype("Rows") %>% setcoltype("Cols"))
})


test_that("clean_byname() works as expected for Matrix objects", {
  Mat1 <- matsbyname::Matrix(c(0,1,0,1), nrow = 2, ncol = 2, dimnames = list(c("r (1)", "r (2)"), c("c (1)", "c (2)")), 
                             rowtype = "Rows", coltype = "Cols")
  # Now clean in rows Should eliminate row 1.
  res1 <- Mat1 %>% 
    clean_byname(margin = 1, clean_value = 0)
  expect_true(is.Matrix(res1))
  matsbyname:::expect_equal_matrix_or_Matrix(res1, 
                                             matrix(1, nrow = 1, ncol = 2, dimnames = list("r (2)", c("c (1)", "c (2)"))) %>% 
                                               setrowtype("Rows") %>% setcoltype("Cols"))
  # No column consists of all zeroes. So nothing to clean in columns Should get "mat1" back.
  matsbyname:::expect_equal_matrix_or_Matrix(Mat1 %>% 
                                               clean_byname(margin = 2, clean_value = 0), 
                                             Mat1)
  # Clean on columns
  Mat2 <- matsbyname::Matrix(c(0,0,1,1), nrow = 2, ncol = 2, dimnames = list(c("r (1)", "r (2)"), c("c (1)", "c (2)")), 
                             rowtype = "Rows", coltype = "Cols")
  res2 <- Mat2 %>% 
    clean_byname(margin = 1, clean_value = 0)
  # No row consists of all zeroes. So nothing to clean in rows. Should get "mat2" back.
  matsbyname:::expect_equal_matrix_or_Matrix(res2, Mat2)
  # Now clean in columns. Should eliminate column 1.
  res3 <- Mat2 %>%
    clean_byname(margin = 2, clean_value = 0)
  expect_true(is.Matrix(res3))
  matsbyname:::expect_equal_matrix_or_Matrix(res3, 
                                             matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r (1)", "r (2)"), "c (2)")) %>% 
                                               setrowtype("Rows") %>% setcoltype("Cols"))
})


test_that("cumsum_byname() works as expected", {
  expect_null(cumsum_byname(NULL))
  expect_true(is.na(cumsum_byname(NA)))
  expect_equal(cumsum_byname(2), 2)
  
  lst <- list(1, 2, 3, 4, 5)
  lst_expected <- list(1, 3, 6, 10, 15)
  expect_equal(cumsum_byname(lst), lst_expected)
  # Try in a data frame.
  DF <- data.frame(l1 = I(lst), l2 = I(lst))
  CS <- DF %>% 
    dplyr::mutate(
      cs1 = cumsum_byname(l1), 
      cs2 = cumsum_byname(l2)
    )
  expect_equal(CS$cs1, lst_expected)
  expect_equal(CS$cs2, lst_expected)
  
  # Try with matrices
  rowmat <- matrix(c(1, 2, 3), nrow = 1)
  expect_equal(cumsum_byname(rowmat), rowmat)
  # Test in a list
  expect_equal(cumsum_byname(list(rowmat, rowmat, rowmat)), list(rowmat, 2*rowmat, 3*rowmat))
  # Test in a data frame
  DF2 <- data.frame(m = I(list(rowmat, rowmat, rowmat))) %>% 
    dplyr::mutate(
      m2 = cumsum_byname(m)
    )
  expect_equal(DF2$m2, list(rowmat, 2*rowmat, 3*rowmat))
  # Test with a matrix that will take advantage of the "by name" aspect of sum_byname
  m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
    setrowtype("row") %>% setcoltype("col")
  m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>% 
    setrowtype("row") %>% setcoltype("col")
  m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>% 
    setrowtype("row") %>% setcoltype("col")
  mlist <- list(m1, m2, m3)
  expected <- list(m1, sum_byname(m1, m2), sum_byname(m1, m2) %>% sum_byname(m3))
  expect_equal(cumsum_byname(mlist), expected)
  
  # Ensure that groups are respected in the context of mutate.
  DF3 <- tibble::tibble(grp = c("A", "A", "B"), m = mlist) %>% 
    dplyr::group_by(grp) %>% 
    dplyr::mutate(
      m2 = cumsum_byname(m)
    )
  expect_equal(DF3$m2, list(m1, sum_byname(m1, m2), m3))
})


test_that("cumsum_byname() works with Matrix objects", {
  rowmat <- matsbyname::Matrix(c(1, 2, 3), nrow = 1, ncol = 3)
  expect_equal(cumsum_byname(rowmat), rowmat)
  
  # Test in a list
  expect_equal(cumsum_byname(list(rowmat, rowmat, rowmat)), list(rowmat, 2*rowmat, 3*rowmat))
  # Test in a data frame
  DF2 <- data.frame(m = I(list(rowmat, rowmat, rowmat))) %>% 
    dplyr::mutate(
      m2 = cumsum_byname(m)
    )
  expect_equal(DF2$m2, list(rowmat, 2*rowmat, 3*rowmat))
})


test_that("cumprod_byname() works as expected", {
  expect_null(cumprod_byname(NULL))
  expect_true(is.na(cumprod_byname(NA)))
  expect_equal(cumprod_byname(2), 2)

  lst <- list(1, 2, 3, 4, 5)
  lst_expected <- list(1, 2, 6, 24, 120)
  expect_equal(cumprod_byname(lst), lst_expected)
  # Try in a data frame.
  DF <- data.frame(l1 = I(lst), l2 = I(lst))
  CS <- DF %>%
    dplyr::mutate(
      cs1 = cumprod_byname(l1),
      cs2 = cumprod_byname(l2)
    )
  expect_equal(CS$cs1, lst_expected)
  expect_equal(CS$cs2, lst_expected)

  # Try with matrices
  rowmat <- matrix(c(1, 2, 3), nrow = 1)
  expect_equal(cumprod_byname(rowmat), rowmat)
  # Test in a list
  expected_powers <- list(rowmat, rowmat*rowmat, rowmat*rowmat*rowmat)
  expect_equal(cumprod_byname(list(rowmat, rowmat, rowmat)), expected_powers)
  # Test in a data frame
  DF2 <- data.frame(m = I(list(rowmat, rowmat, rowmat))) %>%
    dplyr::mutate(
      m2 = cumprod_byname(m)
    )
  expect_equal(DF2$m2, expected_powers)
  # Test with a matrix that will take advantage of the "by name" aspect of sum_byname
  m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>%
    setrowtype("row") %>% setcoltype("col")
  m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>%
    setrowtype("row") %>% setcoltype("col")
  m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>%
    setrowtype("row") %>% setcoltype("col")
  mlist <- list(m1, m2, m3)
  expected <- list(m1, sum_byname(m1, m2) * 0, (sum_byname(m1, m2, m3)) * 0)
  expect_equal(cumprod_byname(mlist), expected)

  # Ensure that groups are respected in the context of mutate.
  # DF3 <- data.frame(grp = c("A", "A", "B"), m = I(mlist)) %>%
  DF3 <- tibble::tibble(grp = c("A", "A", "B"), m = mlist) %>%
    dplyr::group_by(grp) %>%
    dplyr::mutate(
      m2 = cumprod_byname(m)
    )
  expect_equal(DF3$m2, list(m1, sum_byname(m1, m2) * 0, m3))
})


test_that("cumprod_byname() works with Matrix objects", {
  # Try with matrices
  rowmat <- matsbyname::Matrix(c(1, 2, 3), nrow = 1, ncol = 3)
  expect_equal(cumprod_byname(rowmat), rowmat)
  # Test in a list
  expected_powers <- list(rowmat, rowmat*rowmat, rowmat*rowmat*rowmat)
  expect_equal(cumprod_byname(list(rowmat, rowmat, rowmat)), expected_powers)
  # Test in a data frame
  DF2 <- data.frame(m = I(list(rowmat, rowmat, rowmat))) %>%
    dplyr::mutate(
      m2 = cumprod_byname(m)
    )
  expect_equal(DF2$m2, expected_powers)
  # Test with a matrix that will take advantage of the "by name" aspect of sum_byname
  m1 <- matsbyname::Matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1"), rowtype = "row", coltype = "col")
  m2 <- matsbyname::Matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2"), rowtype = "row", coltype = "col")
  m3 <- matsbyname::Matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3"), rowtype = "row", coltype = "col")
  mlist <- list(m1, m2, m3)
  res <- cumprod_byname(mlist)
  expected <- list(m1, sum_byname(m1, m2) * 0, (sum_byname(m1, m2, m3)) * 0)

  for (i in 1:3) {
    expect_true(is.Matrix(res[[i]]))
    matsbyname:::expect_equal_matrix_or_Matrix(res[[i]], expected[[i]])
  }
})


test_that("replaceNaN() works as expected", {
  expected <- matrix(c(1, 0))
  suppressWarnings(a <- matrix(c(1, sqrt(-1))))
  expect_equal(replaceNaN_byname(a), expected)
  # Should work with lists
  expect_equal(replaceNaN_byname(list(a,a)), list(expected, expected))
  # Try with a different value
  expect_equal(replaceNaN_byname(a, 42), matrix(c(1,42)))
})


test_that("replaceNaN_byname() works with Matrix objects", {
  suppressWarnings(a <- matsbyname::Matrix(c(1, 
                                             sqrt(-1)), byrow = TRUE,
                                           nrow = 2, ncol = 1,
                                           dimnames <- list(c("r1", "r2"), "c1"), 
                                           rowtype = "row", coltype = "col"))
  expected <- matsbyname::Matrix(c(1, 
                                   0), byrow = TRUE, 
                                 nrow = 2, ncol = 1, 
                                 dimnames <- list(c("r1", "r2"), "c1"), 
                                 rowtype = "row", coltype = "col")
  expect_equal(replaceNaN_byname(a), expected)
  # Should work with lists
  expect_equal(replaceNaN_byname(list(a,a)), list(expected, expected))
  # Try with a different value
  expect_equal(replaceNaN_byname(a, 42),
               matsbyname::Matrix(c(1, 
                                    42), byrow = TRUE, 
                                  nrow = 2, ncol = 1, 
                                  dimnames <- list(c("r1", "r2"), "c1"), 
                                  coltype = "col", rowtype = "row"))
})


test_that("count_vals_byname() works as expected", {
  m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  expect_equal(count_vals_byname(m), 2)
  expect_equal(count_vals_byname(m, compare_fun = "==", 0), 2)
  expect_equal(count_vals_byname(m, compare_fun = `==`, 0), 2)
  expect_equal(count_vals_byname(m, "==", 0), 2)
  expect_equal(count_vals_byname(m, compare_fun = "!="), 4)
  expect_equal(count_vals_byname(m, compare_fun = `!=`), 4)
  expect_equal(count_vals_byname(m, "<", 1), 2)
  expect_equal(count_vals_byname(m, "<=", 1), 3)
  expect_equal(count_vals_byname(m, ">=", 3), 2)
  expect_equal(count_vals_byname(m, ">", 4), 0)
  expect_equal(count_vals_byname(m, `>`, 4), 0)
  # Should also work for lists
  l <- list(m, m)
  expect_equal(count_vals_byname(l, `>`, 4), list(0, 0))
})


test_that("count_vals_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  expect_equal(count_vals_byname(m), 2)
  expect_equal(count_vals_byname(m, compare_fun = "==", 0), 2)
  expect_equal(count_vals_byname(m, compare_fun = `==`, 0), 2)
  expect_equal(count_vals_byname(m, "==", 0), 2)
  expect_equal(count_vals_byname(m, compare_fun = "!="), 4)
  expect_equal(count_vals_byname(m, compare_fun = `!=`), 4)
  expect_equal(count_vals_byname(m, "<", 1), 2)
  expect_equal(count_vals_byname(m, "<=", 1), 3)
  expect_equal(count_vals_byname(m, ">=", 3), 2)
  expect_equal(count_vals_byname(m, ">", 4), 0)
  expect_equal(count_vals_byname(m, `>`, 4), 0)
  # Should also work for lists
  l <- list(m, m)
  expect_equal(count_vals_byname(l, `>`, 4), list(0, 0))
})


test_that("compare_byname() works as expected", {
  m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  expect_equal(compare_byname(m), matrix(c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), nrow = 3, ncol = 2))
  expect_equal(compare_byname(m, "<", 3), 
               matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE), nrow = 3, ncol = 2))
})


test_that("compare_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  matsbyname:::expect_equal_matrix_or_Matrix(compare_byname(m), 
                                             matsbyname::Matrix(c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE), nrow = 3, ncol = 2))
  expect_equal(compare_byname(m, "<", 3), 
               matsbyname::Matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE), nrow = 3, ncol = 2))
})


test_that("count_vals_inrows_byname() works as expected", {
  m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  expect_equal(count_vals_inrows_byname(m), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, compare_fun = "==", 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, compare_fun = `==`, 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, "==", 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, compare_fun = "!="), matrix(c(1, 2, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, compare_fun = `!=`), matrix(c(1, 2, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, "<", 1), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, "<=", 1), matrix(1, nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, ">=", 3), matrix(c(1, 1, 0), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, ">", 4), matrix(c(0, 0, 0), nrow = 3, ncol = 1))
  expect_equal(count_vals_inrows_byname(m, `>`, 4), matrix(c(0, 0, 0), nrow = 3, ncol = 1))
  # Should also work for lists
  l <- list(m, m)
  ans <- matrix(c(0, 0, 0), nrow = 3, ncol = 1)
  expect_equal(count_vals_inrows_byname(l, `>`, 4), list(ans, ans))
})


test_that("count_vals_inrows_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m), 
                                             matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, compare_fun = "==", 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, compare_fun = `==`, 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, "==", 0), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, compare_fun = "!="), matrix(c(1, 2, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, compare_fun = `!=`), matrix(c(1, 2, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, "<", 1), matrix(c(1, 0, 1), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, "<=", 1), matrix(1, nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, ">=", 3), matrix(c(1, 1, 0), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, ">", 4), matrix(c(0, 0, 0), nrow = 3, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_inrows_byname(m, `>`, 4), matrix(c(0, 0, 0), nrow = 3, ncol = 1))
  # Should also work for lists
  l <- list(m, m)
  ans <- matrix(c(0, 0, 0), nrow = 3, ncol = 1)
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, count_vals_inrows_byname(l, `>`, 4), list(ans, ans))
})


test_that("count_vals_incols_byname() works as expected", {
  m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  expect_equal(count_vals_incols_byname(m), matrix(c(1, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, compare_fun = "==", 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, compare_fun = `==`, 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, "==", 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, compare_fun = "!="), matrix(c(2, 2), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, compare_fun = `!=`), matrix(c(2, 2), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, "<", 1), matrix(c(1, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, "<=", 1), matrix(c(2, 1), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, ">=", 3), matrix(c(0, 2), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, ">", 4), matrix(c(0, 0), nrow = 1, ncol = 2))
  expect_equal(count_vals_incols_byname(m, `>`, 4), matrix(c(0, 0), nrow = 1, ncol = 2))
  # Should also work for lists
  l <- list(m, m)
  ans <- matrix(c(0, 2), nrow = 1, ncol = 2)
  expect_equal(count_vals_incols_byname(l, `>`, 2), list(ans, ans))
})


test_that("count_vals_incols_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
  # By default, looks for 0's and checks for equality
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M), matrix(c(1, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, compare_fun = "==", 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, compare_fun = `==`, 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, "==", 0), matrix(c(1, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, compare_fun = "!="), matrix(c(2, 2), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, compare_fun = `!=`), matrix(c(2, 2), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, "<", 1), matrix(c(1, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, "<=", 1), matrix(c(2, 1), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, ">=", 3), matrix(c(0, 2), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, ">", 4), matrix(c(0, 0), nrow = 1, ncol = 2))
  matsbyname:::expect_equal_matrix_or_Matrix(count_vals_incols_byname(M, `>`, 4), matrix(c(0, 0), nrow = 1, ncol = 2))
  # Should also work for lists
  l <- list(M, M)
  ans <- matrix(c(0, 2), nrow = 1, ncol = 2)
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, count_vals_incols_byname(l, `>`, 2), list(ans, ans))
})
  

test_that("any_byname() works as expected", {
  m <- matrix(rep(TRUE, times = 4), nrow = 2, ncol = 2)
  expect_true(all_byname(m))
  expect_true(any_byname(m))
  
  n <- matrix(c(TRUE, FALSE), nrow = 2, ncol = 1)
  expect_false(all_byname(n))
  expect_true(any_byname(n))
  
  # Also works for lists
  expect_equal(all_byname(list(m,m)), list(TRUE, TRUE))
  expect_equal(any_byname(list(m,m)), list(TRUE, TRUE))
  expect_equal(all_byname(list(n,n)), list(FALSE, FALSE))
  expect_equal(any_byname(list(n,n)), list(TRUE, TRUE))
})


test_that("any_byname() works as expected with Matrix objexts", {
  M <- matsbyname::Matrix(rep(TRUE, times = 4), nrow = 2, ncol = 2)
  expect_true(all_byname(M))
  expect_true(any_byname(M))
  
  N <- matsbyname::Matrix(c(TRUE, FALSE), nrow = 2, ncol = 1)
  expect_false(all_byname(N))
  expect_true(any_byname(N))
  
  # Also works for lists
  expect_equal(all_byname(list(M, M)), list(TRUE, TRUE))
  expect_equal(any_byname(list(M, M)), list(TRUE, TRUE))
  expect_equal(all_byname(list(N, N)), list(FALSE, FALSE))
  expect_equal(any_byname(list(N, N)), list(TRUE, TRUE))
})


test_that("rename_to_pref_suff_byname() works as expected", {
  m <- matrix(1:4, ncol = 1, dimnames = list(letters[1:4], "Product -> Industry"))
  # This aggregation should simply return m with a renamed column.
  res <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expected <- m %>% 
    magrittr::set_colnames("Industry")
  expect_equal(res, expected)
})


test_that("rename_to_pref_suff_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 4, ncol = 1, dimnames = list(letters[1:4], "Product -> Industry"))
  # This aggregation should simply return m with a renamed column.
  res <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expected <- m %>% 
    magrittr::set_colnames("Industry")
  expect_equal(res, expected)
})


