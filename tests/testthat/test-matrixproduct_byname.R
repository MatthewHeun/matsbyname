test_that("matrixproduct_byname() works as expected", {
  V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("p1", "p2", "p3"))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  Y <- matrix(1:4, ncol = 2, dimnames = list(c("p2", "p1"), c("s2", "s1"))) %>%
    setrowtype("Products") %>% setcoltype("Sectors")
  Z <- matrix(11:14, ncol = 2, dimnames = list(c("s2", "s1"), c("c1", "c2"))) %>% 
    setrowtype("Sectors") %>% setcoltype("Columns")
  VY <- matrix(c(13,5,
                 20,8),
               nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("i1", "i2"), c("s1", "s2"))) %>% 
    setrowtype("Industries") %>% setcoltype("Sectors")
  VYZ <- matrixproduct_byname(VY, Z)

  # Fails. 3 columns of V cannot be matrix multiplied into 2 rows of Y.  Y lacks a row named p3.
  expect_error(V %*% Y, "non-conformable arguments")
  # Succeeds because Y is completed to include a row named p3 (that contains zeroes).
  # Furthermore, rows and columns of Y are sorted to be in alphabetical order.
  expect_equal(matrixproduct_byname(V, Y), VY)
  expect_equal(matrixproduct_byname(V, Y, Z), VYZ)
  
  # Check that it works down a list if .summarise = TRUE.
  expect_equal(matrixproduct_byname(list(V, Y, Z), .summarise = TRUE), list(VYZ))
    
  M <- matrix(c(11, 12,
                21, 22),
              nrow = 2, ncol = 2, byrow = TRUE) %>% 
    setrownames_byname(c("C", "D")) %>% setcolnames_byname(c("A", "B"))
  I <- identize_byname(M) %>%
    setrownames_byname(c("A", "B")) %>% setcolnames_byname(c("E", "F"))
  expect_equal(matrixproduct_byname(M, I), M %>% setcolnames_byname(colnames(I)))
  I2 <- I %>% setrownames_byname(c("G", "H"))
  # Next line produces results you would expect if you respect 
  # names for the columns of M and the rows of I2.
  expect_equal(matrixproduct_byname(M, I2), 
               matrix(c(0,0,
                        0,0),
                      nrow = 2, ncol = 2, byrow = TRUE) %>% 
                 setrownames_byname(c("C", "D")) %>% 
                 setcolnames_byname(c("E", "F")))
  
  # This works, but does not respect the fact that column names of M
  # are different from the row names of I2.
  expect_equal(M %*% I2, M %>% setrownames_byname(c("C", "D")) %>% setcolnames_byname(c("E", "F")))
  
  # This also works with lists
  expect_equal(matrixproduct_byname(list(V,V), list(Y,Y)), list(VY, VY))
  # And data frames (whose columns are lists)
  DF <- data.frame(V = I(list()), Y = I(list()), Z = I(list()))
  DF[[1,"V"]] <- V
  DF[[2,"V"]] <- V
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z
  expect_equal(matrixproduct_byname(DF$V, DF$Y), list(VY, VY))
  expect_equal(matrixproduct_byname(DF$V, DF$Y, DF$Z), list(VYZ, VYZ))
  
  # And it works with the tidyverse functions
  DF_expected <- data.frame(V = I(list()), Y = I(list()), Z = I(list()), matprods = I(list()), VYZ = I(list()))
  DF_expected[[1, "V"]] <- V
  DF_expected[[2, "V"]] <- V
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "Z"]] <- Z
  DF_expected[[2, "Z"]] <- Z
  DF_expected[[1, "matprods"]] <- VY
  DF_expected[[2, "matprods"]] <- VY
  DF_expected[[1, "VYZ"]] <- VYZ
  DF_expected[[2, "VYZ"]] <- VYZ
  # Because DF_expected$matprods is created with I(list()), its class is "AsIs".
  # Because DF$matprods is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$matprods to NULL to get a match.
  attr(DF_expected$matprods, which = "class") <- NULL
  attr(DF_expected$VYZ, which = "class") <- NULL
  expect_equal(DF %>% 
                 dplyr::mutate(
                   matprods = matrixproduct_byname(V, Y),
                   VYZ = matrixproduct_byname(V, Y, Z)
                 ), 
               DF_expected)
  
  # Test whether this works with a column of matrices multiplied by a single matrix.
  # In other words, we want a single matrix to multiply several matrices.
  # M is a single matrix. 
  # Should obtain same results as above.
  M <- Y
  expect_equal(DF %>% 
                 dplyr::mutate(
                   matprods = matrixproduct_byname(V, M),
                   VYZ = matrixproduct_byname(V, M, Z)), 
               DF_expected)
})


test_that("matrixproduct_byname() works with Matrix objects", {
  V <- matsbyname::Matrix(1:6, nrow = 2, ncol = 3, 
                          dimnames = list(c("i1", "i2"), c("p1", "p2", "p3"))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  Y <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2,
                          dimnames = list(c("p2", "p1"), c("s2", "s1"))) %>%
    setrowtype("Products") %>% setcoltype("Sectors")
  Z <- matrix(11:14, nrow = 2, ncol = 2,
              dimnames = list(c("s2", "s1"), c("c1", "c2"))) %>% 
    setrowtype("Sectors") %>% setcoltype("Columns")
  VY <- matsbyname::Matrix(c(13,5,
                             20,8),
                           nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("i1", "i2"), c("s1", "s2"))) %>% 
    setrowtype("Industries") %>% setcoltype("Sectors")
  VYZ <- matrixproduct_byname(VY, Z)
  
  # Fails. 3 columns of V cannot be matrix multiplied into 2 rows of Y.  Y lacks a row named p3.
  # expect_error(V %*% Y, "Matrices are not conformable for multiplication")
  # New error message in Matrix 1.6-2 is "non-conformaable arguments".
  # But no sense is testing for the exact error message. 
  # Just test that we do, in fact, receive some type of error.
  expect_error(V %*% Y)
  # Succeeds because Y is completed to include a row named p3 (that contains zeroes).
  # Furthermore, rows and columns of Y are sorted to be in alphabetical order.
  expect_equal(matrixproduct_byname(V, Y), VY)
  expect_equal(matrixproduct_byname(V, Y, Z), VYZ)
  
  # Check that it works down a list if .summarise = TRUE.
  expect_equal(matrixproduct_byname(list(V, Y, Z), .summarise = TRUE), list(VYZ))
  
  M <- matsbyname::Matrix(c(11, 12,
                            21, 22), nrow = 2, ncol = 2, byrow = TRUE) %>% 
    setrownames_byname(c("C", "D")) %>% setcolnames_byname(c("A", "B"))
  I <- identize_byname(M) %>%
    setrownames_byname(c("A", "B")) %>% setcolnames_byname(c("E", "F"))
  expect_equal(matrixproduct_byname(M, I), M %>% setcolnames_byname(colnames(I)))
  I2 <- I %>% setrownames_byname(c("G", "H"))
  # Next line produces results you would expect if you respect 
  # names for the columns of M and the rows of I2.
  matsbyname:::expect_equal_matrix_or_Matrix(
    matrixproduct_byname(M, I2), 
    matsbyname::Matrix(c(0,0,
                         0,0),
                       nrow = 2, ncol = 2, byrow = TRUE) %>% 
      setrownames_byname(c("C", "D")) %>% 
      setcolnames_byname(c("E", "F")))
  
  # On my macs, this does not respect the fact that column names of M
  # are different from the row names of I2.
  # expect_equal(M %*% I2, M)
  # Weirdly, on win-builder, 
  # The above test fails, because the column names of the result are taken from I2.
  # So I'm commenting the test for now.
  
  # This also works with lists
  expect_equal(matrixproduct_byname(list(V,V), list(Y,Y)), list(VY, VY))
  # And data frames (whose columns are lists)
  DF <- data.frame(V = I(list()), Y = I(list()), Z = I(list()))
  DF[[1,"V"]] <- V
  DF[[2,"V"]] <- V
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z
  expect_equal(matrixproduct_byname(DF$V, DF$Y), list(VY, VY))
  expect_equal(matrixproduct_byname(DF$V, DF$Y, DF$Z), list(VYZ, VYZ))
  
  # And it works with the tidyverse functions
  DF_expected <- data.frame(V = I(list()), Y = I(list()), Z = I(list()), matprods = I(list()), VYZ = I(list()))
  DF_expected[[1, "V"]] <- V
  DF_expected[[2, "V"]] <- V
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "Z"]] <- Z
  DF_expected[[2, "Z"]] <- Z
  DF_expected[[1, "matprods"]] <- VY
  DF_expected[[2, "matprods"]] <- VY
  DF_expected[[1, "VYZ"]] <- VYZ
  DF_expected[[2, "VYZ"]] <- VYZ
  # Because DF_expected$matprods is created with I(list()), its class is "AsIs".
  # Because DF$matprods is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$matprods to NULL to get a match.
  attr(DF_expected$matprods, which = "class") <- NULL
  attr(DF_expected$VYZ, which = "class") <- NULL
  expect_equal(DF %>% 
                 dplyr::mutate(
                   matprods = matrixproduct_byname(V, Y),
                   VYZ = matrixproduct_byname(V, Y, Z)
                 ), 
               DF_expected)
  
  # Test whether this works with a column of matrices multiplied by a single matrix.
  # In other words, we want a single matrix to multiply several matrices.
  # M is a single matrix. 
  # Should obtain same results as above.
  M <- Y
  expect_equal(DF %>% 
                 dplyr::mutate(
                   matprods = matrixproduct_byname(V, M),
                   VYZ = matrixproduct_byname(V, M, Z)), 
               DF_expected)
})


test_that("matrixproduct_byname() with NA is correct", {
  Z <- 42 %>% setrowtype("Product") %>% setcoltype("Industry")
  D <- 42 %>% setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(matrixproduct_byname(NA_real_, D), 
               matrix(NA_real_, nrow = 1, ncol = 1) %>% setcoltype("Product"))
  expect_equal(matrixproduct_byname(Z, NA_real_), 
               matrix(NA_real_, nrow = 1, ncol = 1) %>% setrowtype("Product"))
  
  # Try when a is NA and b is a matrix
  Z2 <- matrix(42, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  D2 <- matrix(42, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(matrixproduct_byname(NA, D2),
               matrix(NA_real_, nrow = 2, ncol = 2, dimnames = dimnames(D2)) %>% setcoltype(coltype(D2)))
  expect_equal(matrixproduct_byname(Z2, NA), 
               matrix(NA_real_, nrow = 2, ncol = 2, dimnames = dimnames(Z2)) %>% setrowtype(rowtype(Z2)))
})


test_that("matrixproduct_byname() with NA is correct for Matrix objects", {
  # Try when a is NA and b is a Matrix
  Z2 <- matsbyname::Matrix(42, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  D2 <- matsbyname::Matrix(42, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("Industry") %>% setcoltype("Product")
  expect_equal(matrixproduct_byname(NA, D2),
               matsbyname::Matrix(NA_real_, nrow = 2, ncol = 2, dimnames = dimnames(D2)) %>% setcoltype(coltype(D2)))
  expect_equal(matrixproduct_byname(Z2, NA), 
               matsbyname::Matrix(NA_real_, nrow = 2, ncol = 2, dimnames = dimnames(Z2)) %>% setrowtype(rowtype(Z2)))
})