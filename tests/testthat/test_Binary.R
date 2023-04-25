# Contains tests for binary functions in the byname package.

# Define some matrices with product and industry names and types
# These matrices will be used in the tests below.

test_that("hadamardproduct_byname() works as expected", {
  expect_equal(hadamardproduct_byname(2, 2), 4)
  expect_equal(hadamardproduct_byname(2, 2, 2), 8)
  expect_equal(hadamardproduct_byname(matrix(c(10, 10), nrow = 2, ncol = 1), 1000), 
               matrix(c(10000, 10000), nrow = 2, ncol = 1))
  expect_equal(hadamardproduct_byname(matrix(c(10, 10), nrow = 2, ncol = 1), 1000, 10), 
               matrix(c(100000, 100000), nrow = 2, ncol = 1))
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  # Not what is desired, because names aren't aligned
  expect_equal(U * Y, 
               matrix(c(1,4,9,16), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  UY_expected <- matrix(c(4,6,6,4), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(hadamardproduct_byname(U, Y), UY_expected)
  expect_equal(hadamardproduct_byname(U, 0), matrix(c(0,0,0,0), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  # Make sure it works down a list with .summarise = TRUE.
  expect_equal(hadamardproduct_byname(list(U, Y), .summarise = TRUE), list(UY_expected))
  # See if a product of 4 vectors works as expected
  UUYY_expected <- matrix(c(16, 36, 36, 16), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(hadamardproduct_byname(U, U, Y, Y), UUYY_expected)
  
  # Use dimnames(U), because after performing hadamardproduct_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  expect_equal(hadamardproduct_byname(0, Y), matrix(c(0,0,0,0), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(hadamardproduct_byname(list(U, U), list(Y, Y)), list(UY_expected, UY_expected))
  # And it works with data frames 
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(hadamardproduct_byname(DF$U, DF$Y), list(UY_expected, UY_expected))
  DF_expected <- data.frame(U = I(list()), Y = I(list()), elementprods = I(list()), UUYY = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "elementprods"]] <- UY_expected
  DF_expected[[2, "elementprods"]] <- UY_expected
  DF_expected[[1, "UUYY"]] <- UUYY_expected
  DF_expected[[2, "UUYY"]] <- UUYY_expected
  # Because DF_expected$elementprods and DF_expected$UUYY are created with I(list()), 
  # their classes are "AsIs".
  # Because DF$elementprods and DF$UUYY are created from an actual calculation, their classes are NULL.
  # Need to set the class of DF_expected$elementprods 
  # and DF_expected$UUYY to NULL to get a match.
  attr(DF_expected$elementprods, which = "class") <- NULL
  attr(DF_expected$UUYY, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(
    elementprods = hadamardproduct_byname(U, Y), 
    UUYY = hadamardproduct_byname(U, Y, U, Y)
  ), 
  DF_expected)
  # Test with a constant multiplying a column of the DF
  DF_2 <- DF %>% 
    dplyr::mutate(
      c = 10,
      A = hadamardproduct_byname(c, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_2$A[[i]], DF$U[[i]]*10)
  }
  constant <- 20
  DF_3 <- DF %>% 
    dplyr::mutate(
      B = hadamardproduct_byname(constant, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_3$B[[i]], DF$U[[i]]*20)
  }
  # Try with two constants multiplying a column of the DF.
  DF_3 <- DF_2 %>% 
    dplyr::mutate(
      d = 0.5,
      B = hadamardproduct_byname(c, d, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_3$B[[i]], DF$U[[i]]*10*0.5)
  }
  
  # Try with a list of matrices and a single value.
  Ux2_expected <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2, dimnames = dimnames(DF$U[[1]])) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(hadamardproduct_byname(DF$U, 2), list(Ux2_expected, Ux2_expected))
  # Try with a list of matrices and a single matrix
  expect_equal(hadamardproduct_byname(DF$U, 
                                     matrix(c(2,2,
                                              2,2), 
                                            nrow = 2, ncol = 2, 
                                            dimnames = dimnames(Ux2_expected)) %>% 
                                       setrowtype("Products") %>% setcoltype("Industries")), 
               list(Ux2_expected, Ux2_expected))
})


test_that("hadamardproduct_byname() works with Matrix objects", {
  matsbyname:::expect_equal_matrix_or_Matrix(
    hadamardproduct_byname(matsbyname::Matrix(c(10, 10), nrow = 2, ncol = 1), 1000), 
    matrix(c(10000, 10000), nrow = 2, ncol = 1))
  matsbyname:::expect_equal_matrix_or_Matrix(
    hadamardproduct_byname(matsbyname::Matrix(c(10, 10), nrow = 2, ncol = 1), 1000, 10), 
    matrix(c(100000, 100000), nrow = 2, ncol = 1))
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, 
                          dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  Y <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2,
                          dimnames = list(rev(productnames), rev(industrynames)), 
                          rowtype = "Products", coltype = "Industries")
  # Not what is desired, because names aren't aligned
  res <- U * Y
  expect_true(is.Matrix(res))
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res, 
    matrix(c(1,4,9,16), nrow = 2, dimnames = dimnames(U)))
  UY_expected <- matsbyname::Matrix(c(4,6,6,4), ncol = 2, nrow = 2, dimnames = dimnames(U), 
                                    rowtype = "Products", coltype = "Industries")
  expect_equal(hadamardproduct_byname(U, Y), UY_expected)
  matsbyname:::expect_equal_matrix_or_Matrix(hadamardproduct_byname(U, 0), 
                                             matsbyname::Matrix(c(0,0,0,0), nrow = 2, ncol = 2, dimnames = dimnames(U), 
                                                                rowtype = "Products", coltype = "Industries"))
  # Make sure it works down a list with .summarise = TRUE.
  expect_equal(hadamardproduct_byname(list(U, Y), .summarise = TRUE), list(UY_expected))
  # See if a product of 4 vectors works as expected
  UUYY_expected <- matsbyname::Matrix(c(16, 36, 36, 16), nrow = 2, ncol = 2, 
                                      dimnames = dimnames(U), 
                                      rowtype = "Products", coltype = "Industries")
  expect_equal(hadamardproduct_byname(U, U, Y, Y), UUYY_expected)
  
  # Use dimnames(U), because after performing hadamardproduct_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  matsbyname:::expect_equal_matrix_or_Matrix(
    hadamardproduct_byname(0, Y), 
    matrix(c(0,0,0,0), nrow = 2, ncol = 2, 
           dimnames = dimnames(U)) %>% 
      setrowtype("Products") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(hadamardproduct_byname(list(U, U), list(Y, Y)), list(UY_expected, UY_expected))
  # And it works with data frames 
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(hadamardproduct_byname(DF$U, DF$Y), list(UY_expected, UY_expected))
  DF_expected <- data.frame(U = I(list()), Y = I(list()), elementprods = I(list()), UUYY = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "elementprods"]] <- UY_expected
  DF_expected[[2, "elementprods"]] <- UY_expected
  DF_expected[[1, "UUYY"]] <- UUYY_expected
  DF_expected[[2, "UUYY"]] <- UUYY_expected
  # Because DF_expected$elementprods and DF_expected$UUYY are created with I(list()), 
  # their classes are "AsIs".
  # Because DF$elementprods and DF$UUYY are created from an actual calculation, their classes are NULL.
  # Need to set the class of DF_expected$elementprods 
  # and DF_expected$UUYY to NULL to get a match.
  attr(DF_expected$elementprods, which = "class") <- NULL
  attr(DF_expected$UUYY, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(
    elementprods = hadamardproduct_byname(U, Y), 
    UUYY = hadamardproduct_byname(U, Y, U, Y)
  ), 
  DF_expected)
  # Test with a constant multiplying a column of the DF
  DF_2 <- DF %>% 
    dplyr::mutate(
      c = 10,
      A = hadamardproduct_byname(c, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_2$A[[i]], DF$U[[i]]*10)
  }
  constant <- 20
  DF_3 <- DF %>% 
    dplyr::mutate(
      B = hadamardproduct_byname(constant, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_3$B[[i]], DF$U[[i]]*20)
  }
  # Try with two constants multiplying a column of the DF.
  DF_3 <- DF_2 %>% 
    dplyr::mutate(
      d = 0.5,
      B = hadamardproduct_byname(c, d, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_3$B[[i]], DF$U[[i]]*10*0.5)
  }
  
  # Try with a list of matrices and a single value.
  Ux2_expected <- matsbyname::Matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2, 
                                     dimnames = dimnames(DF$U[[1]])) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(hadamardproduct_byname(DF$U, 2), list(Ux2_expected, Ux2_expected))
  # Try with a list of matrices and a single matrix
  expect_equal(hadamardproduct_byname(DF$U, 
                                      matsbyname::Matrix(c(2,2,
                                                           2,2), 
                                                         nrow = 2, ncol = 2, 
                                                         dimnames = dimnames(Ux2_expected)) %>% 
                                        setrowtype("Products") %>% setcoltype("Industries")), 
               list(Ux2_expected, Ux2_expected))
})


test_that("quotient_byname() works as expected", {
  expect_equal(quotient_byname(100, 50), 2)
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  # Non-sensical.  Names aren't aligned
  expect_equal(U/Y, 
               matrix(c(0.25, 2/3, 1.5, 4), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  UoverY_expected <- matrix(c(1,1,1,1), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(U, Y), UoverY_expected)
  expect_equal(quotient_byname(U, 10), 
               matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  tenoverY_expected <- matrix(c(10, 5, 10/3, 2.5), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(10, Y), tenoverY_expected)
  # This also works with lists
  expect_equal(quotient_byname(10, list(Y,Y)), list(tenoverY_expected, tenoverY_expected))
  # Try more-complicated lists
  expect_equal(quotient_byname(list(10, 10, 10), list(Y, Y, Y)), 
               list(tenoverY_expected, tenoverY_expected, tenoverY_expected))
  mat12 <- matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  mat34 <- matrix(c(3, 4), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(quotient_byname(list(mat12, mat34), list(2, 4)), 
               list(mat12 / 2, mat34 / 4))
  
  # Use dimnames(U), because after performing quotient_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  Yover10_expected <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(list(Y,Y), 10), list(Yover10_expected, Yover10_expected))
  expect_equal(quotient_byname(list(U, U), list(Y, Y)), list(UoverY_expected, UoverY_expected))
  # Also works with data frames.
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(quotient_byname(DF$U, DF$Y), list(UoverY_expected, UoverY_expected))
  DF_expected <- data.frame(U = I(list()), Y = I(list()), elementquotients = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "elementquotients"]] <- UoverY_expected
  DF_expected[[2, "elementquotients"]] <- UoverY_expected
  # Because DF_expected$elementquotients is created with I(list()), its class is "AsIs".
  # Because DF$elementquotients is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$elementquotients to NULL to get a match.
  attr(DF_expected$elementquotients, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(elementquotients = quotient_byname(U, Y)), DF_expected)
})


test_that("quotient_byname() works with Matrix objects", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matsbyname::Matrix(rev(1:4), nrow = 2, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  # Non-sensical.  Names aren't aligned
  expect_equal(U/Y, 
               matsbyname::Matrix(c(0.25, 2/3, 1.5, 4), nrow = 2, ncol = 2, 
                                  dimnames = dimnames(U)))
  UoverY_expected <- matsbyname::Matrix(c(1,1,1,1), nrow = 2, ncol = 2, 
                                        dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(U, Y), UoverY_expected)
  expect_equal(quotient_byname(U, 10), 
               matsbyname::Matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2,
                                  dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  tenoverY_expected <- matsbyname::Matrix(c(10, 5, 10/3, 2.5), nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(10, Y), tenoverY_expected)
  # This also works with lists
  expect_equal(quotient_byname(10, list(Y,Y)), list(tenoverY_expected, tenoverY_expected))
  # Try more-complicated lists
  expect_equal(quotient_byname(list(10, 10, 10), list(Y, Y, Y)), 
               list(tenoverY_expected, tenoverY_expected, tenoverY_expected))
  mat12 <- matsbyname::Matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  mat34 <- matsbyname::Matrix(c(3, 4), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(quotient_byname(list(mat12, mat34), list(2, 4)), 
               list(mat12 / 2, mat34 / 4))
  
  # Use dimnames(U), because after performing quotient_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  Yover10_expected <- matsbyname::Matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, ncol = 2, 
                                         dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(quotient_byname(list(Y,Y), 10), list(Yover10_expected, Yover10_expected))
  expect_equal(quotient_byname(list(U, U), list(Y, Y)), list(UoverY_expected, UoverY_expected))
  # Also works with data frames.
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(quotient_byname(DF$U, DF$Y), list(UoverY_expected, UoverY_expected))
  DF_expected <- data.frame(U = I(list()), Y = I(list()), elementquotients = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "elementquotients"]] <- UoverY_expected
  DF_expected[[2, "elementquotients"]] <- UoverY_expected
  # Because DF_expected$elementquotients is created with I(list()), its class is "AsIs".
  # Because DF$elementquotients is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$elementquotients to NULL to get a match.
  attr(DF_expected$elementquotients, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(elementquotients = quotient_byname(U, Y)), DF_expected)
})


test_that("quotient_byname() detailed example works as expected", {
  Lv <- list(
    matrix(c(36.40956907, 
             86.56170245), nrow = 2, ncol = 1), 
    matrix(c(61.97848865, 
             145.7748236), nrow = 2, ncol = 1),
    matrix(c(56.71228867,
             226.8281467), nrow = 2, ncol = 1)) %>% 
    setrownames_byname(c("subcat 1", "subcat 2")) %>% setcolnames_byname("factor") %>% 
    setrowtype("subcat") %>% setcoltype("factor")
  LV <- list(123.3151731, 208.1079558, 285.6464036)
  expected <- list(matrix(c(0.295256197,
                            0.701955001), nrow = 2, ncol = 1), 
                   matrix(c(0.29781893, 
                            0.700476938), nrow = 2, ncol = 1),
                   matrix(c(0.198540181, 
                            0.794087179), nrow = 2, ncol = 1)) %>% 
    setrownames_byname(c("subcat 1", "subcat 2")) %>% setcolnames_byname("factor") %>% 
    setrowtype("subcat") %>% setcoltype("factor")
  expect_equal(quotient_byname(Lv, LV), expected)
  
  # This is the failure mode.
  # Somehow, LV is not maintained as a list. 
  # It comes in as a numeric vector.
  # Then, organize_args turns it into a funky list.
  LVnumeric <- c(123.3151731, 208.1079558, 285.6464036)
  expect_equal(quotient_byname(Lv, LVnumeric), expected)
  
  # Now try these in a data frame
  DF <- data.frame(Lv = I(list()), LV = I(list()))
  DF[[1,"Lv"]] <- Lv[[1]]
  DF[[2,"Lv"]] <- Lv[[2]]
  DF[[3,"Lv"]] <- Lv[[3]]
  
  DF[[1,"LV"]] <- LV[[1]]
  DF[[2,"LV"]] <- LV[[2]]
  DF[[3,"LV"]] <- LV[[3]]
  DF2 <- DF %>% 
    dplyr::mutate(
      wv = quotient_byname(Lv, LV)
    )
  expect_equal(DF2$wv, expected)
})


test_that("quotient_byname() detailed example works as expected with Matrix objects", {
  Lv <- list(
    matsbyname::Matrix(c(36.40956907, 
                         86.56170245), nrow = 2, ncol = 1), 
    matsbyname::Matrix(c(61.97848865, 
                         145.7748236), nrow = 2, ncol = 1),
    matsbyname::Matrix(c(56.71228867,
                         226.8281467), nrow = 2, ncol = 1)) %>% 
    setrownames_byname(c("subcat 1", "subcat 2")) %>% setcolnames_byname("factor") %>% 
    setrowtype("subcat") %>% setcoltype("factor")
  LV <- list(123.3151731, 208.1079558, 285.6464036)
  expected <- list(matsbyname::Matrix(c(0.295256197,
                                        0.701955001), nrow = 2, ncol = 1), 
                   matsbyname::Matrix(c(0.29781893, 
                                        0.700476938), nrow = 2, ncol = 1),
                   matsbyname::Matrix(c(0.198540181, 
                                        0.794087179), nrow = 2, ncol = 1)) %>% 
    setrownames_byname(c("subcat 1", "subcat 2")) %>% setcolnames_byname("factor") %>% 
    setrowtype("subcat") %>% setcoltype("factor")
  expect_equal(quotient_byname(Lv, LV), expected)
  
  # This is the failure mode.
  # Somehow, LV is not maintained as a list. 
  # It comes in as a numeric vector.
  # Then, organize_args turns it into a funky list.
  LVnumeric <- c(123.3151731, 208.1079558, 285.6464036)
  expect_equal(quotient_byname(Lv, LVnumeric), expected)
  
  # Now try these in a data frame
  DF <- data.frame(Lv = I(list()), LV = I(list()))
  DF[[1,"Lv"]] <- Lv[[1]]
  DF[[2,"Lv"]] <- Lv[[2]]
  DF[[3,"Lv"]] <- Lv[[3]]
  
  DF[[1,"LV"]] <- LV[[1]]
  DF[[2,"LV"]] <- LV[[2]]
  DF[[3,"LV"]] <- LV[[3]]
  DF2 <- DF %>% 
    dplyr::mutate(
      wv = quotient_byname(Lv, LV)
    )
  expect_equal(DF2$wv, expected)
})


test_that("pow_byname() works as expected", {
  # Try with single numbers
  expect_equal(pow_byname(2, 2), 4)
  expect_equal(pow_byname(2, 3), 8)
  expect_equal(pow_byname(-1, 3), -1)
  expect_equal(pow_byname(-1, 4), 1)
  expect_equal(pow_byname(-1000, 0), 1)
  expect_equal(pow_byname(0, 500), 0)
  expect_equal(pow_byname(2, -1), 0.5)
  
  # Try with single matrices
  m <- matrix(2, nrow = 2, ncol = 3)
  one_over_m <- matrix(0.5, nrow = 2, ncol = 3)
  sqrtm <- matrix(sqrt(2), nrow = 2, ncol = 3)
  identity <- matrix(1, nrow = 2, ncol = 3)
  squarem <- matrix(4, nrow = 2, ncol = 3)
  
  expect_equal(pow_byname(m, -1), one_over_m)
  expect_equal(pow_byname(m, 0), identity)
  expect_equal(pow_byname(m, 0.5), sqrtm)
  expect_equal(pow_byname(m, 0), identity)
  expect_equal(pow_byname(m, 2), squarem)
  
  # Try with a list of matrices
  expect_equal(pow_byname(list(m, m), 0.5), list(sqrtm, sqrtm))
  expect_equal(pow_byname(list(m, m), pow = list(0.5, 1)), list(sqrtm, m))
  expect_equal(pow_byname(list(m, m, m, m, m), pow = list(-1, 0, 0.5, 1, 2)), list(one_over_m, identity, sqrtm, m, squarem))
  
  # Try in a data frame
  DF <- data.frame(m = I(list()), pow = I(list()))
  DF[[1, "m"]] <- m
  DF[[2, "m"]] <- m
  DF[[1, "pow"]] <- 0.5
  DF[[2, "pow"]] <- -1
  res <- DF %>% dplyr::mutate(
    sqrtm = pow_byname(m, 0.5),
    mtopow = pow_byname(m, pow)
  )
  expect_equal(res$sqrtm, list(sqrtm, sqrtm))
  expect_equal(res$mtopow, list(m^0.5, m^-1))
})


test_that("pow_byname() works with Mattrix objects", {
  # Try with single matrices
  m <- matsbyname::Matrix(2, nrow = 2, ncol = 3)
  one_over_m <- matsbyname::Matrix(0.5, nrow = 2, ncol = 3)
  sqrtm <- matsbyname::Matrix(sqrt(2), nrow = 2, ncol = 3)
  identity <- matsbyname::Matrix(1, nrow = 2, ncol = 3)
  squarem <- matsbyname::Matrix(4, nrow = 2, ncol = 3)
  
  expect_equal(pow_byname(m, -1), one_over_m)
  expect_equal(pow_byname(m, 0), identity)
  expect_equal(pow_byname(m, 0.5), sqrtm)
  expect_equal(pow_byname(m, 0), identity)
  expect_equal(pow_byname(m, 2), squarem)
  
  # Try with a list of matrices
  expect_equal(pow_byname(list(m, m), 0.5), list(sqrtm, sqrtm))
  expect_equal(pow_byname(list(m, m), pow = list(0.5, 1)), list(sqrtm, m))
  expect_equal(pow_byname(list(m, m, m, m, m), pow = list(-1, 0, 0.5, 1, 2)), list(one_over_m, identity, sqrtm, m, squarem))
  
  # Try in a data frame
  DF <- data.frame(m = I(list()), pow = I(list()))
  DF[[1, "m"]] <- m
  DF[[2, "m"]] <- m
  DF[[1, "pow"]] <- 0.5
  DF[[2, "pow"]] <- -1
  res <- DF %>% dplyr::mutate(
    sqrtm = pow_byname(m, 0.5),
    mtopow = pow_byname(m, pow)
  )
  expect_equal(res$sqrtm, list(sqrtm, sqrtm))
  expect_equal(res$mtopow, list(m^0.5, m^-1))
})


test_that("mean_byname() works as expected", {
  expect_equal(mean_byname(100, 50), 75)
  expect_equal(mean_byname(0, 0), 0)
  expect_equal(mean_byname(-2, -4), -3)
  expect_equal(mean_byname(-10, 10), 0)
  expect_equal(mean_byname(1,2,3), 2)
  commoditynames <- c("c1", "c2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGavg <- matrix(1:4, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  # Non-sensical. Row and column names not respected.
  expect_equal((U + G) / 2, 
               matrix(2.5, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # Row and column names respected! Should be 1, 2, 3, and 4.
  expect_equal(mean_byname(U, G), UGavg)
  expect_equal(mean_byname(100, U), 
               matrix((100 + 1:4)/2, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  expect_equal(mean_byname(10, G), 
               matrix((10 + 1:4)/2, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  A <- matrix(1:4, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  B <- 2*A
  C <- 2*B
  ABCavg_expected <- matrix(c((1 + 2 + 4), (3 + 6 + 12), 
                              (2 + 4 + 8), (4 + 8 + 16)) / 3, 
                            byrow = TRUE, nrow = 2, dimnames = dimnames(A)) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(mean_byname(A, B, C), ABCavg_expected)
  
  # This also works with lists
  expect_equal(mean_byname(list(100, 100), list(50, 50)), list(75, 75))
  expect_equal(mean_byname(list(100, 100), list(50, 50), list(75, 75), .summarise = TRUE), list(100, 50, 75))
  expect_equal(mean_byname(list(U,U), list(G,G)), list(UGavg, UGavg))
  expect_equal(mean_byname(list(A,A), list(B,B), list(C,C)), list(ABCavg_expected, ABCavg_expected))
  DF <- data.frame(U = I(list()), G = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"G"]] <- G
  DF[[2,"G"]] <- G
  expect_equal(mean_byname(DF$U, DF$G), list(UGavg, UGavg))
  DF_expected <- data.frame(U = I(list()), G = I(list()), means = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "G"]] <- G
  DF_expected[[2, "G"]] <- G
  DF_expected[[1, "means"]] <- UGavg
  DF_expected[[2, "means"]] <- UGavg
  # Because DF_expected$means is created with I(list()), its class is "AsIs".
  # Because DF$means is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$means to NULL to get a match.
  attr(DF_expected$means, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(means = mean_byname(U, G)), DF_expected)
})


test_that("mean_byname() works with Matrix objects", {
  commoditynames <- c("c1", "c2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  G <- matsbyname::Matrix(rev(1:4), nrow = 2, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGavg <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  # Non-sensical. Row and column names not respected.
  expect_equal((U + G) / 2, 
               matsbyname::Matrix(2.5, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)))
  # Row and column names respected! Should be 1, 2, 3, and 4.
  expect_equal(mean_byname(U, G), UGavg)
  expect_equal(mean_byname(100, U), 
               matsbyname::Matrix((100 + 1:4)/2, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  expect_equal(mean_byname(10, G), 
               matsbyname::Matrix((10 + 1:4)/2, nrow = 2, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  A <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  B <- 2*A
  C <- 2*B
  ABCavg_expected <- matsbyname::Matrix(c((1 + 2 + 4), (3 + 6 + 12), 
                              (2 + 4 + 8), (4 + 8 + 16)) / 3, 
                            byrow = TRUE, nrow = 2, ncol = 2, dimnames = dimnames(A)) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(mean_byname(A, B, C), ABCavg_expected)
  
  # This also works with lists
  expect_equal(mean_byname(list(U,U), list(G,G)), list(UGavg, UGavg))
  expect_equal(mean_byname(list(A,A), list(B,B), list(C,C)), list(ABCavg_expected, ABCavg_expected))
  DF <- data.frame(U = I(list()), G = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"G"]] <- G
  DF[[2,"G"]] <- G
  expect_equal(mean_byname(DF$U, DF$G), list(UGavg, UGavg))
  DF_expected <- data.frame(U = I(list()), G = I(list()), means = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "G"]] <- G
  DF_expected[[2, "G"]] <- G
  DF_expected[[1, "means"]] <- UGavg
  DF_expected[[2, "means"]] <- UGavg
  # Because DF_expected$means is created with I(list()), its class is "AsIs".
  # Because DF$means is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$means to NULL to get a match.
  attr(DF_expected$means, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(means = mean_byname(U, G)), DF_expected)
})


test_that("geometricmean_byname() works as expected", {
  expect_equal(geometricmean_byname(0, 0), 0)
  expect_equal(geometricmean_byname(10, 20), sqrt(10*20))
  expect_equal(geometricmean_byname(10, 20, 30), (10*20*30)^(1/3))
  expect_true(is.nan(geometricmean_byname(-10, 10)))
  expect_equal(geometricmean_byname(10, 1000), 100)
  expect_equal(geometricmean_byname(a = matrix(c(10, 10), nrow = 2, ncol = 1), b = 1000), 
               matrix(c(100, 100), nrow = 2, ncol = 1))
  expect_equal(geometricmean_byname(a = 1000, b = matrix(c(10, 10), nrow = 2, ncol = 1)), 
               matrix(c(100, 100), nrow = 2, ncol = 1))
  
  commoditynames <- c("c1", "c2")
  industrynames <- "i1"
  U <- matrix(c(10, 1000), ncol = 1, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  G <- matrix(c(1e3, 1e5), ncol = 1, nrow = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGgeomean <- matrix(c(1000, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGGgeomean <- matrix(c(10*1e5*1e5, 1000*1e3*1e3), nrow = 2, ncol = 1,
                       dimnames = list(commoditynames, industrynames))^(1/3) %>% 
    setrowtype("Commodities") %>% setcoltype("Industries")
  # Non-sensical. Row and column names not respected.
  expect_equal(sqrt(U*G), 
               matrix(c(100, 10000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # Row and column names respected!
  expect_equal(geometricmean_byname(U, G), UGgeomean)
  expect_equal(geometricmean_byname(U, G, G), UGGgeomean)
  expect_equal(geometricmean_byname(1000, U), 
               matrix(c(100, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  expect_equal(geometricmean_byname(10, G), 
               matrix(c(1000, 100), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(geometricmean_byname(list(10, 1000), list(1000, 10)), list(100, 100))
  expect_equal(geometricmean_byname(list(1, 1000), list(10, 10), .summarise = TRUE), list(31.6227766, 10))
  expect_equal(geometricmean_byname(list(U,U), list(G,G)), list(UGgeomean, UGgeomean))
  DF <- data.frame(U = I(list()), G = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"G"]] <- G
  DF[[2,"G"]] <- G
  expect_equal(geometricmean_byname(DF$U, DF$G), list(UGgeomean, UGgeomean))
  DF_expected <- data.frame(U = I(list()), G = I(list()), geomeans = I(list()), UGGgeomean = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "G"]] <- G
  DF_expected[[2, "G"]] <- G
  DF_expected[[1, "geomeans"]] <- UGgeomean
  DF_expected[[2, "geomeans"]] <- UGgeomean
  DF_expected[[1, "UGGgeomean"]] <- UGGgeomean
  DF_expected[[2, "UGGgeomean"]] <- UGGgeomean
  # Because DF_expected$geomeans is created with I(list()), its class is "AsIs".
  # Because DF$geomeans is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$geomeans to NULL to get a match.
  attr(DF_expected$geomeans, which = "class") <- NULL
  attr(DF_expected$UGGgeomean, which = "class") <- NULL
  expect_equal(DF %>% 
                 dplyr::mutate(
                   geomeans = geometricmean_byname(U, G), 
                   UGGgeomean = geometricmean_byname(U, G, G)
                 ), 
               DF_expected
  )
})


test_that("geometricmean_byname() works with Matrix objects", {
  expect_equal(geometricmean_byname(a = matsbyname::Matrix(c(10, 10), nrow = 2, ncol = 1), b = 1000), 
               matsbyname::Matrix(c(100, 100), nrow = 2, ncol = 1))
  expect_equal(geometricmean_byname(a = 1000, b = matsbyname::Matrix(c(10, 10), nrow = 2, ncol = 1)), 
               matsbyname::Matrix(c(100, 100), nrow = 2, ncol = 1))
  
  commoditynames <- c("c1", "c2")
  industrynames <- "i1"
  U <- matsbyname::Matrix(c(10, 1000), ncol = 1, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  G <- matsbyname::Matrix(c(1e3, 1e5), ncol = 1, nrow = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGgeomean <- matsbyname::Matrix(c(1000, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGGgeomean <- matsbyname::Matrix(c(10*1e5*1e5, 1000*1e3*1e3), nrow = 2, ncol = 1,
                       dimnames = list(commoditynames, industrynames))^(1/3) %>% 
    setrowtype("Commodities") %>% setcoltype("Industries")
  # Non-sensical. Row and column names not respected.
  expect_equal(sqrt(U*G), 
               matsbyname::Matrix(c(100, 10000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)))
  # Row and column names respected!
  expect_equal(geometricmean_byname(U, G), UGgeomean)
  expect_equal(geometricmean_byname(U, G, G), UGGgeomean)
  expect_equal(geometricmean_byname(1000, U), 
               matsbyname::Matrix(c(100, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  expect_equal(geometricmean_byname(10, G), 
               matsbyname::Matrix(c(1000, 100), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(geometricmean_byname(list(U,U), list(G,G)), list(UGgeomean, UGgeomean))
  DF <- data.frame(U = I(list()), G = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"G"]] <- G
  DF[[2,"G"]] <- G
  expect_equal(geometricmean_byname(DF$U, DF$G), list(UGgeomean, UGgeomean))
  DF_expected <- data.frame(U = I(list()), G = I(list()), geomeans = I(list()), UGGgeomean = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "G"]] <- G
  DF_expected[[2, "G"]] <- G
  DF_expected[[1, "geomeans"]] <- UGgeomean
  DF_expected[[2, "geomeans"]] <- UGgeomean
  DF_expected[[1, "UGGgeomean"]] <- UGGgeomean
  DF_expected[[2, "UGGgeomean"]] <- UGGgeomean
  # Because DF_expected$geomeans is created with I(list()), its class is "AsIs".
  # Because DF$geomeans is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$geomeans to NULL to get a match.
  attr(DF_expected$geomeans, which = "class") <- NULL
  attr(DF_expected$UGGgeomean, which = "class") <- NULL
  expect_equal(DF %>% 
                 dplyr::mutate(
                   geomeans = geometricmean_byname(U, G), 
                   UGGgeomean = geometricmean_byname(U, G, G)
                 ), 
               DF_expected
  )
})


test_that("logmean() works as expected", {
  # The logmean function is an internal helper function that should also be tested.
  expect_equal(logmean(0, 0), 0)
  expect_equal(logmean(0, 1), 0)
  expect_equal(logmean(1, 0), 0)
  expect_equal(logmean(1, 1), 1)
  expect_equal(logmean(2, 1), 1.442695041)
  # commutative!
  expect_equal(logmean(1, 2), 1.442695041)
  # base = exp(1), the default
  expect_equal(logmean(1, 10), 3.908650337)
  expect_equal(logmean(1, 10, base = 10), 9)
  # Try negative numbers. 
  # These work, because the denominator is implemented as a ratio.
  expect_equal(logmean(-1, -2), -1.442695041)
  expect_equal(logmean(-2, -1), -1.442695041)
  # These fail, because the denominator will be negative.
  expect_warning(val1 <- logmean(-1, 2), "NaNs produced")
  expect_true(is.nan(val1))
  expect_warning(val2 <- logmean(1, -2), "NaNs produced")
  expect_true(is.nan(val2))
})


test_that("logarithmicmean_byname() works as expected", {
  # Should work with single numbers.
  expect_equal(logarithmicmean_byname(0, 0), 0)
  expect_equal(logarithmicmean_byname(0, 1), 0)
  expect_equal(logarithmicmean_byname(1, 0), 0)
  expect_equal(logarithmicmean_byname(1, 1), 1)
  expect_equal(logarithmicmean_byname(2, 1), 1.442695041)
  # commutative!
  expect_equal(logarithmicmean_byname(1, 2), 1.442695041)
  # base = exp(1), the default
  expect_equal(logarithmicmean_byname(1, 10), 3.908650337)
  expect_equal(logarithmicmean_byname(1, 10, base = 10), 9)
  # Try with a matrix and a constant.
  m1 <- matrix(c(1:6), nrow = 3, ncol = 2) %>% 
    setrownames_byname(c("r1", "r2", "r3")) %>% setcolnames_byname(c("c1", "c2")) %>% 
    setrowtype("row") %>% setcoltype("col")
  expected_lmm12 <- matrix(c(1.442695041, 2.885390082, 
                             2, 3.274070004, 
                             2.466303462, 3.640956907), byrow = TRUE,
                           nrow = 3, ncol = 2, dimnames = dimnames(m1)) %>% 
    setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1))
  expect_equal(logarithmicmean_byname(m1, 2), expected_lmm12)
  expect_equal(logarithmicmean_byname(2, m1), expected_lmm12) 
  # Try with a matrix and a constant in lists
  expect_equal(logarithmicmean_byname(list(m1, m1), list(2, 2)), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(list(2, 2), list(m1, m1)), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(list(m1, m1), 2), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(2, list(m1, m1)), list(expected_lmm12, expected_lmm12))
  
  # Try with two matrices
  m2 <- matrix(c(7:12), nrow = 3, ncol = 2) %>% 
    setrownames_byname(c("r2", "r3", "r4")) %>% setcolnames_byname(c("c2", "c3")) %>% 
    setrowtype("row") %>% setcoltype("col")
  logmean <- logarithmicmean_byname(m1, m2)
  expectedlm <- matrix(c(0, 0, 0, 
                         0, 5.944026824, 0,
                         0, 6.952118994, 0,
                         0, 0, 0), nrow = 4, ncol = 3, byrow = TRUE) %>% 
    setrownames_byname(c("r1", "r2", "r3", "r4")) %>% setcolnames_byname(c("c1", "c2", "c3")) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(logmean, expectedlm)
  # This also works with lists
  expect_equal(logarithmicmean_byname(list(m1, m1), list(m2, m2)), list(expectedlm, expectedlm))
  DF <- data.frame(m1 = I(list()), m2 = I(list()))
  DF[[1,"m1"]] <- m1
  DF[[2,"m1"]] <- m1
  DF[[1,"m2"]] <- m2
  DF[[2,"m2"]] <- m2
  expect_equal(logarithmicmean_byname(DF$m1, DF$m2), list(expectedlm, expectedlm))
  DF_expected <- data.frame(m1 = I(list()), m2 = I(list()), logmeans = I(list()))
  DF_expected[[1, "m1"]] <- m1
  DF_expected[[2, "m1"]] <- m1
  DF_expected[[1, "m2"]] <- m2
  DF_expected[[2, "m2"]] <- m2
  DF_expected[[1, "logmeans"]] <- logmean
  DF_expected[[2, "logmeans"]] <- logmean
  # Because DF_expected$geomeans is created with I(list()), its class is "AsIs".
  # Because DF$geomeans is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$geomeans to NULL to get a match.
  attr(DF_expected$logmeans, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(logmeans = logarithmicmean_byname(m1, m2)), DF_expected)
})


test_that("logarithmicmean_byname() works wtih Matrix objects", {
  # Try with a matrix and a constant.
  m1 <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2) %>% 
    setrownames_byname(c("r1", "r2", "r3")) %>% setcolnames_byname(c("c1", "c2")) %>% 
    setrowtype("row") %>% setcoltype("col")
  expected_lmm12 <- matsbyname::Matrix(c(1.442695041, 2.885390082, 
                                         2, 3.274070004, 
                                         2.466303462, 3.640956907), byrow = TRUE,
                                       nrow = 3, ncol = 2, dimnames = dimnames(m1)) %>% 
    setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1))
  expect_equal(logarithmicmean_byname(m1, 2), expected_lmm12)
  expect_equal(logarithmicmean_byname(2, m1), expected_lmm12) 
  # Try with a matrix and a constant in lists
  expect_equal(logarithmicmean_byname(list(m1, m1), list(2, 2)), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(list(2, 2), list(m1, m1)), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(list(m1, m1), 2), list(expected_lmm12, expected_lmm12))
  expect_equal(logarithmicmean_byname(2, list(m1, m1)), list(expected_lmm12, expected_lmm12))
  
  # Try with two matrices
  m2 <- matsbyname::Matrix(c(7:12), nrow = 3, ncol = 2) %>% 
    setrownames_byname(c("r2", "r3", "r4")) %>% setcolnames_byname(c("c2", "c3")) %>% 
    setrowtype("row") %>% setcoltype("col")
  logmean <- logarithmicmean_byname(m1, m2)
  expectedlm <- matsbyname::Matrix(c(0, 0, 0, 
                                     0, 5.944026824, 0,
                                     0, 6.952118994, 0,
                                     0, 0, 0), nrow = 4, ncol = 3, byrow = TRUE) %>% 
    setrownames_byname(c("r1", "r2", "r3", "r4")) %>% setcolnames_byname(c("c1", "c2", "c3")) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(logmean, expectedlm)
  # This also works with lists
  expect_equal(logarithmicmean_byname(list(m1, m1), list(m2, m2)), list(expectedlm, expectedlm))
  DF <- data.frame(m1 = I(list()), m2 = I(list()))
  DF[[1,"m1"]] <- m1
  DF[[2,"m1"]] <- m1
  DF[[1,"m2"]] <- m2
  DF[[2,"m2"]] <- m2
  expect_equal(logarithmicmean_byname(DF$m1, DF$m2), list(expectedlm, expectedlm))
  DF_expected <- data.frame(m1 = I(list()), m2 = I(list()), logmeans = I(list()))
  DF_expected[[1, "m1"]] <- m1
  DF_expected[[2, "m1"]] <- m1
  DF_expected[[1, "m2"]] <- m2
  DF_expected[[2, "m2"]] <- m2
  DF_expected[[1, "logmeans"]] <- logmean
  DF_expected[[2, "logmeans"]] <- logmean
  # Because DF_expected$geomeans is created with I(list()), its class is "AsIs".
  # Because DF$geomeans is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$geomeans to NULL to get a match.
  attr(DF_expected$logmeans, which = "class") <- NULL
  expect_equal(DF %>% dplyr::mutate(logmeans = logarithmicmean_byname(m1, m2)), DF_expected)
})


test_that("equal_byname() works as expected", {
  
  # Try with single numbers
  expect_true(equal_byname(2, 2))
  expect_true(equal_byname(2, 2, 2))
  expect_false(equal_byname(2, 3, 5))
  expect_false(equal_byname(2, 2, 5))
  
  # Try without row and column names
  a <- matrix(1:4, nrow = 2)
  expect_true(equal_byname(a, a))
  expect_true(equal_byname(a, a, a))
  b <- matrix(4:1, nrow = 2)
  expect_true(equal_byname(b, b, b))
  expect_false(equal_byname(a, b))
  expect_false(equal_byname(a, a, b))
  expect_false(equal_byname(b, a, a))
  b <- matrix(1:4, nrow = 2)
  expect_true(equal_byname(a, b))
  expect_true(equal_byname(a, a, b))
  expect_true(equal_byname(b, a, a, b))
  
  a <- a %>% setrowtype("Industries") %>% setcoltype("Products")
  # FALSE because a has row and column types, but b does not.
  expect_false(equal_byname(a, b))
  
  b <- b %>% setrowtype("Industries") %>% setcoltype("Products")
  # TRUE because b now has same row and column types as a.
  expect_true(equal_byname(a, b))
  
  dimnames(a) <- list(c("i1", "i2"), c("p1", "p2"))
  dimnames(b) <- list(c("p1", "p2"), c("i1", "i2"))
  # FALSE, because row and column names are not equal
  expect_false(equal_byname(a, b)) 
  # Put back the way it was, and it should work.
  dimnames(b) <- dimnames(a)
  expect_true(equal_byname(a, b))
  
  # Try with lists.
  expect_equal(equal_byname(list(2, 3), list(2, 3), list(42, 42)), list(FALSE, FALSE))
  expect_equal(equal_byname(list(2, 3), list(2, 3), list(2, 3)), list(TRUE, TRUE))
  expect_equal(equal_byname(list(2, 2), list(3, 3), list(42, 42), .summarise = TRUE), list(TRUE, TRUE, TRUE))
  expect_equal(equal_byname(list(2, 2), list(3, 4), list(42, 42), .summarise = TRUE), list(TRUE, FALSE, TRUE))
  
  expect_equal(equal_byname(list(a, a), list(b, b)), list(TRUE, TRUE))
  expect_equal(equal_byname(list(a, a), list(b, b), .summarise = TRUE), list(TRUE, TRUE))
  
  # Try with two unsorted matrices. They should be equal (byname), 
  # because they will be sorted prior to comparison.
  matc <- matrix(c(1, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1")))
  matd <- matrix(c(2, 1), nrow = 2, dimnames = list(c("r2", "r1"), c("c1")))
  # This is not what we want. Comparison is not done "byname", as we wish.
  expect_equal(matc == matd, matrix(c(FALSE, FALSE), nrow = 2, dimnames = list(c("r1", "r2"), c("c1"))))
  # This works as desired. The comparison is handled by the function, not the analyst.
  expect_true(equal_byname(matc, matd))
  expect_true(equal_byname(matc, matc, matd))
  expect_equal(equal_byname(list(matc, matc), list(matd, matd), list(matc, matc)), list(TRUE, TRUE))
  
  # Try within data frames
  DF <- data.frame(matc = I(list()), matd = I(list()))
  DF[[1,"matc"]] <- matc
  DF[[2,"matc"]] <- matc
  DF[[1,"matd"]] <- matd
  DF[[2,"matd"]] <- matd
  DF_2 <- DF %>% 
    dplyr::mutate(
      equal = equal_byname(matc, matd)
    )
  expect_equal(DF_2$equal, list(TRUE, TRUE))
  
  # When two objects have different order for attributes, equal_byname should still return true.
  a <- 2 %>% setrowtype("row") %>% setcoltype("col")
  b <- 2 %>% setcoltype("col") %>% setrowtype("row")
  expect_true(equal_byname(a, b))
  # But if the objects have different attributes, the comparison should fail.
  c <- a %>% setcoltype("cols")
  expect_false(equal_byname(c, b))
  
  # Try with some numerical fuzz.
  e <- matrix(1:4, nrow = 2)
  f <- matrix(1:4, nrow = 2)
  # equal_byname works
  expect_true(equal_byname(e, f))
  expect_true(equal_byname(e, f + 1e-100))
  # But identical_byname should fail
  expect_false(identical_byname(e, f + 1e-100))
})


test_that("equal_byname() works with Matrix objects", {
  
  # Try without row and column names
  a <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  expect_true(equal_byname(a, a))
  expect_true(equal_byname(a, a, a))
  b <- matsbyname::Matrix(4:1, nrow = 2, ncol = 2)
  expect_true(equal_byname(b, b, b))
  expect_false(equal_byname(a, b))
  expect_false(equal_byname(a, a, b))
  expect_false(equal_byname(b, a, a))
  b <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  expect_true(equal_byname(a, b))
  expect_true(equal_byname(a, a, b))
  expect_true(equal_byname(b, a, a, b))
  
  a <- a %>% setrowtype("Industries") %>% setcoltype("Products")
  # FALSE because a has row and column types, but b does not.
  expect_false(equal_byname(a, b))
  
  b <- b %>% setrowtype("Industries") %>% setcoltype("Products")
  # TRUE because b now has same row and column types as a.
  expect_true(equal_byname(a, b))
  
  dimnames(a) <- list(c("i1", "i2"), c("p1", "p2"))
  dimnames(b) <- list(c("p1", "p2"), c("i1", "i2"))
  # FALSE, because row and column names are not equal
  expect_false(equal_byname(a, b)) 
  # Put back the way it was, and it should work.
  dimnames(b) <- dimnames(a)
  expect_true(equal_byname(a, b))
  
  # Try with lists.
  expect_equal(equal_byname(list(a, a), list(b, b)), list(TRUE, TRUE))
  expect_equal(equal_byname(list(a, a), list(b, b), .summarise = TRUE), list(TRUE, TRUE))
  
  # Try with two unsorted matrices. They should be equal (byname), 
  # because they will be sorted prior to comparison.
  matc <- matsbyname::Matrix(c(1, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1")))
  matd <- matsbyname::Matrix(c(2, 1), nrow = 2, dimnames = list(c("r2", "r1"), c("c1")))
  # This is not what we want. Comparison is not done "byname", as we wish.
  matsbyname:::expect_equal_matrix_or_Matrix(
    matc == matd, 
    matsbyname::Matrix(c(FALSE, FALSE), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), c("c1"))))
  # This works as desired. The comparison is handled by the function, not the analyst.
  expect_true(equal_byname(matc, matd))
  expect_true(equal_byname(matc, matc, matd))
  expect_equal(equal_byname(list(matc, matc), list(matd, matd), list(matc, matc)), list(TRUE, TRUE))
  
  # Try within data frames
  DF <- data.frame(matc = I(list()), matd = I(list()))
  DF[[1,"matc"]] <- matc
  DF[[2,"matc"]] <- matc
  DF[[1,"matd"]] <- matd
  DF[[2,"matd"]] <- matd
  DF_2 <- DF %>% 
    dplyr::mutate(
      equal = equal_byname(matc, matd)
    )
  expect_equal(DF_2$equal, list(TRUE, TRUE))
  
  # When two objects have different order for attributes, equal_byname should still return true.
  a <- 2 %>% setrowtype("row") %>% setcoltype("col")
  b <- 2 %>% setcoltype("col") %>% setrowtype("row")
  expect_true(equal_byname(a, b))
  # But if the objects have different attributes, the comparison should fail.
  c <- a %>% setcoltype("cols")
  expect_false(equal_byname(c, b))
  
  # Try with some numerical fuzz.
  e <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  f <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  # equal_byname works
  expect_true(equal_byname(e, f))
  expect_true(equal_byname(e, f + 1e-100))
  # But identical_byname should fail
  # when the added value is large enough.
  expect_true(identical_byname(e, f + 1e-16))
  expect_true(identical_byname(e, f + 1.1e-16))
  expect_false(identical_byname(e, f + 1.12e-16))
  expect_false(identical_byname(e, f + 1.15e-16))
  expect_false(identical_byname(e, f + 1.2e-16))
  expect_false(identical_byname(e, f + 1.5e-16))
  expect_false(identical_byname(e, f + 1.2e-16))
  expect_false(identical_byname(e, f + 1e-15))
})


test_that("equal_byname() works with tol argument", {
  expect_true(equal_byname(0, 0))
  expect_false(equal_byname(0, 1e-10))
  expect_true(equal_byname(0, 1e-6, tol = 1e-5))
  expect_true(equal_byname(0, -1e-6, tol = 1e-5))
})


test_that("identical_byname() works as expected", {
  expect_true(identical_byname(100, 100))
  # With a little bit of numerical fuzz, identical_byname fails
  expect_false(identical_byname(100, 100 + 1e-10))
  # With a little bit of numerical fuzz, equal_byname passes
  expect_true(equal_byname(100, 100 + 1e-15))
  # Now try with matrices
  a <- matrix(1:4, nrow = 2)
  b <- matrix(1:4, nrow = 2)
  expect_true(identical_byname(a, b))
  expect_false(identical_byname(a, b + 1e-100))
  a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
  # FALSE because a has row and column types, but b does not.
  expect_false(identical_byname(a, b)) 
  b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
  expect_true(identical_byname(a, b))
  dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
  dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
  # FALSE, because row and column names are not equal
  expect_false(identical_byname(a, b)) 
  dimnames(b) <- dimnames(a)
  expect_true(identical_byname(a, b))
  
  # Try with lists
  expect_equal(identical_byname(list(1, 2, 3), list(1, 2, 3)), list(TRUE, TRUE, TRUE))
  expect_equal(identical_byname(list(1, 2, 4), list(1, 2, 3)), list(TRUE, TRUE, FALSE))
  expect_equal(identical_byname(list(1, 1, 1), list(2, 2, 2), .summarise = TRUE), list(TRUE, TRUE))
  expect_equal(identical_byname(list(1, 1, 1), list(2, 2, 3), .summarise = TRUE), list(TRUE, FALSE))
})


test_that("identical_byname() works with Matrix objects", {
  # Now try with matrices
  a <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  b <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2)
  expect_true(identical_byname(a, b))
  # The sensitivity for Matrix is much smaller than for matrix.
  expect_false(identical_byname(a, b + 1e-15))
  a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
  # FALSE because a has row and column types, but b does not.
  expect_false(identical_byname(a, b)) 
  b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
  expect_true(identical_byname(a, b))
  dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
  dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
  # FALSE, because row and column names are not equal
  expect_false(identical_byname(a, b)) 
  dimnames(b) <- dimnames(a)
  expect_true(identical_byname(a, b))
})


test_that("samestructure_byname() works as expected", {
  expect_true(samestructure_byname(2, 2))
  expect_false(samestructure_byname(2, 2 %>% setrowtype("row")))
  expect_false(samestructure_byname(2 %>% setrowtype("row"), 2))
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  V <- matrix(5:8, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  expect_true(samestructure_byname(U, U))
  expect_true(samestructure_byname(U, V))
  expect_true(samestructure_byname(V, U))
  expect_false(samestructure_byname(U, U %>% setrowtype("row")))
  expect_false(samestructure_byname(U %>% setcoltype("col"), U))
  expect_false(samestructure_byname(U, U %>% setrownames_byname(c("a", "b"))))
  expect_false(samestructure_byname(U, U %>% setcolnames_byname(c("a", "b"))))
  expect_true(samestructure_byname(U, U))
  # Also works for lists
  expect_true(all(samestructure_byname(list(U, U), list(U, U)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(U, U), list(V, V)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(V, V), list(U, U)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(U, V), list(U, V), .summarise = TRUE) %>% as.logical()))
  expect_equal(samestructure_byname(list(U, V), list(U %>% setrowtype(NULL), V), .summarise = TRUE), list(TRUE, FALSE))
  
  # Check when one or both of rowtype or coltype is NULL
  expect_false(samestructure_byname(U, U %>% setrowtype(NULL)))
  expect_false(samestructure_byname(U, U %>% setcoltype(NULL)))
})


test_that("samestructure_byname() works with Matrix objects", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  V <- matsbyname::Matrix(5:8, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  expect_true(samestructure_byname(U, U))
  expect_true(samestructure_byname(U, V))
  expect_true(samestructure_byname(V, U))
  expect_false(samestructure_byname(U, U %>% setrowtype("row")))
  expect_false(samestructure_byname(U %>% setcoltype("col"), U))
  expect_false(samestructure_byname(U, U %>% setrownames_byname(c("a", "b"))))
  expect_false(samestructure_byname(U, U %>% setcolnames_byname(c("a", "b"))))
  expect_true(samestructure_byname(U, U))
  # Also works for lists
  expect_true(all(samestructure_byname(list(U, U), list(U, U)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(U, U), list(V, V)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(V, V), list(U, U)) %>% as.logical()))
  expect_true(all(samestructure_byname(list(U, V), list(U, V), .summarise = TRUE) %>% as.logical()))
  expect_equal(samestructure_byname(list(U, V), list(U %>% setrowtype(NULL), V), .summarise = TRUE), list(TRUE, FALSE))
  
  # Check when one or both of rowtype or coltype is NULL
  expect_false(samestructure_byname(U, U %>% setrowtype(NULL)))
  expect_false(samestructure_byname(U, U %>% setcoltype(NULL)))
})


test_that("make_pattern() works as expected", {
  expect_equal(RCLabels::make_or_pattern(strings = c("a", "b"), pattern_type = "exact"), "^a$|^b$")
  expect_equal(RCLabels::make_or_pattern(strings = c("a", "b"), pattern_type = "leading"), "^a|^b")
  expect_equal(RCLabels::make_or_pattern(strings = c("a", "b"), pattern_type = "trailing"), "a$|b$")
  expect_equal(RCLabels::make_or_pattern(strings = c("a", "b"), pattern_type = "anywhere"), "a|b")
  expect_equal(RCLabels::make_or_pattern(strings = c("^a$", "^b", "c$"), pattern_type = "literal"), c("^a$", "^b", "c$"))
  expect_equal(RCLabels::make_or_pattern(strings = "Non-specified (industry)", pattern_type = "exact"), "^Non-specified \\(industry\\)$")
  # Check with a list and parentheses
  expect_equal(RCLabels::make_or_pattern(strings = c("a(1)", "a(2)"), pattern_type = "exact"), 
               "^a\\(1\\)$|^a\\(2\\)$")
})


test_that("matrix multiplied by a constant in a data frame works", {
  matA <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Industries") %>% setcoltype("Products")
  temp <- data.frame(matrix = I(list()), vals = I(list()))
  temp[[1, "matrix"]] <- "A"
  temp[[1, "vals"]] <- matA
  mats <- temp %>% 
    dplyr::rename(
      matrix.name = matrix,
      matrix = vals
    ) %>% 
    tidyr::spread(key = matrix.name, value = matrix) %>% 
    # Duplicate the row to demonstrate byname operating simultaneously 
    # on all rows of the data frame.
    rbind(., .) %>% 
    dplyr::mutate(
      constant = RCLabels::make_list(x = 1:2, n = 2, lenx = 2),
      # Multiplies matrices in the sum column by corresponding constants in the c column.
      product = hadamardproduct_byname(constant, A)
    )
  expect_equal(mats$product[[1]], matrix(c(1, 3,
                                           2, 4),
                                         nrow = 2, byrow = TRUE) %>% 
                 setrownames_byname(c("p1", "p2")) %>% setcolnames_byname(c("i1", "i2")) %>% 
                 setrowtype("Industries") %>% setcoltype("Products"))
  expect_equal(mats$product[[2]], matrix(c(2, 6,
                                           4, 8),
                                         nrow = 2, byrow = TRUE) %>% 
                 setrownames_byname(c("p1", "p2")) %>% setcolnames_byname(c("i1", "i2")) %>% 
                 setrowtype("Industries") %>% setcoltype("Products"))
})


test_that("matrix multiplied by a constant in a data frame works with Matrix objects", {
  matA <- matsbyname::Matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Industries") %>% setcoltype("Products")
  temp <- data.frame(matrix = I(list()), vals = I(list()))
  temp[[1, "matrix"]] <- "A"
  temp[[1, "vals"]] <- matA
  mats <- temp %>% 
    dplyr::rename(
      matrix.name = matrix,
      matrix = vals
    ) %>% 
    tidyr::spread(key = matrix.name, value = matrix) %>% 
    # Duplicate the row to demonstrate byname operating simultaneously 
    # on all rows of the data frame.
    rbind(., .) %>% 
    dplyr::mutate(
      constant = RCLabels::make_list(x = 1:2, n = 2, lenx = 2),
      # Multiplies matrices in the sum column by corresponding constants in the c column.
      product = hadamardproduct_byname(constant, A)
    )
  expect_equal(mats$product[[1]], matsbyname::Matrix(c(1, 3,
                                                       2, 4),
                                                     nrow = 2, ncol = 2, byrow = TRUE) %>% 
                 setrownames_byname(c("p1", "p2")) %>% setcolnames_byname(c("i1", "i2")) %>% 
                 setrowtype("Industries") %>% setcoltype("Products"))
  expect_equal(mats$product[[2]], matsbyname::Matrix(c(2, 6,
                                                       4, 8),
                                                     nrow = 2, ncol = 2, byrow = TRUE) %>% 
                 setrownames_byname(c("p1", "p2")) %>% setcolnames_byname(c("i1", "i2")) %>% 
                 setrowtype("Industries") %>% setcoltype("Products"))
})


test_that("and_byname() works as expected", {
  # Test with non-logicals. 0 is interpreted as FALSE, any other number is interpreted as TRUE.
  expect_true(and_byname(2, 2))
  expect_false(and_byname(0, -1000))
  # Test with single values.
  expect_true(and_byname(TRUE))
  expect_false(and_byname(FALSE))
  expect_true(and_byname(TRUE, TRUE))
  expect_true(and_byname(TRUE, TRUE, TRUE))
  expect_false(and_byname(TRUE, TRUE, FALSE, TRUE))
  
  # Test with lists
  expect_equal(and_byname(list(TRUE)), list(TRUE))
  expect_equal(and_byname(list(FALSE)), list(FALSE))
  expect_equal(and_byname(list(TRUE, TRUE)), list(TRUE, TRUE))
  expect_equal(and_byname(list(TRUE, TRUE, TRUE)), list(TRUE, TRUE, TRUE))
  expect_equal(and_byname(list(FALSE, TRUE, TRUE, TRUE)), list(FALSE, TRUE, TRUE, TRUE))
  expect_equal(and_byname(list(TRUE, FALSE), list(TRUE, TRUE)), list(TRUE, FALSE))
  expect_equal(and_byname(list(TRUE, FALSE), list(TRUE, TRUE), list(TRUE, TRUE), list(TRUE, TRUE)), list(TRUE, FALSE))
  
  # Test with matrices
  m1 <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  m2 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(and_byname(m1, m1), m1)
  expect_equal(and_byname(m1, m2), m1 & m2)
  # Test with lists of matrices
  expect_equal(and_byname(list(m1, m1), list(m2, m2)), list(m1 & m2, m1 & m2))
  expect_equal(and_byname(list(m1, m1), list(m1, m1), list(m2, m2)), list(m1 & m2, m1 & m2))
  
  # Test with .summarise
  expect_equal(and_byname(list(m1, m1), list(m2, m2), .summarise = TRUE), list(m1 & m1, m2 & m2))
  expect_equal(and_byname(list(m1, m1), list(m1, m1), list(m2, m2), .summarise = TRUE), list(m1 & m1, m1 & m1, m2 & m2))
})


test_that("and_byname() works with Matrix objects", {
  # Test with matrices
  m1 <- matsbyname::Matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  m2 <- matsbyname::Matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  expect_equal(and_byname(m1, m1), m1)
  expect_equal(and_byname(m1, m2), m1 & m2)
  # Test with lists of matrices
  expect_equal(and_byname(list(m1, m1), list(m2, m2)), list(m1 & m2, m1 & m2))
  expect_equal(and_byname(list(m1, m1), list(m1, m1), list(m2, m2)), list(m1 & m2, m1 & m2))
  
  # Test with .summarise
  expect_equal(and_byname(list(m1, m1), list(m2, m2), .summarise = TRUE), list(m1 & m1, m2 & m2))
  expect_equal(and_byname(list(m1, m1), list(m1, m1), list(m2, m2), .summarise = TRUE), list(m1 & m1, m1 & m1, m2 & m2))
  
})
