test_that("difference_byname() with constants works as expected", {
  # Simple difference of constants
  expect_equal(difference_byname(100, 50), 50)
  
  # If differenced against NULL, return the item.
  expect_equal(difference_byname(NULL, 1), -1)
  expect_equal(difference_byname(2, NULL), 2)
  expect_equal(difference_byname(list(NULL, 1), list(1, 1)), list(-1, 0))
  # If differenced against NA, return NA
  expect_equal(difference_byname(2, NA), NA_integer_)
})


test_that("difference_byname() of matrices works as expected", {
  test_func <- function(this_U, this_Z) {
    # If only one argument, return it.
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(this_U), this_U)
    
    # This is a non-sensical test.  Row and column names are not respected. 
    # Row names, column names, and row and column types come from the first operand (U).
    matsbyname:::expect_equal_matrix_or_Matrix(this_U - this_Z, matrix(c(-3, -1, 1, 3), 
                                         nrow = 2, 
                                         dimnames = dimnames(this_U)) %>% 
                   setrowtype(rowtype(this_U)) %>% setcoltype(coltype(this_U)))
    
    # Row and column names respected! Should be all zeroes.
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(this_U, this_Z), 
                 matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(this_U)) %>% 
                   setrowtype(rowtype(this_U)) %>% setcoltype(coltype(this_U)))
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(100, this_U), 
                 matrix(c(99, 98, 97, 96), nrow = 2, dimnames = dimnames(this_U)) %>% 
                   setrowtype(rowtype(this_U)) %>% setcoltype(coltype(this_U)))
    # difference_byname should sort the rows and column names.
    # So we expect the dimnames of the difference to be the same as the dimnames of U (which has sorted dimnames).
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(10, this_Z), 
                                               matrix(c(9, 8, 7, 6), 
                                                      ncol = 2, 
                                                      dimnames = dimnames(this_U)) %>% 
                                                 setrowtype(rowtype(this_Z)) %>% setcoltype(coltype(this_Z)))
    # When subtrahend is missing, return minuend (in this case, Z) with sorted rows and columns.
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(this_Z), sort_rows_cols(this_Z))
    # When minuend is missing, return - subtrahend (in this case, -Z)
    matsbyname:::expect_equal_matrix_or_Matrix(difference_byname(subtrahend = this_Z), hadamardproduct_byname(-1, this_Z))    
  }
  
  # Try with matrix objects
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  Um <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Zm <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  test_func(Um, Zm)
  
  # Try with Matrix objects
  UM <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  ZM <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  test_func(UM, ZM)
})


test_that("difference_byname() of matrices in lists and data frames works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  # Try with U being a Matrix and Z being a matrix
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Z <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  UminusZ <- matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Z = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z
  
  expect_equal(difference_byname(list(100, 100), list(50, 50)), list(50, 50))
  diffs1 <- difference_byname(list(U, U), list(Z, Z))
  lapply(diffs1, FUN = function(this_diff) {
    matsbyname:::expect_equal_matrix_or_Matrix(this_diff, UminusZ)
  })
  
  diffs2 <- difference_byname(DF$U, DF$Z)
  lapply(diffs2, FUN = function(this_diff) {
    matsbyname:::expect_equal_matrix_or_Matrix(this_diff, UminusZ)
  })
  
  DF_diff <- DF %>% 
    dplyr::mutate(
      diffs = difference_byname(U, Z)
    )
  lapply(DF_diff$diffs, FUN = function(this_diff) {
    matsbyname:::expect_equal_matrix_or_Matrix(this_diff, UminusZ)
  })
})
