# Contains tests for the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(parallel)
library(byname)
library(magrittr)
library(testthat)
library(matsindf)
library(tidyr)


###########################################################
context("Sums")
###########################################################

# Define some matrices with product and industry names and types
# These matrices will be used in the tests below.
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
  setrowtype("Products") %>% setcoltype("Industries")
V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, productnames)) %>%
  setrowtype("Industries") %>% setcoltype("Products")
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
Z <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
  setrowtype("Products") %>% setcoltype("Industries")

UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
  setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
UminusZ <- matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
  setrowtype(rowtype(U)) %>% setcoltype(coltype(U))

Uplus100 <- U + 100

test_that("sums of constants works as expected", {
  # Simple sum of constants
  expect_equal(sum_byname(2, 3), 5)
  
  # If summed against NULL, return the item.
  expect_equal(sum_byname(NULL, 1), 1)
  expect_equal(sum_byname(2, NULL), 2)
  expect_equal(sum_byname(list(NULL, 1), list(1, 1)), list(1, 2))
  # If summed against NA, return NA
  expect_equal(sum_byname(2, NA), NA_integer_)
})

test_that("sums of matrices works as expected", {
  # If only one argument, return it.
  expect_equal(sum_byname(U), U)
  
  # This is a non-sensical test.  Row and column names are not respected. 
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U + Y, 
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  # Now, row and column names are respected.
  expect_equal(sum_byname(U, Y), UplusY)
  
  expect_equal(sum_byname(U, 100), U + 100)
  expect_equal(sum_byname(200, Y), 200 + Y %>% sort_rows_cols() %>% 
                                               setrowtype(rowtype(Y)) %>% 
                                               setcoltype(coltype(Y)))
  
  # This is a non-sensical test.  Row and column names are not respected.
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U + V,
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  
  # We should not be able to add U and V, because their row and column types differ.
  # Would like to test for entire error message which is 
  # "rowtype(a) == rowtype(b) is not TRUE"
  # However, it seems that the testthat package has trouble dealing with "(" in error messages.
  # So, we'll just test for the first word.
  expect_error(sum_byname(U, V), "rowtype")
})

test_that("sums of matrices in lists and data frames works as expected", {
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  
  # sum_byname should also work with lists.
  expect_equal(sum_byname(list(U,U), list(Y, Y)), list(UplusY, UplusY))
  expect_equal(sum_byname(list(U,U), list(100,100)), list(Uplus100, Uplus100))
  expect_equal(sum_byname(list(U,U), as.list(rep_len(100, 2))), list(Uplus100, Uplus100))
  
  # sum_byname also should work with data frames, as they are lists.
  expect_equal(sum_byname(DF$U, DF$Y), list(UplusY, UplusY))
  expect_equal(DF %>% mutate(sums = sum_byname(U, Y)), DF %>% mutate(sums = list(UplusY, UplusY)))
})


###########################################################
context("Differences")
###########################################################

test_that("differences of constants works as expected", {
  # Simple difference of constants
  expect_equal(difference_byname(100, 50), 50)
  
  # If differenced against NULL, return the item.
  expect_equal(difference_byname(NULL, 1), -1)
  expect_equal(difference_byname(2, NULL), 2)
  expect_equal(difference_byname(list(NULL, 1), list(1, 1)), list(-1, 0))
  # If summed against NA, return NA
  expect_equal(difference_byname(2, NA), NA_integer_)
})

test_that("differences of matrices works as expected", {
  # If only one argument, return it.
  expect_equal(difference_byname(U), U)
  
  # This is a non-sensical test.  Row and column names are not respected. 
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U - Z, matrix(c(-3, -1, 1, 3), nrow = 2, dimnames = dimnames(U)) %>% 
                      setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
               
  # Row and column names respected! Should be all zeroes.
  expect_equal(difference_byname(U, Z), matrix(0, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>% 
                                        setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  expect_equal(difference_byname(100, U), matrix(c(99, 98, 97, 96), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  # difference_byname should sort the rows and column names.
  # So we expect the dimnames of the difference to be the same as the dimnames of U (which has sorted dimnames).
  expect_equal(difference_byname(10, Z), matrix(c(9, 8, 7, 6), ncol = 2, dimnames = dimnames(U)) %>% 
                                        setrowtype(rowtype(Z)) %>% setcoltype(coltype(Z)))
  # When subtrahend is missing, return minuend (in this case, Z).
  expect_equal(difference_byname(Z), Z)
  # When minuend is missing, return - subtrahend (in this case, -Z)
  expect_equal(difference_byname(subtrahend = Z), elementproduct_byname(-1, Z))
})
  
test_that("differences of matrices in lists and data frames works as expected", {
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Z = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z

  expect_equal(difference_byname(list(100, 100), list(50, 50)), list(50, 50))
  expect_equal(difference_byname(list(U, U), list(Z, Z)), list(UminusZ, UminusZ))
  expect_equal(difference_byname(DF$U, DF$Z), list(UminusZ, UminusZ))
  expect_equal(difference_byname(DF %>% mutate(diffs = difference_byname(U, Z))), 
               DF %>% mutate(diffs = list(UminusZ)))
})


###########################################################
context("Products")
###########################################################

test_that("matrixproduct_byname works as expected", {
  V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("p1", "p2", "p3"))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  Y <- matrix(1:4, ncol = 2, dimnames = list(c("p2", "p1"), c("s2", "s1"))) %>%
    setrowtype("Products") %>% setcoltype("Sectors")
  VY <- matrix(c(13,5,
                 20,8),
               nrow = 2, ncol = 2, byrow = TRUE,
               dimnames = list(c("i1", "i2"), c("s1", "s2"))) %>% 
    setrowtype("Industries") %>% setcoltype("Sectors")
  # Fails. 3 columns of V cannot be matrix multiplied into 2 rows of Y.  Y lacks a row named p3.
  expect_error(V %*% Y, "non-conformable arguments")
  # Succeeds because Y is completed to include a row named p3 (that contains zeroes).
  # Furthermore, rows and columns of Y are sorted to be in alphabetical order.
  expect_equal(matrixproduct_byname(V, Y), VY)
  
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
  DF <- data.frame(V = I(list()), Y = I(list()))
  DF[[1,"V"]] <- V
  DF[[2,"V"]] <- V
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(matrixproduct_byname(DF$V, DF$Y), list(VY, VY))
  
  # And it works with the tidyverse functions
  DF_expected <- data.frame(V = I(list()), Y = I(list()), matprods = I(list()))
  DF_expected[[1, "V"]] <- V
  DF_expected[[2, "V"]] <- V
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "matprods"]] <- VY
  DF_expected[[2, "matprods"]] <- VY
  # Because DF_expected$matprods is created with I(list()), its class is "AsIs".
  # Because DF$matprods is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$matprods to NULL to get a match.
  attr(DF_expected$matprods, which = "class") <- NULL
  expect_equal(DF %>% mutate(matprods = matrixproduct_byname(V, Y)), DF_expected)
  
  # Test whether this works with a column of matrices multiplied by a single matrix.
  # In other words, we want a single matrix to multiply several matrices.
  # M is a single matrix. 
  # Should obtain same results as above.
  M <- Y
  expect_equal(DF %>% mutate(matprods = matrixproduct_byname(V, M)), DF_expected)
})

test_that("elementproduct_byname works as expected", {
  expect_equal(elementproduct_byname(2, 2), 4)
  expect_equal(elementproduct_byname(matrix(c(10, 10), nrow = 2, ncol = 1), 1000), 
               matrix(c(10000, 10000), nrow = 2, ncol = 1))
  
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
  expect_equal(elementproduct_byname(U, Y), UY_expected)
  expect_equal(elementproduct_byname(U, 0), matrix(c(0,0,0,0), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  # Use dimnames(U), because after performing elementproduct_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  expect_equal(elementproduct_byname(0, Y), matrix(c(0,0,0,0), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(elementproduct_byname(list(U, U), list(Y, Y)), list(UY_expected, UY_expected))
  # And it works with data frames 
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(elementproduct_byname(DF$U, DF$Y), list(UY_expected, UY_expected))
  DF_expected <- data.frame(U = I(list()), Y = I(list()), elementprods = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "Y"]] <- Y
  DF_expected[[2, "Y"]] <- Y
  DF_expected[[1, "elementprods"]] <- UY_expected
  DF_expected[[2, "elementprods"]] <- UY_expected
  # Because DF_expected$elementprods is created with I(list()), its class is "AsIs".
  # Because DF$elementprods is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$elementprods to NULL to get a match.
  attr(DF_expected$elementprods, which = "class") <- NULL
  expect_equal(DF %>% mutate(elementprods = elementproduct_byname(U, Y)), DF_expected)
  # Test with a constant multipliying a colum of the DF
  DF_2 <- DF %>% 
    mutate(
      c = 10,
      A = elementproduct_byname(c, U)
    )
  for (i in c(1:2)){
    expect_equal(DF_2$A[[i]], DF$U[[i]]*10)
  }
  constant <- 20
  DF_3 <- DF %>% 
    mutate(
      B = elementproduct_byname(constant, U)
    )
  for (i in c(1:2)) {
    expect_equal(DF_3$B[[i]], DF$U[[i]]*20)
  }
  
  # Try with a list of matrices and a single value.
  Ux2_expected <- matrix(c(2, 4, 6, 8), nrow = 2, ncol = 2, dimnames = dimnames(DF$U[[1]])) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(elementproduct_byname(DF$U, 2), list(Ux2_expected, Ux2_expected))
  # Try with a list of matrices and a single matrix
  expect_equal(elementproduct_byname(DF$U, 
                                     matrix(c(2,2,2,2), nrow = 2, ncol = 2, dimnames = dimnames(Ux2_expected))), 
               list(Ux2_expected, Ux2_expected))
})


###########################################################
context("Quotients")
###########################################################

test_that("elementquotient_byname works as expected", {
  expect_equal(elementquotient_byname(100, 50), 2)
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
  expect_equal(elementquotient_byname(U, Y), UoverY_expected)
  expect_equal(elementquotient_byname(U, 10), 
               matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, dimnames = dimnames(U)) %>% 
                 setrowtype("Products") %>% setcoltype("Industries"))
  tenoverY_expected <- matrix(c(10, 5, 10/3, 2.5), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(elementquotient_byname(10, Y), tenoverY_expected)
  # This also works with lists
  expect_equal(elementquotient_byname(10, list(Y,Y)), list(tenoverY_expected, tenoverY_expected))
  # Use dimnames(U), because after performing elementquotient_byname, 
  # the rows and columns will be sorted alphabetically by name. 
  # U has rows and columns that are sorted alphabetically by name.
  Yover10_expected <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow = 2, dimnames = dimnames(U)) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(elementquotient_byname(list(Y,Y), 10), list(Yover10_expected, Yover10_expected))
  expect_equal(elementquotient_byname(list(U, U), list(Y, Y)), list(UoverY_expected, UoverY_expected))
  # Also works with data frames.
  DF <- data.frame(U = I(list()), Y = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  expect_equal(elementquotient_byname(DF$U, DF$Y), list(UoverY_expected, UoverY_expected))
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
  expect_equal(DF %>% mutate(elementquotients = elementquotient_byname(U, Y)), DF_expected)
})


###########################################################
context("Means")
###########################################################

test_that("mean_byname works as expected", {
  expect_equal(mean_byname(100, 50), 75)
  expect_equal(mean_byname(0, 0), 0)
  expect_equal(mean_byname(-2, -4), -3)
  expect_equal(mean_byname(-10, 10), 0)
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
  # This also works with lists
  expect_equal(mean_byname(list(100, 100), list(50, 50)), list(75, 75))
  expect_equal(mean_byname(list(U,U), list(G,G)), list(UGavg, UGavg))
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
  expect_equal(DF %>% mutate(means = mean_byname(U, G)), DF_expected)
})

test_that("geometricmean_byname works as expected", {
  expect_equal(geometricmean_byname(0, 0), 0)
  expect_equal(geometricmean_byname(10, 20), sqrt(10*20))
  expect_error(geometricmean_byname(-10, 10), "X1 and X2 must have same sign in geometricmean_byname")
  expect_equal(geometricmean_byname(10, 1000), 100)
  expect_equal(geometricmean_byname(X1 = matrix(c(10, 10), nrow = 2, ncol = 1), X2 = 1000), 
               matrix(c(100, 100), nrow = 2, ncol = 1))
  
  commoditynames <- c("c1", "c2")
  industrynames <- "i1"
  U <- matrix(c(10, 1000), ncol = 1, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  G <- matrix(c(1e3, 1e5), ncol = 1, nrow = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  UGgeomean <- matrix(c(1000, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  # Non-sensical. Row and column names not respected.
  expect_equal(sqrt(U*G), 
               matrix(c(100, 10000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # Row and column names respected!
  expect_equal(geometricmean_byname(U, G), UGgeomean)
  expect_equal(geometricmean_byname(1000, U), 
               matrix(c(100, 1000), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  expect_equal(geometricmean_byname(10, G), 
               matrix(c(1000, 100), nrow = 2, ncol = 1, dimnames = list(commoditynames, industrynames)) %>%
                 setrowtype("Commodities") %>% setcoltype("Industries"))
  # This also works with lists
  expect_equal(geometricmean_byname(list(10, 1000), list(1000, 10)), list(100, 100))
  expect_equal(geometricmean_byname(list(U,U), list(G,G)), list(UGgeomean, UGgeomean))
  DF <- data.frame(U = I(list()), G = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"G"]] <- G
  DF[[2,"G"]] <- G
  expect_equal(geometricmean_byname(DF$U, DF$G), list(UGgeomean, UGgeomean))
  DF_expected <- data.frame(U = I(list()), G = I(list()), geomeans = I(list()))
  DF_expected[[1, "U"]] <- U
  DF_expected[[2, "U"]] <- U
  DF_expected[[1, "G"]] <- G
  DF_expected[[2, "G"]] <- G
  DF_expected[[1, "geomeans"]] <- UGgeomean
  DF_expected[[2, "geomeans"]] <- UGgeomean
  # Because DF_expected$geomeans is created with I(list()), its class is "AsIs".
  # Because DF$geomeans is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$geomeans to NULL to get a match.
  attr(DF_expected$geomeans, which = "class") <- NULL
  expect_equal(DF %>% mutate(geomeans = geometricmean_byname(U, G)), DF_expected)
})

test_that("logmean works as expected", {
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

test_that("logarithmicmean_byname works as expected", {
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
  expect_equal(logarithmicmean_byname(m1, 2), 
               matrix(c(1.442695041, 2.885390082, 
                        2, 3.274070004, 
                        2.466303462, 3.640956907), byrow = TRUE,
                      nrow = 3, ncol = 2, dimnames = dimnames(m1)) %>% 
                 setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1)))
  expect_equal(logarithmicmean_byname(2, m1), 
               matrix(c(1.442695041, 2.885390082, 
                        2, 3.274070004, 
                        2.466303462, 3.640956907), byrow = TRUE,
                      nrow = 3, ncol = 2, dimnames = dimnames(m1)) %>% 
                 setrowtype(rowtype(m1)) %>% setcoltype(coltype(m1)))
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
  expect_equal(DF %>% mutate(logmeans = logarithmicmean_byname(m1, m2)), DF_expected)
})


###########################################################
context("Inversion")
###########################################################

test_that("invert_byname works as expected", {
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
  expect_equal(DF %>% mutate(minv = invert_byname(m)), DF_expected)
})


###########################################################
context("Transpose")
###########################################################

test_that("transpose_byname works as expected", {
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
  expect_equal(DF %>% mutate(mT = transpose_byname(m)), DF_expected)
})


###########################################################
context("Hatize")
###########################################################

test_that("hatize_byname works as expected", {
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
  expect_equal(hatize_byname(r), r_hat_expected)
  # This also works with lists.
  expect_equal(hatize_byname(list(v, v)), list(v_hat_expected, v_hat_expected))
  # And it works with data frames.
  DF <- data.frame(v = I(list()))
  DF[[1,"v"]] <- v
  DF[[2,"v"]] <- v
  expect_equal(hatize_byname(DF$v), list(v_hat_expected, v_hat_expected))
  DF_expected <- data.frame(v = I(list()), v_hat = I(list()))
  DF_expected[[1,"v"]] <- v
  DF_expected[[2,"v"]] <- v
  DF_expected[[1,"v_hat"]] <- v_hat_expected
  DF_expected[[2,"v_hat"]] <- v_hat_expected
  # Because DF_expected$v_hat is created with I(list()), its class is "AsIs".
  # Because DF$v_hat is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$v_hat to NULL to get a match.
  attr(DF_expected$v_hat, which = "class") <- NULL
  expect_equal(DF %>% mutate(v_hat = hatize_byname(v)), DF_expected)
})


###########################################################
context("Identize")
###########################################################

test_that("identize_byname works as expected", {
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
  expect_error(identize_byname(m, margin = c(1,2,3,4)), "margin should have length 1 or 2 in fractionize_byname")
  expect_error(identize_byname(m, margin = c(3)), "Unknown margin 3 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m, margin = c(-1)), "Unknown margin -1 in identize_byname. margin should be 1, 2, or c\\(1,2\\)")
  expect_error(identize_byname(m, margin = c(1,1,2,2)), "margin should have length 1 or 2 in fractionize_byname")

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
  expect_equal(identize_byname(list(m, m)), list(mI_expected, mI_expected))
  # This also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(identize_byname(DF$m), list(mI_expected, mI_expected))
  expect_equal(identize_byname(DF$m, margin = c(1,2)), list(mI_expected, mI_expected))
  expect_equal(identize_byname(DF$m, margin = c(2,1)), list(mI_expected, mI_expected))
  DF_expected <- data.frame(m = I(list()), mI = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"mI"]] <- mI_expected
  DF_expected[[2,"mI"]] <- mI_expected
  # Because DF_expected$mI is created with I(list()), its class is "AsIs".
  # Because DF$mI is created from an actual calculation, its class is NULL.
  # Need to set the class of DF_expected$mI to NULL to get a match.
  attr(DF_expected$mI, which = "class") <- NULL
  expect_equal(DF %>% mutate(mI = identize_byname(m)), DF_expected)
})


###########################################################
context("Fractionize")
###########################################################

test_that("fractionze_byname works as expected", {
  M <- matrix(c(1, 5,
                4, 5),
              nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setcoltype("Products") %>% setrowtype("Industries")
  expectedM_rows <- matrix(c(1/6, 5/6,
                             4/9, 5/9),
                           nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setcoltype("Products") %>% setrowtype("Industries")
  expectedM_cols <- matrix(c(1/5, 5/10,
                             4/5, 5/10),
                           nrow = 2, ncol = 2, byrow = TRUE,
                           dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setcoltype("Products") %>% setrowtype("Industries")
  expectedM_sumall <- matrix(c(1/15, 5/15,
                               4/15, 5/15),
                             nrow = 2, ncol = 2, byrow = TRUE,
                             dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setcoltype("Products") %>% setrowtype("Industries")
  
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
  expect_true(is.nan(fractionize_byname(0, margin = 1)))
  
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
    mutate(
      F_row = fractionize_byname(M, margin = 1),
      F_col = fractionize_byname(M, margin = 2),
      F_tot = fractionize_byname(M, margin = c(2,1))
    )

  expect_equal(DF2$F_row, list(expectedM_rows, expectedM_rows))
  expect_equal(DF2$F_col, list(expectedM_cols, expectedM_cols))
  expect_equal(DF2$F_tot, list(expectedM_sumall, expectedM_sumall))
})


###########################################################
context("Row selection")
###########################################################

m_rownames <- paste0("i", 1:4)
m_colnames <- paste0("p", 1:4)
m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
  setrowtype("Industries") %>% setcoltype("Products")

n1 <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
n2 <- setcolnames_byname(m, c("a1", "a2", "b1", "b2"))

test_that("matrix row selection by name with exact matches (^name$) works as expected", {
  # Select only the first row (i1)
  expect_equal(select_rows_byname(m, retain_pattern = "^i1$"), 
               matrix(c(seq(1, 13, by = 4)), nrow = 1, dimnames = list(c("i1"), m_colnames)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Try same test using the make_pattern utility function.
  expect_equal(select_rows_byname(m, retain_pattern = make_pattern(row_col_names = "i1", pattern_type = "exact")), 
               matrix(c(seq(1, 13, by = 4)), nrow = 1, dimnames = list(c("i1"), m_colnames)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Select rows 1 and 4 (i1, i4)
  expect_equal(select_rows_byname(m, retain_pattern = "^i1$|^i4$"), 
               m[c(1, 4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate row 3 (i3)
  expect_equal(select_rows_byname(m, remove_pattern = "^i3$"), 
               m[-3, ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate rows 1 and 3
  expect_equal(select_rows_byname(m, remove_pattern = "^i1$|^i3$"), 
               m[c(-1,-3), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Retain row 4.  Retain has precedence over remove.
  expect_equal(select_rows_byname(m, retain_pattern = "^i4$", remove_pattern = "^i1$|^i3$|^i4$"), 
               matrix(c(seq(4, 16, by = 4)), nrow = 1, dimnames = list(c("i4"), m_colnames)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Matches nothing.  NULL is returned.
  expect_null(select_rows_byname(m, retain_pattern = "^x$"))
  # Matches nothing.  All of m is returned.
  expect_equal(select_rows_byname(m, remove_pattern = "^x$"), m)
  
  # Here is a pathological case where the row name contains ( and ).
  # ( and ) need to be escaped properly for use in regex.
  crazymat <- matrix(1, nrow = 2, ncol = 2, 
                     dimnames = list(c("i (1)", "i (2)"), c("p (1)", "p (2)"))) %>% 
    setrowtype("Industries") %>% setcoltype("Prodcuts")
  expect_equal(select_rows_byname(crazymat, retain_pattern = make_pattern(row_col_names = "i (1)", pattern_type = "exact")), 
               matrix(1, nrow = 1, ncol = 2, dimnames = list("i (1)", c("p (1)", "p (2)"))) %>% 
                 setrowtype(rowtype(crazymat)) %>% setcoltype(coltype(crazymat)))
})

test_that("matrix row selection by name with inexact matches works as expected", {
  # Matches first two rows, because partial match is OK.
  expect_equal(select_rows_byname(n1, retain_pattern = "^a"), 
               n1[c(1,2), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
  # Deletes first two rows, because partial match is OK, and first two row names start with "a".
  expect_equal(select_rows_byname(n1, remove_pattern = "^a"), 
               n1[c(3,4), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
})

test_that("matrix row selection by name with inexact matches and multiple selectors", {
  # The retain_pattern selects all rows whose names start with "a" or "b".
  # This approach should retain rows with names "a1", "a2", "b1", and "b2", i.e.,
  # all rows in n1.
  expect_equal(select_rows_byname(n1, retain_pattern = "^a|^b"), n1)
})

test_that("matrix row selection by name in lists works as expected", {
  # Use different row names for each item in the list
  expect_equal(select_rows_byname(list(m,m), retain_pattern = list("^i1$|^i4$", "^i2$|^i3$")), 
               list(m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[c(2,3), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Use same row names for each item in the list
  expect_equal(select_rows_byname(list(m,m), retain_pattern = "^i1$|^i4$"),
               list(m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% mutate(trimmed = select_rows_byname(.$m, 
                                                   retain_pattern = make_pattern(row_col_names = c("i1", "i2"), 
                                                                                 pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  DF_expected[[2,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equivalent(DF, DF_expected)
})


###########################################################
context("Column selection")
###########################################################

test_that("matrix column selection by name with exact matches (^name$) works as expected", {
  # Select only the first column (p1)
  expect_equal(select_cols_byname(m, retain_pattern = "^p1$"), 
               matrix(1:4, ncol = 1, dimnames = list(m_rownames, c("p1"))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Try same test using the make_pattern utility function.
  expect_equal(select_cols_byname(m, retain_pattern = make_pattern(row_col_names = "p1", pattern_type = "exact")), 
               matrix(1:4, ncol = 1, dimnames = list(m_rownames, c("p1"))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Select columns 1 and 4 (p1, p4)
  expect_equal(select_cols_byname(m, retain_pattern = "^p1$|^p4$"), 
               m[ , c(1, 4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate column 3 (p3)
  expect_equal(select_cols_byname(m, remove_pattern = "^p3$"), 
               m[ , -3] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate columns 1 and 3
  expect_equal(select_cols_byname(m, remove_pattern = "^p1$|^p3$"), 
               m[ , c(-1,-3)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Retain column 4.  Retain has precedence over remove.
  expect_equal(select_cols_byname(m, retain_pattern = "^p4$", remove_pattern = "^p1$|^p3$|^p4$"), 
               matrix(13:16, ncol = 1, dimnames = list(m_rownames, c("p4"))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Matches nothing.  NULL is returned.
  expect_null(select_cols_byname(m, retain_pattern = "^x$"))
  # Matches nothing.  All of m is returned.
  expect_equal(select_cols_byname(m, remove_pattern = "^x$"), m)
})

test_that("matrix column selection by name with inexact matches works as expected", {
  # Matches first two columns, because partial match is OK.
  expect_equal(select_cols_byname(n2, retain_pattern = "^a"), 
               n2[ , c(1,2)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
  # Deletes first two columns, because partial match is OK, and first two column names start with "a".
  expect_equal(select_cols_byname(n2, remove_pattern = "^a"), 
               n2[ , c(3,4)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
})

test_that("matrix column selection by name with inexact matches and multiple selectors", {
  # The retain_pattern selects all columns whose names start with "a" or "b".
  # This approach should retain columns with names "a1", "a2", "b1", and "b2", i.e.,
  # all columns in n2.
  expect_equal(select_cols_byname(n2, retain_pattern = "^a|^b"), n2)
})

test_that("matrix column selection by name in lists works as expected", {
  # Use different column names for each item in the list
  expect_equal(select_cols_byname(list(m,m), retain_pattern = list("^p1$|^p4$", "^p2$|^p3$")), 
               list(m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[ , c(2,3)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Use same column names for each item in the list
  expect_equal(select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$"),
               list(m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% mutate(trimmed = select_cols_byname(.$m, 
                                                   retain_pattern = make_pattern(row_col_names = c("p1", "p2"), 
                                                                                 pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  DF_expected[[2,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equivalent(DF, DF_expected)
})


###########################################################
context("Row, column, and all sums")
###########################################################

test_that("rowsums_byname works as expected", {
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
  expect_equal(rowsums_byname(list(m, m), NULL), list(rowsumsm_expected, rowsumsm_expected))
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
  expect_equal(DF %>% mutate(mi = rowsums_byname(m)), DF_expected)
})

test_that("colsums_byname works as expected", {
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
  expect_equal(colsums_byname(list(m, m), NULL), list(colsumsm_expected, colsumsm_expected))
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
  expect_equal(DF %>% mutate(iTm = colsums_byname(m)), DF_expected)
})

test_that("sumall_byname works as expected", {
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
  expect_equal(DF %>% mutate(summ = sumall_byname(m)), DF_expected)
})


###########################################################
context("Row, column, and all prods")
###########################################################

test_that("rowprods_byname works as expected", {
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
  expect_equal(DF %>% mutate(mi = rowprods_byname(m)), DF_expected)
})

test_that("colprods_byname works as expected", {
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
  expect_equal(DF %>% mutate(iTm = colprods_byname(m)), DF_expected)
})

test_that("prodall_byname works as expected", {
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
  expect_equal(DF %>% mutate(prodm = prodall_byname(m)), DF_expected)
})


###########################################################
context("Iminus")
###########################################################

test_that("Iminus_byname works as expected", {
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
  expect_equal(DF %>% mutate(Iminusm = Iminus_byname(m)), DF_expected)

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


###########################################################
context("Matrix cleaning")
###########################################################

test_that("matrix cleaning works as expected", {
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


###########################################################
context("Row and column naming")
###########################################################

test_that("getting row names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getrownames_byname(m), c("i1", "i2"))
  # This also works for lists
  expect_equal(getrownames_byname(list(m,m)), list(c("i1", "i2"), c("i1", "i2")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getrownames_byname(DF$m), list(c("i1", "i2"), c("i1", "i2")))
})
  
test_that("getting column names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getcolnames_byname(m), c("p1", "p2", "p3"))
  # This also works for lists
  expect_equal(getcolnames_byname(list(m,m)), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getcolnames_byname(DF$m), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
})

test_that("setting row names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m1, c("a", "b"))
  expect_equal(rownames(m2), c("a", "b"))
  m3 <- setrownames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
  expect_equal(rownames(m3), c("c", "d"))
  m4 <- m1 %>% setrownames_byname(NULL)
  expect_null(rownames(m4))
  m5 <- m1 %>% setrownames_byname(NA)
  expect_null(rownames(m5))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setrownames_byname(l1, list(c("a", "b"), c("c", "d")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("c", "d")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    mutate(
      mcol2 = setrownames_byname(mcol, c("r1", "r2"))
    )
  expect_equal(rownames(DF2$mcol2[[1]]), c("r1", "r2"))
  expect_equal(rownames(DF2$mcol2[[2]]), c("r1", "r2"))
  DF3 <- DF1 %>% 
    mutate(
      mcol2 = setrownames_byname(mcol, list(c("r1", "r2"), c("r3", "r4")))
    )
  expect_equal(list(rownames(DF3$mcol2[[1]]), rownames(DF3$mcol2[[2]])), list(c("r1", "r2"), c("r3", "r4")))
})

test_that("setting col names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setcolnames_byname(m1, c("a", "b", "c"))
  expect_equal(colnames(m2), c("a", "b", "c"))
  m3 <- setcolnames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("d", "e", "f"))
  expect_equal(colnames(m3), c("d", "e", "f"))
  m4 <- m1 %>% setcolnames_byname(NULL)
  expect_null(colnames(m4))
  m5 <- m1 %>% setcolnames_byname(NA)
  expect_null(colnames(m5))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setcolnames_byname(l1, list(c("a", "b", "c"), c("d", "e", "f")))
  expect_equal(list(colnames(l2[[1]]), colnames(l2[[2]])), list(c("a", "b", "c"), c("d", "e", "f")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(colnames(DF2$mcol2[[1]]), c("c1", "c2", "c3"))
  expect_equal(colnames(DF2$mcol2[[2]]), c("c1", "c2", "c3"))
  DF3 <- DF1 %>% 
    mutate(
      mcol2 = setcolnames_byname(mcol, list(c("c1", "c2", "c3"), c("c4", "c5", "c6")))
    )
  expect_equal(list(colnames(DF3$mcol2[[1]]), colnames(DF3$mcol2[[2]])), list(c("c1", "c2", "c3"), c("c4", "c5", "c6")))
})


###########################################################
context("Row and column types")
###########################################################

test_that("setrowtype and rowtype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products")
  expect_null(U %>% setrowtype(NULL) %>% rowtype())
  expect_equal(rowtype(U), "Products")
  # This also works for lists
  Ul <- setrowtype(list(U,U), rowtype = "Products")
  expect_equal(rowtype(Ul), list("Products", "Products"))
  Ul2 <- setrowtype(list(U,U), rowtype = list("Products", "Junk"))
  expect_equal(rowtype(Ul2), list("Products", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setrowtype(DF$U, "Products")
  expect_equal(rowtype(DF2), list("Products", "Products"))
  DF3 <- DF %>% mutate(newcol = setrowtype(U, "Products"))
  expect_equal(DF3$newcol %>% rowtype, list("Products", "Products"))
})

test_that("setcoltype and coltype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setcoltype("Industries")
  expect_null(U %>% setcoltype(NULL) %>% coltype())
  expect_equal(coltype(U), "Industries")
  # This also works for lists
  Ul <- setcoltype(list(U,U), coltype = "Industries")
  expect_equal(coltype(Ul), list("Industries", "Industries"))
  Ul2 <- setcoltype(list(U,U), coltype = list("Industries", "Junk"))
  expect_equal(coltype(Ul2), list("Industries", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setcoltype(DF$U, "Industries")
  expect_equal(coltype(DF2), list("Industries", "Industries"))
  DF3 <- DF %>% mutate(newcol = setcoltype(U, "Industries"))
  expect_equal(DF3$newcol %>% coltype, list("Industries", "Industries"))
})


###########################################################
context("Equal_byname")
###########################################################

test_that("equal_byname works as expected", {
  # Try without row and column names
  a <- matrix(1:4, nrow = 2)
  b <- matrix(4:1, nrow = 2)
  expect_false(equal_byname(a, b))
  b <- matrix(1:4, nrow = 2)
  expect_true(equal_byname(a, b))
  
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
  dimnames(b) <- dimnames(a)
  expect_true(equal_byname(a, b))
})


###########################################################
context("Utilities")
###########################################################

test_that("make_pattern works as expected", {
  expect_equal(make_pattern(row_col_names = c("a", "b"), pattern_type = "exact"), "^a$|^b$")
  expect_equal(make_pattern(row_col_names = c("a", "b"), pattern_type = "leading"), "^a|^b")
  expect_equal(make_pattern(row_col_names = c("a", "b"), pattern_type = "trailing"), "a$|b$")
  expect_equal(make_pattern(row_col_names = c("a", "b"), pattern_type = "anywhere"), "a|b")
  expect_equal(make_pattern(row_col_names = "Non-specified (industry)", pattern_type = "exact"), "^Non-specified \\(industry\\)$")
})
  
test_that("list_of_rows_or_cols works as expected", {
  m <- matrix(data = c(1:6), nrow = 2, ncol = 3, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>% 
    setrowtype(rowtype = "Products") %>% setcoltype(coltype = "Industries")
  expected_margin_1 <- list(p1 = matrix(seq(1, 5, by = 2), nrow = 3, ncol = 1, dimnames = list(c("i1", "i2", "i3"), "p1")) %>% 
                              setrowtype("Industries") %>% setcoltype("Products"), 
                            p2 = matrix(seq(2, 6, by = 2), nrow = 3, ncol = 1, dimnames = list(c("i1", "i2", "i3"), "p2")) %>% 
                              setrowtype("Industries") %>% setcoltype("Products"))
  expected_margin_2 <- list(i1 = matrix(1:2, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i1")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"),
                            i2 = matrix(3:4, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i2")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"),
                            i3 = matrix(5:6, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i3")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"))
  expect_equal(list_of_rows_or_cols(m, margin = 1), expected = expected_margin_1)
  expect_equal(list_of_rows_or_cols(m, margin = 2), expected = expected_margin_2)
  
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% mutate(
    extracted_rows = list_of_rows_or_cols(m, margin = 1), 
    extracted_cols = list_of_rows_or_cols(m, margin = 2)
  )
  expect_equal(DF$extracted_rows, list(expected_margin_1, expected_margin_1))
  expect_equal(DF$extracted_cols, list(expected_margin_2, expected_margin_2))
})

test_that("organize_args works as expected", {
  # If only one argument is a list, make the other argument also a list of equal length.
  expect_equal(byname:::organize_args(a = list(1,2), b = 3), list(a = list(1,2), b = list(3,3)))
  expect_equal(byname:::organize_args(a = 3, b = list(1,2)), list(a = list(3,3), b = list(1,2)))
  
  # If both arguments are lists, ensure that they are same length.
  expect_equal(byname:::organize_args(a = list(1,2,3), b = list(4,5,6)), list(a = list(1,2,3), b = list(4,5,6)))
  expect_error(byname:::organize_args(a = list(1,2,3), b = list(4,5,6,7)), "length\\(a\\) == length\\(b\\) is not TRUE") 
  
  # If one argument is a matrix and the other is a constant, make the constant into a matrix.
  m <- matrix(c(1,2,3,4), nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(byname:::organize_args(a = m, b = 2), 
               list(a = m, b = matrix(2, nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries")))
  
  # Ensures that row and column types match
  # Completes and sorts the matrices
  n <- matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  # Neither row nor column types match, but error will say column types are mismatched.
  expect_error(byname:::organize_args(a = m %>% setrowtype("rows"), b = n), 
               "rowtype\\(a\\) \\(rows\\) != rowtype\\(b\\) \\(Products\\).")
  # By setting the rowtype to match, the error should shift to mismatched column types
  expect_error(byname:::organize_args(a = m, b = n %>% setcoltype("cols")), 
               "coltype\\(a\\) \\(Industries\\) != coltype\\(b\\) \\(cols\\).")
  # This should work, because the rowtype and coltype are now same for both
  expect_equal(byname:::organize_args(a = m, b = n), 
               list(a = matrix(c(1,3,
                                 2,4,
                                 0,0),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries"), 
                    b = matrix(c(1,4,
                                 2,5,
                                 3,6),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries")))
  
  # If one argument is a list and the other argument is a matrix, duplicate the matrix to match the length of the list
  expect_equal(byname:::organize_args(a = list(1, 2), b = m), list(a = list(1, 2), b = list(m, m)))
  expect_equal(byname:::organize_args(a = n, b = list(m, m)), list(a = list(n, n), b = list(m, m)))
  
  # Test the match_type argument
  p <- transpose_byname(n)
  # If we don't specify match_type = "matmult", 
  # organize_args will try to ensure that rows of m and rows of p are same type. 
  # organize_args will also try to ensure that cols of m and cols of p are same type.
  # These aren't true, so this will error.
  expect_error(byname:::organize_args(a = m, b = p), 
               "rowtype\\(a\\) \\(Products\\) != rowtype\\(b\\) \\(Industries\\).")
  # When we say match_type = "matmult", we indicate that the columns of a and the rows of b must match.
  expect_equal(byname:::organize_args(a = m, b = p, match_type = "matmult"), list(a = m, b = p))
})


###########################################################
context("Row and column names")
###########################################################

test_that("setrownames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m, c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("a", "b")) %>% rownames(), 
               c("a", "b"))
  expect_equal(m %>% setrownames_byname(rownames(m2)) %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("c", "d")) %>% rownames(), c("c", "d"))
  expect_null(m %>% setrownames_byname(NULL) %>% rownames())
  expect_null(m %>% setrownames_byname(NA) %>% rownames())
  # The function should convert the constant to a matrix and apply the row name
  expect_equal(2 %>% setrownames_byname("row"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(c("row"), NULL)))
})

test_that("setcolnames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  expect_equal(m %>% setcolnames_byname(c("a", "b", "c")) %>% colnames(), 
               c("a", "b", "c"))
  expect_equal(m %>% setcolnames_byname(c("d", "e", "f")) %>% colnames(), c("d", "e", "f"))
  expect_null(m %>% setcolnames_byname(NULL) %>% colnames())
  expect_null(m %>% setcolnames_byname(NA) %>% colnames())
  # The function should convert the constant to a matrix and apply the col name
  expect_equal(2 %>% setcolnames_byname("col"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(NULL, c("col"))))
})


###########################################################
context("Testing in a data frame")
###########################################################

test_that("matrix multiplied by a constant in a data frame works", {
  mats <- data.frame(
    matrix = c("A", "A", "A", "A"),
    row = c("p1", "p1", "p2", "p2"),
    col = c("i1", "i2", "i1", "i2"),
    vals = c(1, 3, 2, 4)
  ) %>% 
    mutate(
      rowtype = "Industries",
      coltype  = "Products"
    ) %>% 
    group_by(matrix) %>% 
    collapse_to_matrices(matnames = "matrix", values = "vals", 
                         rownames = "row", colnames = "col", 
                         rowtypes = "rowtype", coltypes = "coltype") %>% 
    rename(
      matrix.name = matrix,
      matrix = vals
    ) %>% 
    spread(key = matrix.name, value = matrix) %>% 
    # Duplicate the row to demonstrate byname operating simultaneously 
    # on all rows of the data frame.
    rbind(., .) %>% 
    mutate(
      constant = make_list(x = 1:2, n = 2, lenx = 2),
      # Multiplies matrices in the sum column by corresponding constants in the c column.
      product = elementproduct_byname(constant, A)
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


###########################################################
# context("Apply")
###########################################################

# test_that("apply_byname works as expected", {
#   apply_byname(FUN = sum, c1 = list(1, 2, 3), c2 = list(4,5,6))
# })
