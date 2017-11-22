# Contains tests for the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(parallel)
library(byname)
library(magrittr)
library(testthat)


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
context("Matrix products")
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




###########################################################
context("Row selection")
###########################################################

m_rownames <- paste0("i", 1:4)
m_colnames <- paste0("p", 1:4)
m <- matrix(1:16, ncol = 4, dimnames=list(m_rownames, m_colnames)) %>%
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
context("Row and column naming")
###########################################################

test_that("setting row col names works as expected", {
  mat1 <- matrix(c(0,1,0,1), nrow = 2, dimnames = list(c("r (1)", "r (2)"), c("c (1)", "c (2)")))
  mat1_rrenamed <- mat1 %>% setrownames_byname(c("r1", "r2"))
  expect_equal(rownames(mat1_rrenamed), c("r1", "r2"))
  
  mat1_crenamed <- mat1 %>% setcolnames_byname(c("c1", "c2"))
  expect_equal(colnames(mat1_crenamed), c("c1", "c2"))

  # Also works with lists in data frames.
  DF <- data.frame(m = I(list()))
  DF[[1, "m"]] <- mat1
  DF[[2, "m"]] <- mat1
  
  DF_renamed <- DF %>% 
    mutate(
      m_renamed = m %>% setrownames_byname(c("r1", "r2")) %>% setcolnames_byname(c("c1", "c2"))
    )
    
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
  expect_error(byname:::organize_args(a = m %>% setrowtype("rows"), b = n), "rowtype\\(a\\) == rowtype\\(b\\) is not TRUE")
  # By setting the rowtype to match, the error should shift to mismatched column types
  expect_error(byname:::organize_args(a = m, b = n %>% setcoltype("cols")), "coltype\\(a\\) == coltype\\(b\\) is not TRUE")
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
  expect_error(byname:::organize_args(a = m, b = p), "rowtype\\(a\\) == rowtype\\(b\\) is not TRUE")
  # When we say match_type = "matmult", we indicate that the columns of a and the rows of b must match.
  expect_equal(byname:::organize_args(a = m, b = p, match_type = "matmult"), list(a = m, b = p))
})