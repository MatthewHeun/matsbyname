# This file contains tests for functions in Utilities.R.

test_that("organize_args() generates errors when called with baloney", {
  expect_error(matsbyname:::organize_args(b = 42), 
               "Missing argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = NULL, b = 42), 
               "Null argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42), 
               "Missing argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42, b = NULL), 
               "Null argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(1), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1) %>% setcoltype("col"), 
                                          b = matrix(1) %>% setrowtype("bogus"), 
                                          match_type = "matmult"))
})


test_that("oddball match_type works as expected", {
  expect_equal(matsbyname:::organize_args(a = 1, b = 2, match_type = "none"), 
               list(a = 1, b = 2))
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_equal(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "none"),
               list(a = matrix(1), b = matrix(2)))
})


test_that("select_rows_byname() generates an error when no retain or remove patterns are default", {
  # Check with non-NULL values for a.
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_error(m %>% select_rows_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
  expect_error(m %>% select_cols_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
})


test_that("select_rows_byname() works with exact matches (^name$)", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  # Select only the first row (i1)
  expect_equal(select_rows_byname(m, retain_pattern = "^i1$"), 
               matrix(c(seq(1, 13, by = 4)), nrow = 1, dimnames = list(c("i1"), m_colnames)) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Try same test using the make_or_pattern utility function.
  expect_equal(select_rows_byname(m, retain_pattern = RCLabels::make_or_pattern(strings = "i1", pattern_type = "exact")), 
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
  # ( and ) needs to be escaped properly for use in regex.
  crazymat <- matrix(1, nrow = 2, ncol = 2, 
                     dimnames = list(c("i (1)", "i (2)"), c("p (1)", "p (2)"))) %>% 
    setrowtype("Industries") %>% setcoltype("Prodcuts")
  expect_equal(select_rows_byname(crazymat, retain_pattern = RCLabels::make_or_pattern(strings = "i (1)", pattern_type = "exact")), 
               matrix(1, nrow = 1, ncol = 2, dimnames = list("i (1)", c("p (1)", "p (2)"))) %>% 
                 setrowtype(rowtype(crazymat)) %>% setcoltype(coltype(crazymat)))
})


test_that("select_rows_byname() works with exact matches (^name$) for Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  M <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  
  # Select only the first row (i1)
  res <- select_rows_byname(M, retain_pattern = "^i1$")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(seq(1, 13, by = 4)), nrow = 1, dimnames = list(c("i1"), m_colnames)) %>% 
                                               setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
})


test_that("select_rows_byname() works with inexact matches", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  n1 <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
  
  # Matches first two rows, because partial match is OK.
  expect_equal(select_rows_byname(n1, retain_pattern = "^a"), 
               n1[c(1,2), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
  # Deletes first two rows, because partial match is OK, and first two row names start with "a".
  expect_equal(select_rows_byname(n1, remove_pattern = "^a"), 
               n1[c(3,4), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
})


test_that("select_rows_byname() works with inexact matches on Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  
  n1 <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
  
  # Matches first two rows, because partial match is OK.
  res1 <- select_rows_byname(n1, retain_pattern = "^a")
  expect_true(is.Matrix(res1))
  expect_equal(res1, 
               n1[c(1,2), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
  # Deletes first two rows, because partial match is OK, and first two row names start with "a".
  res2 <- select_rows_byname(n1, remove_pattern = "^a")
  matsbyname:::expect_equal_matrix_or_Matrix(res2, 
                                             n1[c(3,4), ] %>% setrowtype(rowtype(n1)) %>% setcoltype(coltype(n1)))
})


test_that("select_rows_byname() works with inexact matches and multiple selectors", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  n1 <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
  
  # The retain_pattern selects all rows whose names start with "a" or "b".
  # This approach should retain rows with names "a1", "a2", "b1", and "b2", i.e.,
  # all rows in n1.
  expect_equal(select_rows_byname(n1, retain_pattern = "^a|^b"), n1)
})


test_that("select_rows_byname() works with inexact matches and multiple selectors for Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname:::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                           rowtype = "Industries", coltype = "Products")
  n1 <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
  
  # The retain_pattern selects all rows whose names start with "a" or "b".
  # This approach should retain rows with names "a1", "a2", "b1", and "b2", i.e.,
  # all rows in n1.
  expect_equal(select_rows_byname(n1, retain_pattern = "^a|^b"), n1)
})


test_that("select_rows_byname() works in lists", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  # Use same row names for each item in the list
  expect_equal(select_rows_byname(list(m,m), retain_pattern = "^i1$|^i4$"),
               list(m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% dplyr::mutate(trimmed = select_rows_byname(.$m, 
                                                          retain_pattern = RCLabels::make_or_pattern(strings = c("i1", "i2"), 
                                                                                                     pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  DF_expected[[2,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equal(DF, DF_expected, ignore_attr = TRUE)
})


test_that("select_rows_byname() works in lists with Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  
  # Use same row names for each item in the list
  expect_equal(select_rows_byname(list(m,m), retain_pattern = "^i1$|^i4$"),
               list(m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[c(1,4), ] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% dplyr::mutate(trimmed = select_rows_byname(.$m, 
                                                          retain_pattern = RCLabels::make_or_pattern(strings = c("i1", "i2"), 
                                                                                                     pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  DF_expected[[2,"trimmed"]] <- select_rows_byname(m, retain_pattern = "^i1$|^i2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equal(DF, DF_expected, ignore_attr = TRUE)
})


test_that("select_cols_byname() works with exact matches (^name$)", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  # Select only the first column (p1)
  expect_equal(select_cols_byname(m, retain_pattern = "^p1$"), 
               matrix(1:4, ncol = 1, dimnames = list(m_rownames, c("p1"))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Try same test using the make_or_pattern utility function.
  expect_equal(select_cols_byname(m, retain_pattern = RCLabels::make_or_pattern(strings = "p1", pattern_type = "exact")), 
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


test_that("select_cols_byname() works with exact matches (^name$) and Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  
  # Select only the first column (p1)
  res1 <- select_cols_byname(m, retain_pattern = "^p1$")
  expect_true(is.Matrix(res1))
  matsbyname:::expect_equal_matrix_or_Matrix(res1, 
                                             matrix(1:4, ncol = 1, dimnames = list(m_rownames, c("p1"))) %>% 
                                               setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Try same test using the make_or_pattern utility function.
  res2 <- select_cols_byname(m, retain_pattern = RCLabels::make_or_pattern(strings = "p1", pattern_type = "exact"))
  matsbyname:::expect_equal_matrix_or_Matrix(res2, 
                                             matrix(1:4, ncol = 1, dimnames = list(m_rownames, c("p1"))) %>% 
                                               setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Select columns 1 and 4 (p1, p4)
  res3 <- select_cols_byname(m, retain_pattern = "^p1$|^p4$")
  matsbyname:::expect_equal_matrix_or_Matrix(res3, 
                                             m[ , c(1, 4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate column 3 (p3)
  res4 <- select_cols_byname(m, remove_pattern = "^p3$")
  matsbyname:::expect_equal_matrix_or_Matrix(res4, 
                                             m[ , -3] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Eliminate columns 1 and 3
  res5 <- select_cols_byname(m, remove_pattern = "^p1$|^p3$")
  matsbyname:::expect_equal_matrix_or_Matrix(res5, 
                                             m[ , c(-1,-3)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Retain column 4.  Retain has precedence over remove.
  res6 <- select_cols_byname(m, retain_pattern = "^p4$", remove_pattern = "^p1$|^p3$|^p4$")
  matsbyname:::expect_equal_matrix_or_Matrix(res6, 
               matrix(13:16, ncol = 1, dimnames = list(m_rownames, c("p4"))) %>% 
                 setrowtype(rowtype(m)) %>% setcoltype(coltype(m)))
  # Matches nothing.  NULL is returned.
  expect_null(select_cols_byname(m, retain_pattern = "^x$"))
  # Matches nothing.  All of m is returned.
  expect_equal(select_cols_byname(m, remove_pattern = "^x$"), m)
})


test_that("setcolnames_byname() works with inexact matches", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  n2 <- setcolnames_byname(m, c("a1", "a2", "b1", "b2")) 
  
  # Matches first two columns, because partial match is OK.
  expect_equal(select_cols_byname(n2, retain_pattern = "^a"), 
               n2[ , c(1,2)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
  # Deletes first two columns, because partial match is OK, and first two column names start with "a".
  expect_equal(select_cols_byname(n2, remove_pattern = "^a"), 
               n2[ , c(3,4)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
})


test_that("setcolnames_byname() works with inexact matches and Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  n2 <- setcolnames_byname(m, c("a1", "a2", "b1", "b2")) 
  
  # Matches first two columns, because partial match is OK.
  res1 <- select_cols_byname(n2, retain_pattern = "^a")
  expect_true(is.Matrix(res1))
  expect_equal(res1, 
               n2[ , c(1,2)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
  # Deletes first two columns, because partial match is OK, and first two column names start with "a".
  matsbyname:::expect_equal_matrix_or_Matrix(select_cols_byname(n2, remove_pattern = "^a"), 
                                             n2[ , c(3,4)] %>% setrowtype(rowtype(n2)) %>% setcoltype(coltype(n2)))
})


test_that("select_cols_byname(0 works with inexact matches and multiple selectors", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  n2 <- setcolnames_byname(m, c("a1", "a2", "b1", "b2")) 
  
  # The retain_pattern selects all columns whose names start with "a" or "b".
  # This approach should retain columns with names "a1", "a2", "b1", and "b2", i.e.,
  # all columns in n2.
  expect_equal(select_cols_byname(n2, retain_pattern = "^a|^b"), n2)
})


test_that("select_cols_byname(0 works with inexact matches and multiple selectors for Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  n2 <- setcolnames_byname(m, c("a1", "a2", "b1", "b2")) 
  
  # The retain_pattern selects all columns whose names start with "a" or "b".
  # This approach should retain columns with names "a1", "a2", "b1", and "b2", i.e.,
  # all columns in n2.
  expect_equal(select_cols_byname(n2, retain_pattern = "^a|^b"), n2)
})


test_that("select_cols_byname() works in lists", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matrix(1:16, ncol = 4, dimnames = list(m_rownames, m_colnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  
  # Use same column names for each item in the list
  expect_equal(select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$"),
               list(m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% dplyr::mutate(trimmed = select_cols_byname(.$m, 
                                                          retain_pattern = RCLabels::make_or_pattern(strings = c("p1", "p2"), 
                                                                                                     pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  DF_expected[[2,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equal(DF, DF_expected, ignore_attr = TRUE)
})


test_that("select_cols_byname() works in lists for Matrix objects", {
  m_rownames <- paste0("i", 1:4)
  m_colnames <- paste0("p", 1:4)
  m <- matsbyname::Matrix(1:16, nrow = 4, ncol = 4, dimnames = list(m_rownames, m_colnames), 
                          rowtype = "Industries", coltype = "Products")
  
  # Use same column names for each item in the list
  res1 <- select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$")
  expect_true(is.Matrix(res1[[1]]))
  expect_true(is.Matrix(res1[[2]]))
  expect_equal(select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$"),
               list(m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m)), 
                    m[ , c(1,4)] %>% setrowtype(rowtype(m)) %>% setcoltype(coltype(m))))
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% dplyr::mutate(trimmed = select_cols_byname(.$m, 
                                                          retain_pattern = RCLabels::make_or_pattern(strings = c("p1", "p2"), 
                                                                                                     pattern_type = "exact")))
  DF_expected <- data.frame(m = I(list()), trimmed = I(list()))
  DF_expected[[1,"m"]] <- m
  DF_expected[[2,"m"]] <- m
  DF_expected[[1,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  DF_expected[[2,"trimmed"]] <- select_cols_byname(m, retain_pattern = "^p1$|^p2$")
  # Need to use "expect_equivalent" because attributes are different 
  # because DF_expected was made differently from how the mutated data fram was made.
  expect_equal(DF, DF_expected, ignore_attr = TRUE)
})


test_that("select_rows_byname() works even when everything is removed", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try to remove all rows
  expect_null(select_rows_byname(m, remove_pattern = "^r"))
  # Try to remove all columns
  expect_null(select_cols_byname(m, remove_pattern = "^c"))
})


test_that("select_rows_byname() works even when everything is removed with Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, 
                          dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                          rowtype = "rows", coltype = "cols")
  # Try to remove all rows
  expect_null(select_rows_byname(m, remove_pattern = "^r"))
  # Try to remove all columns
  expect_null(select_cols_byname(m, remove_pattern = "^c"))
})


test_that("select_rows_byname() works even when there is a NULL situation", {
  # Check the degenerate condition.
  expect_null(select_rows_byname(a = NULL))
  expect_null(select_cols_byname(a = NULL))
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% select_rows_byname(retain_pattern = "r1"), 
               matrix(c(1, 3), ncol = 2, dimnames = list("r1", c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_rows_byname(retain_pattern = "r3"))
  # Try with columns
  expect_equal(m %>% select_cols_byname(retain_pattern = "c1"), 
               matrix(1:2, nrow = 2, dimnames = list(c("r1", "r2"), "c1")) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_cols_byname(retain_pattern = "c3"))
})


test_that("select_rows_byname() works even when there is a NULL situation with Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, 
                          dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                          rowtype = "rows", coltype = "cols")
  # Try with rows
  expect_equal(m %>% select_rows_byname(retain_pattern = "r1"), 
               matsbyname::Matrix(c(1, 3), nrow = 1, ncol = 2,
                                  dimnames = list("r1", c("c1", "c2")), 
                                  rowtype = "rows", coltype = "cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_rows_byname(retain_pattern = "r3"))
  # Try with columns
  expect_equal(m %>% select_cols_byname(retain_pattern = "c1"), 
               matsbyname::Matrix(1:2, nrow = 2, ncol = 1,
                                  dimnames = list(c("r1", "r2"), "c1"), 
                                  rowtype = "rows", coltype = "cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_cols_byname(retain_pattern = "c3"))
})


test_that("select_rowcol_piece_byname() works for selecting rows", {
  expect_null(select_rowcol_piece_byname(a = NULL))
  
  # Retain a row by its noun.
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expected_1 <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", piece = "noun", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_1, expected_1)
  
  # Retain a row by a preposition.
  expected_2 <- matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  # The next line needs to have bracket_notation.
  # If from_notation is used, "from" is not a valid piece.
  res_2 <- select_rowcol_piece_byname(m_1, retain = "b", piece = "from", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_2, expected_2)
  
  # Retain a row by prefix. Should give the same result as expected_2
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_3, expected_2)
  
  # Retain a row by suffix
  res_4 <- select_rowcol_piece_byname(m_1, retain = "a", piece = "suff", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_4, expected_1)
  
  # Retain a row by prefix
  res_5 <- select_rowcol_piece_byname(m_1, retain = "r2", piece = "pref", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_5, expected_2)
  
  # Replace based on pattern type
  res_6 <- select_rowcol_piece_byname(m_1, retain = "2", piece = "noun",
                                      pattern_type = "trailing", 
                                      notation = RCLabels::bracket_notation, 
                                      margin = 1)
  expect_equal(res_6, expected_2)
})


test_that("select_rowcol_piece_byname() works for selecting rows for Matrix objects", {
  # Retain a row by its noun.
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  expected_1 <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", piece = "noun", notation = RCLabels::from_notation, margin = 1)
  expect_true(is.Matrix(res_1))
  expect_equal(res_1, expected_1)
  
  # Retain a row by a preposition.
  expected_2 <- matsbyname::Matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  
  # The next line needs to have bracket_notation.
  # If from_notation is used, "from" is not a valid piece.
  res_2 <- select_rowcol_piece_byname(m_1, retain = "b", piece = "from", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_2, expected_2)
  
  # Retain a row by prefix. Should give the same result as expected_2
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_3, expected_2)
  
  # Retain a row by suffix
  res_4 <- select_rowcol_piece_byname(m_1, retain = "a", piece = "suff", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_4, expected_1)
  
  # Retain a row by prefix
  res_5 <- select_rowcol_piece_byname(m_1, retain = "r2", piece = "pref", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_5, expected_2)
  
  # Replace based on pattern type
  res_6 <- select_rowcol_piece_byname(m_1, retain = "2", piece = "noun",
                                      pattern_type = "trailing", 
                                      notation = RCLabels::bracket_notation, 
                                      margin = 1)
  expect_equal(res_6, expected_2)
})


test_that("select_rowcol_piece_byname() works for removing rows from Matrix objects", {
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  # Remove a row by prefix
  res_1 <- select_rowcol_piece_byname(m_1, remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expected_1 <- matsbyname::Matrix(3:4, nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_1, expected_1)
  
  # Remove a row by suffix
  res_2 <- select_rowcol_piece_byname(m_1, remove = "b", piece = "suff", notation = RCLabels::from_notation, margin = 1)
  expected_2 <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_2, expected_2)
  
  # Remove a row by preposition
  res_3 <- select_rowcol_piece_byname(m_1, remove = "a", piece = "from", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_3, expected_1)
  
  # Remove a row by noun
  res_4 <- select_rowcol_piece_byname(m_1, remove = "r1", piece = "noun", notation = RCLabels::bracket_notation, margin = 1)
  expect_equal(res_4, expected_1)
})


test_that("select_rowcol_piece_byname() works with both retain and remove", {
  # Retain and remove at same time.  Retain should take precedence.
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  # Remove a row by prefix
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_1 <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(res_1, expected_1)
  
  res_2 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_2, expected_1)
  
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_2 <- matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(res_3, expected_2)
})


test_that("select_rowcol_piece_byname() works with both retain and remove for Matrix objects", {
  # Retain and remove at same time.  Retain should take precedence.
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  # Remove a row by prefix
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_true(is.Matrix(res_1))
  expected_1 <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  
  expect_equal(res_1, expected_1)
  
  res_2 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_2, expected_1)
  
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_2 <- matsbyname::Matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  
  expect_equal(res_3, expected_2)
})


test_that("select_rowcol_piece_byname() works with both retain and remove", {
  # Retain and remove at same time.  Retain should take precedence.
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  # Remove a row by prefix
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_1 <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(res_1, expected_1)
  
  res_2 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_2, expected_1)
  
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_2 <- matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(res_3, expected_2)
})


test_that("select_rowcol_piece_byname() works with both retain and remove for Matrix objects", {
  # Retain and remove at same time.  Retain should take precedence.
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  # Remove a row by prefix
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_true(is.Matrix(res_1))  
  expected_1 <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_1, expected_1)
  
  res_2 <- select_rowcol_piece_byname(m_1, retain = "r1", remove = "r2", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  expect_equal(res_2, expected_1)
  
  res_3 <- select_rowcol_piece_byname(m_1, retain = "r2", remove = "r1", piece = "pref", notation = RCLabels::from_notation, margin = 1)
  
  expected_2 <- matsbyname::Matrix(c(3,4), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r2 [from b]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_3, expected_2)
})


test_that("select_rowcol_piece_byname() works when selecting columns", {
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  res_1 <- select_rowcol_piece_byname(m_1, retain = "c1", piece = "noun", 
                                      notation = RCLabels::from_notation, margin = 2)  
  
  expected_1 <- matrix(c(1, 3), nrow = 2, ncol = 1, byrow = TRUE, 
                       dimnames = list(c("r1 [from a]", "r2 [from b]"), "c1 [from c]")) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(res_1, expected_1)
  
  # Deselect a column
  res_2 <- select_rowcol_piece_byname(m_1, remove = "c2", piece = "noun", 
                                      notation = RCLabels::bracket_notation, margin = 2)  
  expect_equal(res_2, expected_1)
})


test_that("select_rowcol_piece_byname() works when selecting columns in Matrix objects", {
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  res_1 <- select_rowcol_piece_byname(m_1, retain = "c1", piece = "noun", 
                                      notation = RCLabels::from_notation, margin = 2)  
  expect_true(is.Matrix(res_1))
  expected_1 <- matsbyname::Matrix(c(1, 3), nrow = 2, ncol = 1, byrow = TRUE, 
                                   dimnames = list(c("r1 [from a]", "r2 [from b]"), "c1 [from c]"), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_1, expected_1)
  
  # Deselect a column
  res_2 <- select_rowcol_piece_byname(m_1, remove = "c2", piece = "noun", 
                                      notation = RCLabels::bracket_notation, margin = 2)  
  expect_equal(res_2, expected_1)
})


test_that("select_rowcol_piece_byname() examples work as intended", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                          c("c1 [from c]", "c2 [from d]")), 
                          rowtype = "rows", coltype = "cols")
  res <- select_rowcol_piece_byname(m, retain = "r1", piece = "noun", 
                                    notation = RCLabels::to_notation, 
                                    margin = 1)
  expect_true(is.Matrix(res))
  expected <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                     dimnames = list("r1 [to a]", c("c1 [from c]", "c2 [from d]")), 
                     rowtype = "rows", coltype = "cols")
  expect_equal(res, expected)
  
  res_2 <- select_rowcol_piece_byname(m, retain = "c", piece = "from", 
                                      notation = RCLabels::bracket_notation, 
                                      margin = 2)
  expected_2 <- matsbyname::Matrix(c(1,3), nrow = 2, ncol = 1, byrow = TRUE, 
                                   dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                                   c("c1 [from c]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res_2, expected_2)
})


test_that("select_rowcol_piece_byname() works when specifying both rows and cols", {
  m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                              c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Use default margin (c(1,2)).
  res <- select_rowcol_piece_byname(m, retain = c("r1", "c1", "r3", "c3"), piece = "noun", 
                                    notation = RCLabels::bracket_notation)
  expected <- matrix(1, nrow = 1, ncol = 1, 
                     dimnames = list("r1 [to a]", "c1 [from c]")) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  expect_equal(res, expected)
})


test_that("select_rowcol_piece_byname() works when specifying both rows and cols in Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                          c("c1 [from c]", "c2 [from d]")), 
                          rowtype = "rows", coltype = "cols")
  # Use default margin (c(1,2)).
  res <- select_rowcol_piece_byname(m, retain = c("r1", "c1", "r3", "c3"), piece = "noun", 
                                    notation = RCLabels::bracket_notation)
  expect_true(is.Matrix(res))
  expected <- matsbyname::Matrix(1, nrow = 1, ncol = 1, 
                                 dimnames = list("r1 [to a]", "c1 [from c]"), 
                                 rowtype = "rows", coltype = "cols")
  expect_equal(res, expected)
})


test_that("select_rowcol_piece_byname() works in a list and a data frame", {
  m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                              c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  n <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                              c("c1 [from d]", "c2 [from e]", "c3 [from f]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  # Try in a list
  res <- list(m, n) %>% 
    select_rowcol_piece_byname(retain = c("r1", "r2"), piece = "noun", notation = RCLabels::bracket_notation, margin = 1)
  expected_m <- m # No change, because only rows r1 and r2
  # Eliminates r3.
  expected_n <- matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE, 
                       dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                       c("c1 [from d]", "c2 [from e]", "c3 [from f]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(res, list(expected_m, expected_n))
  
  # Try in a data frame.
  res_df <- tibble::tibble(mats = list(m, n)) %>% 
    dplyr::mutate(
      r1r2 = select_rowcol_piece_byname(mats, retain = c("r1", "r2"), piece = "noun", notation = RCLabels::bracket_notation, margin = 1)
    )
  expect_equal(res_df$r1r2, list(expected_m, expected_n))
})


test_that("select_rowcol_piece_byname() works in a list and a data frame of Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                          c("c1 [from c]", "c2 [from d]")), 
                          rowtype = "rows", coltype = "cols")
  n <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                                          c("c1 [from d]", "c2 [from e]", "c3 [from f]")), 
                          rowtype = "rows", coltype = "cols")
  
  # Try in a list
  res <- list(m, n) %>% 
    select_rowcol_piece_byname(retain = c("r1", "r2"), piece = "noun", notation = RCLabels::bracket_notation, margin = 1)
  expect_true(all(sapply(res, is.Matrix)))
  expected_m <- m # No change, because only rows r1 and r2
  # Eliminates r3.
  expected_n <- matsbyname::Matrix(1:6, nrow = 2, ncol = 3, byrow = TRUE, 
                                   dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                                   c("c1 [from d]", "c2 [from e]", "c3 [from f]")), 
                                   rowtype = "rows", coltype = "cols")
  expect_equal(res, list(expected_m, expected_n))
  
  # Try in a data frame.
  res_df <- tibble::tibble(mats = list(m, n)) %>% 
    dplyr::mutate(
      r1r2 = select_rowcol_piece_byname(mats, retain = c("r1", "r2"), piece = "noun", notation = RCLabels::bracket_notation, margin = 1)
    )
  expect_equal(res_df$r1r2, list(expected_m, expected_n))
})


test_that("select_rowcol_piece_byname() interprets margins correctly", {
  m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                              c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  n <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                              c("c1 [from d]", "c2 [from e]", "c3 [from f]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expected_m <- matrix(c(2,4), nrow = 2, ncol = 1, byrow = TRUE, 
                       dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                       "c2 [from d]")) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected_n <- matrix(c(1, 4, 7), nrow = 3, ncol = 1, byrow = TRUE, 
                       dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                                       "c1 [from d]")) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  res_df <- tibble::tibble(mats = list(m, n)) %>% 
    dplyr::mutate(
      res_col = select_rowcol_piece_byname(mats, retain = "d", piece = "from", notation = RCLabels::bracket_notation, margin = "cols")
    )
  expect_equal(res_df$res_col, list(expected_m, expected_n))
  
  res_df_2 <- tibble::tibble(mats = list(m, transpose_byname(n))) %>% 
    dplyr::mutate(
      # Picks up the correct margin with the string.
      res_col = select_rowcol_piece_byname(mats, retain = "d", piece = "from", notation = RCLabels::bracket_notation, margin = "cols")
    )
  expect_equal(res_df_2$res_col, list(expected_m, transpose_byname(expected_n)))
})


test_that("select_rowcol_piece_byname() interprets margins correctly for Matrix objects", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                          c("c1 [from c]", "c2 [from d]")), 
                          rowtype = "rows", coltype = "cols")
  n <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                                          c("c1 [from d]", "c2 [from e]", "c3 [from f]")), 
                          rowtype = "rows", coltype = "cols")
  
  expected_m <- matsbyname::Matrix(c(2,4), nrow = 2, ncol = 1, byrow = TRUE, 
                                   dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                                                   "c2 [from d]"), 
                                   rowtype = "rows", coltype = "cols")
  expected_n <- matsbyname::Matrix(c(1, 4, 7), nrow = 3, ncol = 1, byrow = TRUE, 
                                   dimnames = list(c("r1 [to a]", "r2 [to b]", "r3 [to c]"), 
                                                   "c1 [from d]"), 
                                   rowtype = "rows", coltype = "cols")
  
  res_df <- tibble::tibble(mats = list(m, n)) %>% 
    dplyr::mutate(
      res_col = select_rowcol_piece_byname(mats, retain = "d", piece = "from", notation = RCLabels::bracket_notation, margin = "cols")
    )
  expect_equal(res_df$res_col, list(expected_m, expected_n))
  
  res_df_2 <- tibble::tibble(mats = list(m, transpose_byname(n))) %>% 
    dplyr::mutate(
      # Picks up the correct margin with the string.
      res_col = select_rowcol_piece_byname(mats, retain = "d", piece = "from", notation = RCLabels::bracket_notation, margin = "cols")
    )
  expect_true(all(sapply(res_df_2$res_col, is.Matrix)))
  expect_equal(res_df_2$res_col, list(expected_m, transpose_byname(expected_n)))
})


test_that("select_rowcol_piece_byname() works with notation inference", {
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expected_1 <- matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                       dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]"))) %>% 
    matsbyname::setrowtype("rows") %>% setcoltype("cols")
  
  # Don't specify notation to force inference.
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", piece = "noun", margin = 1)
  expect_equal(res_1, expected_1)
})


test_that("select_rowcol_piece_byname() works with notation inference in Matrix objects", {
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  
  expected_1 <- matsbyname::Matrix(c(1,2), nrow = 1, ncol = 2, byrow = TRUE, 
                                   dimnames = list("r1 [from a]", c("c1 [from c]", "c2 [from d]")), 
                                   rowtype = "rows", coltype = "cols")
  
  # Don't specify notation to force inference.
  res_1 <- select_rowcol_piece_byname(m_1, retain = "r1", piece = "noun", margin = 1)
  expect_true(is.Matrix(res_1))
  expect_equal(res_1, expected_1)
})


test_that("select_rowcol_piece_byname() works when all rows or all cols are removed", {
  m_1 <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res_1 <- select_rowcol_piece_byname(m_1, retain = "bogus", piece = "noun", margin = 1)
  expect_null(res_1)
  res_2 <- select_rowcol_piece_byname(m_1, retain = "bogus", piece = "all", margin = 2)
  expect_null(res_2)
  
  # Try in a data frame
  df <- tibble::tibble(m = list(m_1, m_1)) %>% 
    dplyr::mutate(
      res = select_rowcol_piece_byname(m, retain = "bogus", piece = "all", margin = 1)
    )
  expect_null(df$res[[1]])
  expect_null(df$res[[2]])
})


test_that("select_rowcol_piece_byname() works when all rows or all cols are removed in a Matrix object", {
  m_1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
                            dimnames = list(c("r1 [from a]", "r2 [from b]"), c("c1 [from c]", "c2 [from d]")), 
                            rowtype = "rows", coltype = "cols")
  res_1 <- select_rowcol_piece_byname(m_1, retain = "bogus", piece = "noun", margin = 1)
  expect_null(res_1)
  res_2 <- select_rowcol_piece_byname(m_1, retain = "bogus", piece = "all", margin = 2)
  expect_null(res_2)
  
  # Try in a data frame
  df <- tibble::tibble(m = list(m_1, m_1)) %>% 
    dplyr::mutate(
      res = select_rowcol_piece_byname(m, retain = "bogus", piece = "all", margin = 1)
    )
  expect_null(df$res[[1]])
  expect_null(df$res[[2]])
})


test_that("setrownames_byname() and setcolnames_byname() works even when there is a NULL situation", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% setrownames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setrownames_byname(NULL, c("a", "b")))
  # Try with columns
  expect_equal(m %>% setcolnames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("a", "b"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setcolnames_byname(NULL, c("a", "b")))
})


test_that("setrownames_byname() and setcolnames_byname() works even when there is a NULL situation", {
  m <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                          rowtype = "rows", coltype = "cols")
  # Try with rows
  expected1 <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("c1", "c2")), 
                                  rowtype = "rows", coltype = "cols")
  res1 <- m %>%
    setrownames_byname(c("a", "b"))
  expect_true(is.Matrix(res1))
  matsbyname:::expect_equal_matrix_or_Matrix(res1, expected1)
  # Try with columns
  expected2 <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("a", "b"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res2 <- m %>%
    setcolnames_byname(c("a", "b"))
  expect_true(is.Matrix(res2))
  matsbyname:::expect_equal_matrix_or_Matrix(res2, expected2)
})


test_that("clean_byname() works with bad margins", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(clean_byname(m, margin = 42), 
               "margin = 42 in clean_byname\\(\\). Must be 1 or 2.")
  
})


test_that("clean_byname() works with bad margins on a Matrix", {
  m <- matsbyname::Matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(clean_byname(m, margin = 42), 
               "margin = 42 in clean_byname\\(\\). Must be 1 or 2.")
})


test_that("clean_byname() works for both rows and cols", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(clean_byname(m), 
               matrix(1:3, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "c2")))
})


test_that("clean_byname() works for both rows and cols in a Matrix", {
  m <- matsbyname::Matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(clean_byname(m), 
                                             matrix(1:3, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "c2")))
})


test_that("clean_byname() works for a vector", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4"), c("c1")))
  expect_equal(clean_byname(v), 
               matrix(42, dimnames = list(c("r4"), c("c1"))))
})


test_that("clean_byname() works for a vector Matrix object", {
  v <- matsbyname::Matrix(c(0, 
                            0, 
                            0, 
                            42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4"), c("c1")))
  res <- clean_byname(v)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, matrix(42, dimnames = list(c("r4"), c("c1"))))
})



test_that("clean_byname() works with unnamed rows and/or columns", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4")))
  expect_equal(clean_byname(v), 
               matrix(42, nrow = 1, byrow = TRUE, dimnames = list(c("r4"), c(NULL))))
  
  
  unnamed <- matrix(c(1, 2, 0,
                      3, 4, 0,
                      5, 6, 0,
                      0, 0, 0), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed), matrix(c(1, 2,
                                               3, 4, 
                                               5, 6), nrow = 3, byrow = TRUE))
})


test_that("clean_byname() works with unnamed rows and/or columns that are Matrix objects", {
  v <- matsbyname::Matrix(c(0, 
                            0, 
                            0, 
                            42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4"), NULL))
  matsbyname:::expect_equal_matrix_or_Matrix(clean_byname(v), 
                                             matrix(42, nrow = 1, byrow = TRUE, dimnames = list(c("r4"), c(NULL))))
  
  
  unnamed <- matsbyname::Matrix(c(1, 2, 0,
                                  3, 4, 0,
                                  5, 6, 0,
                                  0, 0, 0), nrow = 4, ncol = 3, byrow = TRUE)
  matsbyname:::expect_equal_matrix_or_Matrix(clean_byname(unnamed), matrix(c(1, 2,
                                                                             3, 4, 
                                                                             5, 6), nrow = 3, byrow = TRUE))
})


test_that("clean_byname() works with tolerance", {
  unnamed <- matrix(c(1, 2, 0.1,
                      3, 4, -0.1,
                      5, 6, 0.05,
                      0.01, -0.01, -0.05), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed, tol = 0.1), matrix(c(1, 2,
                                                          3, 4, 
                                                          5, 6), nrow = 3, byrow = TRUE))
  # Tighten tolerance to get different result.
  expect_equal(clean_byname(unnamed, tol = 0.0999), matrix(c(1, 2, 0.1, 
                                                             3, 4, -0.1,
                                                             5, 6, 0.05), nrow = 3, byrow = TRUE))
})


test_that("clean_byname() works with tolerance for Matrix objects", {
  unnamed <- matsbyname::Matrix(c(1, 2, 0.1,
                                  3, 4, -0.1,
                                  5, 6, 0.05,
                                  0.01, -0.01, -0.05), nrow = 4, ncol = 3, byrow = TRUE)
  res <- clean_byname(unnamed, tol = 0.1)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, matrix(c(1, 2,
                                                           3, 4, 
                                                           5, 6), nrow = 3, byrow = TRUE))
  # Tighten tolerance to get different result.
  res2 <- clean_byname(unnamed, tol = 0.0999)
  matsbyname:::expect_equal_matrix_or_Matrix(res2, matrix(c(1, 2, 0.1, 
                                                            3, 4, -0.1,
                                                            5, 6, 0.05), nrow = 3, byrow = TRUE))
})


test_that("iszero_byname() works as expected", {
  m <- matrix(0, nrow = 3, ncol = 2)
  expect_true(iszero_byname(m))
  n <- matrix(1, nrow = 42, ncol = 5)
  expect_false(iszero_byname(n))
})


test_that("iszero_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(0, nrow = 3, ncol = 2)
  expect_true(iszero_byname(m))
  n <- matsbyname::Matrix(1, nrow = 42, ncol = 5)
  expect_false(iszero_byname(n))
})


test_that("selectzerorows_byname() works as expected", {
  expect_null(selectzerorows_byname(NULL))
  fail <- matrix(c(1, 0, 1,
                   1, 0, 1),
                 dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                 nrow = 2, ncol = 3, byrow = TRUE) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected_fail <- fail[0, , drop = FALSE] %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerorows_byname(fail), expected_fail)
  
  succeed <- matrix(c(0, 0, 1,
                      0, 0, 0), 
                    dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                    nrow = 2, ncol = 3, byrow = TRUE) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected_succeed <- succeed[2, , drop = FALSE] %>%
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerorows_byname(succeed), expected_succeed)
})


test_that("selectzerorows_byname() works as expected with Matrix objects", {
  fail <- matsbyname::Matrix(c(1, 0, 1,
                               1, 0, 1),
                             dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                             nrow = 2, ncol = 3, byrow = TRUE, 
                             rowtype = "rows", coltype = "cols")
  expect_true(is.Matrix(fail))
  expected_fail <- fail[0, , drop = FALSE] %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerorows_byname(fail), expected_fail)
  
  succeed <- matsbyname::Matrix(c(0, 0, 1,
                                  0, 0, 0), 
                                dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                                nrow = 2, ncol = 3, byrow = TRUE,
                                rowtype = "rows", coltype = "cols")
  expected_succeed <- succeed[2, , drop = FALSE] %>%
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerorows_byname(succeed), expected_succeed)
})


test_that("selectzerocols_byname() works as expected", {
  expect_null(selectzerocols_byname(NULL))
  m <- matrix(c(1, 0, 1,
                1, 0, 1),
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
              nrow = 2, ncol = 3, byrow = TRUE) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected_m <- m[ , 2, drop = FALSE] %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerocols_byname(m), expected_m)
  
  m2 <- matrix(c(0, 0, 1,
                 0, 0, 0), 
               dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
               nrow = 2, ncol = 3, byrow = TRUE) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected_m2 <- m2[, 1:2, drop = FALSE] %>%
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerocols_byname(m2), expected_m2)
})


test_that("selectzerocols_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1, 0, 1,
                            1, 0, 1),
                          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                          nrow = 2, ncol = 3, byrow = TRUE, 
                          rowtype = "rows", coltype = "cols")
  expected_m <- m[ , 2, drop = FALSE] %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res <- selectzerocols_byname(m)
  expect_true(is.Matrix(res))
  expect_equal(res, expected_m)
  
  m2 <- matsbyname::Matrix(c(0, 0, 1,
                             0, 0, 0), 
                           dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                           nrow = 2, ncol = 3, byrow = TRUE, 
                           rowtype = "rows", coltype = "cols")
  expected_m2 <- m2[, 1:2, drop = FALSE] %>%
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(selectzerocols_byname(m2), expected_m2)
})


test_that("getzerorowcolnames_byname() works as expected", {
  m <- matrix(c(1, 0, 1,
                1, 0, 1),
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
              nrow = 2, ncol = 3, byrow = TRUE) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(getzerorowcolnames_byname(m), "c2")
  expect_equal(getzerorowcolnames_byname(list(m, m)), list("c2", "c2"))
  
  m2 <- matrix(c(1, 0, 1,
                 1, 0, 0, 
                 0, 0, 0),
               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")), 
               nrow = 3, ncol = 3, byrow = TRUE)
  expect_equal(getzerorowcolnames_byname(m2), c("r3", "c2"))
  expect_equal(getzerorowcolnames_byname(list(m2, m2)), list(c("r3", "c2"), c("r3", "c2")))
})


test_that("getzerorowcolnames_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1, 0, 1,
                            1, 0, 1),
                          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
                          nrow = 2, ncol = 3, byrow = TRUE, 
                          rowtype = "rows", coltype = "cols")
  res <- getzerorowcolnames_byname(m)
  expect_equal(res, "c2")
  expect_equal(getzerorowcolnames_byname(list(m, m)), list("c2", "c2"))
  
  m2 <- matsbyname::Matrix(c(1, 0, 1,
                             1, 0, 0, 
                             0, 0, 0),
                           dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")), 
                           nrow = 3, ncol = 3, byrow = TRUE)
  expect_equal(getzerorowcolnames_byname(m2), c("r3", "c2"))
  expect_equal(getzerorowcolnames_byname(list(m2, m2)), list(c("r3", "c2"), c("r3", "c2")))
})


test_that("getrownames_byname() works as expected", {
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


test_that("getrownames_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3, 
                          dimnames = list(paste0("i", 1:2), paste0("p", 1:3)), 
                          rowtype = "Industries", coltype = "Products")
  expect_equal(getrownames_byname(m), c("i1", "i2"))
  # This also works for lists
  expect_equal(getrownames_byname(list(m,m)), list(c("i1", "i2"), c("i1", "i2")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getrownames_byname(DF$m), list(c("i1", "i2"), c("i1", "i2")))
})


test_that("getcolnames_byname() works as expected", {
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


test_that("getcolnames_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3,
                          dimnames = list(paste0("i", 1:2), paste0("p", 1:3)), 
                          rowtype = "Industries", coltype = "Products")
  expect_equal(getcolnames_byname(m), c("p1", "p2", "p3"))
  # This also works for lists
  expect_equal(getcolnames_byname(list(m,m)), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getcolnames_byname(DF$m), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
})


test_that("setrownames_byname() works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m, c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("a", "b")) %>% rownames(), 
               c("a", "b"))
  expect_equal(m %>% setrownames_byname(rownames(m2)) %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("c", "d")) %>% rownames(), c("c", "d"))
  expect_null(m %>% setrownames_byname(NULL) %>% rownames())
  expect_equal(m %>% setrownames_byname(c(NA, NA)) %>% rownames(), c(NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the row name
  expect_equal(2 %>% setrownames_byname("row"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(c("row"), NULL)))
})


test_that("setrownames_byname() works on Matrix objects", {
  m <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3,
                          dimnames = list(paste0("i", 1:2), paste0("c", 1:3)), 
                          rowtype = "Industries", coltype = "Commodities")
  m2 <- setrownames_byname(m, c("a", "b"))
  expect_true(is.Matrix(m2))
  expect_equal(m2 %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(rownames(m2)) %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("c", "d")) %>% rownames(), c("c", "d"))
  expect_null(m %>% setrownames_byname(NULL) %>% rownames())
  expect_equal(m %>% setrownames_byname(c(NA, NA)) %>% rownames(), c(NA_character_, NA_character_))
})


test_that("setcolnames_byname() works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  expect_equal(m %>% setcolnames_byname(c("a", "b", "c")) %>% colnames(), 
               c("a", "b", "c"))
  expect_equal(m %>% setcolnames_byname(c("d", "e", "f")) %>% colnames(), c("d", "e", "f"))
  expect_null(m %>% setcolnames_byname(NULL) %>% colnames())
  expect_equal(m %>% setcolnames_byname(c(NA, NA, NA)) %>% colnames(), c(NA_character_, NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the col name
  expect_equal(2 %>% setcolnames_byname("col"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(NULL, c("col"))))
})


test_that("setcolnames_byname() works as expected", {
  m <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3, 
                          dimnames = list(paste0("i", 1:2), paste0("c", 1:3)), 
                          rowtype = "Industries", coltype = "Commodities")
  res <- m %>% setcolnames_byname(c("a", "b", "c"))
  expect_true(is.Matrix(res))
  expect_equal(res %>% colnames(), 
               c("a", "b", "c"))
  expect_equal(m %>% setcolnames_byname(c("d", "e", "f")) %>% colnames(), c("d", "e", "f"))
  expect_null(m %>% setcolnames_byname(NULL) %>% colnames())
  expect_equal(m %>% setcolnames_byname(c(NA, NA, NA)) %>% colnames(), c(NA_character_, NA_character_, NA_character_))
})


test_that("setrownames_byname() works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m1, c("a", "b"))
  expect_equal(rownames(m2), c("a", "b"))
  m3 <- setrownames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
  expect_equal(rownames(m3), c("c", "d"))
  m4 <- m1 %>% setrownames_byname(NULL)
  expect_null(rownames(m4))
  m5 <- m1 %>% setrownames_byname(c(NA, NA))
  expect_equal(rownames(m5), c(NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setrownames_byname(l1, rownames = list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without using a named argument (rownames) to see if they are inferred
  l2 <- setrownames_byname(l1, list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without a list. This should fail, because a is applied to the first matrix and b is applied to the second matrix.
  # But each matrix needs 2 rownames.
  expect_error(setrownames_byname(l1, c("a", "b")), 
               "length of 'dimnames' \\[1\\] not equal to array extent")
  
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r1", "r2")))
    )
  expect_equal(rownames(DF2$mcol2[[1]]), c("r1", "r2"))
  expect_equal(rownames(DF2$mcol2[[2]]), c("r1", "r2"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r3", "r4")))
    )
  expect_equal(list(rownames(DF3$mcol2[[1]]), rownames(DF3$mcol2[[2]])), list(c("r3", "r4"), c("r3", "r4")))
})


test_that("setrownames_byname() works with Matrix objects", {
  m1 <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3,
                           dimnames = list(paste0("i", 1:2), paste0("p", 1:3)), 
                           rowtype = "Industries", coltype = "Commodities")
  m2 <- setrownames_byname(m1, c("a", "b"))
  expect_true(is.Matrix(m2))
  expect_equal(rownames(m2), c("a", "b"))
  m3 <- setrownames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
  expect_equal(rownames(m3), c("c", "d"))
  m4 <- m1 %>% setrownames_byname(NULL)
  expect_null(rownames(m4))
  m5 <- m1 %>% setrownames_byname(c(NA, NA))
  expect_equal(rownames(m5), c(NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1, m1)
  l2 <- setrownames_byname(l1, rownames = list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without using a named argument (rownames) to see if they are inferred
  l2 <- setrownames_byname(l1, list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without a list. This should fail, because a is applied to the first matrix and b is applied to the second matrix.
  # But each matrix needs 2 rownames.
  # However, this test actually works on the ubuntu-latest (oldrel-1) test rig in GitHub actions.
  # Because of the inconsistency across platforme and versions, 
  # commenting this test for now.
  # expect_error(setrownames_byname(l1, c("a", "b")), "length of Dimnames")
  
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r1", "r2")))
    )
  expect_equal(rownames(DF2$mcol2[[1]]), c("r1", "r2"))
  expect_equal(rownames(DF2$mcol2[[2]]), c("r1", "r2"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r3", "r4")))
    )
  expect_equal(list(rownames(DF3$mcol2[[1]]), rownames(DF3$mcol2[[2]])), list(c("r3", "r4"), c("r3", "r4")))
})


test_that("setrownames_byname() works with different names for each matrix", {
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_rownames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setrownames_byname(mlist, rownames = new_rownames)
  expect_equal(getrownames_byname(renamed), new_rownames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), rownames_col = I(new_rownames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setrownames_byname(mcol, rownames = rownames_col)
    )
  expect_equal(getrownames_byname(DF_renamed$mcol_2), new_rownames)
})


test_that("setrownames_byname() works with different names for each Matrix", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4), nrow = 2, ncol = 2, byrow = TRUE,
                          dimnames = list(c("r1", "r2"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_rownames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setrownames_byname(mlist, rownames = new_rownames)
  expect_equal(getrownames_byname(renamed), new_rownames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), rownames_col = I(new_rownames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setrownames_byname(mcol, rownames = rownames_col)
    )
  expect_equal(getrownames_byname(DF_renamed$mcol_2), new_rownames)
})


test_that("setcolnames_byname() works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setcolnames_byname(m1, c("a", "b", "c"))
  expect_equal(colnames(m2), c("a", "b", "c"))
  m3 <- setcolnames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("d", "e", "f"))
  expect_equal(colnames(m3), c("d", "e", "f"))
  m4 <- m1 %>% setcolnames_byname(NULL)
  expect_null(colnames(m4))
  m5 <- m1 %>% setcolnames_byname(c(NA, NA, NA))
  expect_equal(colnames(m5), c(NA_character_, NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setcolnames_byname(l1, c("a", "b", "c"))
  expect_equal(list(colnames(l2[[1]]), colnames(l2[[2]])), list(c("a", "b", "c"), c("a", "b", "c")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(colnames(DF2$mcol2[[1]]), c("c1", "c2", "c3"))
  expect_equal(colnames(DF2$mcol2[[2]]), c("c1", "c2", "c3"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(list(colnames(DF3$mcol2[[1]]), colnames(DF3$mcol2[[2]])), list(c("c1", "c2", "c3"), c("c1", "c2", "c3")))
})


test_that("setcolnames_byname() works as expected", {
  m1 <- matsbyname::Matrix(c(1:6), nrow = 2, ncol = 3, 
                           dimnames = list(paste0("i", 1:2), paste0("p", 1:3)), 
                           rowtype = "Industries", coltype = "Commodities")
  m2 <- setcolnames_byname(m1, c("a", "b", "c"))
  expect_true(is.Matrix(m2))
  expect_equal(colnames(m2), c("a", "b", "c"))
  m3 <- setcolnames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("d", "e", "f"))
  expect_equal(colnames(m3), c("d", "e", "f"))
  m4 <- m1 %>% setcolnames_byname(NULL)
  expect_null(colnames(m4))
  m5 <- m1 %>% setcolnames_byname(c(NA, NA, NA))
  expect_equal(colnames(m5), c(NA_character_, NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setcolnames_byname(l1, c("a", "b", "c"))
  expect_equal(list(colnames(l2[[1]]), colnames(l2[[2]])), list(c("a", "b", "c"), c("a", "b", "c")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(colnames(DF2$mcol2[[1]]), c("c1", "c2", "c3"))
  expect_equal(colnames(DF2$mcol2[[2]]), c("c1", "c2", "c3"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(list(colnames(DF3$mcol2[[1]]), colnames(DF3$mcol2[[2]])), list(c("c1", "c2", "c3"), c("c1", "c2", "c3")))
})


test_that("setcolnames_byname() works with different names for each matrix", {
  m <- matrix(c(1, 2,
                3, 4, 
                5, 6), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_colnames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setcolnames_byname(mlist, colnames = new_colnames)
  expect_equal(getcolnames_byname(renamed), new_colnames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), colnames_col = I(new_colnames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setcolnames_byname(mcol, colnames = colnames_col)
    )
  expect_equal(getcolnames_byname(DF_renamed$mcol_2), new_colnames)
})


test_that("setcolnames_byname() works with different names for each matrix", {
  m <- matsbyname::Matrix(c(1, 2,
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_colnames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setcolnames_byname(mlist, colnames = new_colnames)
  expect_true(all(sapply(renamed, is.Matrix)))
  expect_equal(getcolnames_byname(renamed), new_colnames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), colnames_col = I(new_colnames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setcolnames_byname(mcol, colnames = colnames_col)
    )
  expect_equal(getcolnames_byname(DF_renamed$mcol_2), new_colnames)
})


test_that("rename_to_pref_suff_byname() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("c1", "c2")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
})


test_that("rename_to_pref_suff_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "r2", "r3"), c("c1", "c2")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(actual))
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 1, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
})


test_that("rename_to_pref_suff_byname() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  colnames(expected) <- c("a", "c")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows -> Cols") %>% setcoltype("Cols -> Rows")
  res <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Cols")
  expect_equal(coltype(res), "Rows")
})

test_that("rename_to_pref_suff_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  colnames(expected) <- c("a", "c")
  actual <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(actual))
  expect_equal(actual, expected)
  
  expected <- m
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows -> Cols") %>% setcoltype("Cols -> Rows")
  res <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Cols")
  expect_equal(coltype(res), "Rows")
})


test_that("rename_to_pref_suff_byname() works as expected 2", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  colnames(expected) <- c("a", "c")
  # Default is margin = c(1, 2)
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), margin = list(c(1, 2)), keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows") %>% setcoltype("Cols")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Cols")
})


test_that("rename_to_pref_suff_byname() works with Matrix objects 2", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  colnames(expected) <- c("a", "c")
  # Default is margin = c(1, 2)
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(actual))
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "", "")
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), margin = list(c(1, 2)), keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows") %>% setcoltype("Cols")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Cols")
})


test_that("rename_to_pref_suff_byname() also changes rowtype and coltype", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j"))) %>% 
    setrowtype("Industry -> Product") %>% setcoltype("Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(rownames(res), c("a", "c", "e"))
  expect_equal(colnames(res), c("g", "i"))
  expect_equal(rowtype(res), "Industry")
  expect_equal(coltype(res), "Product")
  
  res2 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rownames(res2), c("b", "d", "f"))
  expect_equal(colnames(res2), c("h", "j"))
  expect_equal(rowtype(res2), "Product")
  expect_equal(coltype(res2), "Industry")
})


test_that("rename_to_pref_suff_byname() also changes rowtype and coltype on Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j")), 
                          rowtype = "Industry -> Product", coltype = "Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(res))
  expect_equal(rownames(res), c("a", "c", "e"))
  expect_equal(colnames(res), c("g", "i"))
  expect_equal(rowtype(res), "Industry")
  expect_equal(coltype(res), "Product")
  
  res2 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rownames(res2), c("b", "d", "f"))
  expect_equal(colnames(res2), c("h", "j"))
  expect_equal(rowtype(res2), "Product")
  expect_equal(coltype(res2), "Industry")
})


test_that("rename_to_pref_suff_byname() to change row and column type correctly ignores missing suffixes for Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j")), 
                          rowtype = "Rows", coltype = "Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(res))
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Product")
  
  res_2 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res_2), "")
  expect_equal(coltype(res_2), "Industry")
})


test_that("rename_to_pref_suff_byname() is OK with setting identical row/col names", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "a -> c", "r3"), c("a -> b", "a -> z")))
  expected <- m
  rownames(expected) <- c("a", "a", "r3")
  colnames(expected) <- c("a", "a")
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  # Interestingly the command View(actual) or View(expected)
  # shows row names that aren't true. 
  # The 2nd row of actual and expected is shown as "a.1", 
  # but the underlying objects have simply "a".
  # (The column names are shown correctly.)
  # This test ensures that R isn't messing with the actual row and column names on the objects.
  expect_equal(actual, expected)
})


test_that("rename_to_pref_suff_byname() is OK with setting identical row/col names on Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "a -> c", "r3"), c("a -> b", "a -> z")))
  expected <- m
  rownames(expected) <- c("a", "a", "r3")
  colnames(expected) <- c("a", "a")
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(actual))
  # Interestingly the command View(actual) or View(expected)
  # shows row names that aren't true. 
  # The 2nd row of actual and expected is shown as "a.1", 
  # but the underlying objects have simply "a".
  # (The column names are shown correctly.)
  # This test ensures that R isn't messing with the actual row and column names on the objects.
  expect_equal(actual, expected)
})


test_that("rename_to_pref_suff_byname() works with full prefix identifiers", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a [b]", "c [d]", "e [f]"), c("g [h]", "i [j]")))
  expected <- m
  dimnames(expected) <- list(c("a", "c", "e"), c("g", "i"))
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::bracket_notation)
  expect_equal(actual, expected)
  
  expected2 <- m
  dimnames(expected2) <- list(c("a", "c", "e"), c("g [h]", "i [j]"))
  actual2 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual2, expected2)
  
  expected3 <- m
  dimnames(expected3) <- list(c("a [b]", "c [d]", "e [f]"), c("g", "i"))
  actual3 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(actual3, expected3)
  
  expected4 <- m
  dimnames(expected4) <- list(c("b", "d", "f"), c("h", "j"))
  actual4 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::bracket_notation)
  expect_equal(actual4, expected4)
  
  expected5 <- m
  dimnames(expected5) <- list(c("b", "d", "f"), c("g [h]", "i [j]"))
  actual5 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(expected5, actual5)
  
  expected6 <- m
  dimnames(expected6) <- list(c("a [b]", "c [d]", "e [f]"), c("h", "j"))
  actual6 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(expected6, actual6)
  
  # Try with a list
  actual_list <- rename_to_pref_suff_byname(list(m, m), keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual_list[[1]], expected2)
  expect_equal(actual_list[[2]], expected2)
  
  # Try in a data frame
  DF <- tibble::tibble(m = list(m, m, m, m, m, m), keep = c("pref", "pref", "pref", "suff", "suff", "suff"), 
                       margin = list(c(1, 2), 1, 2, c(1, 2), 1, 2),
                       notation = list(RCLabels::bracket_notation),
                       expected = list(expected, expected2, expected3, expected4, expected5, expected6))
  
  res <- DF %>% 
    dplyr::mutate(
      actual = rename_to_pref_suff_byname(m, keep = keep, margin = margin, 
                                          notation = notation)
    )
  expect_equal(res$actual, res$expected)
})


test_that("rename_to_pref_suff_byname() works with full prefix identifiers on Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a [b]", "c [d]", "e [f]"), c("g [h]", "i [j]")))
  expected <- m
  dimnames(expected) <- list(c("a", "c", "e"), c("g", "i"))
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::bracket_notation)
  expect_equal(actual, expected)
  
  expected2 <- m
  dimnames(expected2) <- list(c("a", "c", "e"), c("g [h]", "i [j]"))
  actual2 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual2, expected2)
  
  expected3 <- m
  dimnames(expected3) <- list(c("a [b]", "c [d]", "e [f]"), c("g", "i"))
  actual3 <- rename_to_pref_suff_byname(m, keep = "pref", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(actual3, expected3)
  
  expected4 <- m
  dimnames(expected4) <- list(c("b", "d", "f"), c("h", "j"))
  actual4 <- rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::bracket_notation)
  expect_equal(actual4, expected4)
  
  expected5 <- m
  dimnames(expected5) <- list(c("b", "d", "f"), c("g [h]", "i [j]"))
  actual5 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(expected5, actual5)
  
  expected6 <- m
  dimnames(expected6) <- list(c("a [b]", "c [d]", "e [f]"), c("h", "j"))
  actual6 <- rename_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::bracket_notation)
  expect_equal(expected6, actual6)
  
  # Try with a list
  actual_list <- rename_to_pref_suff_byname(list(m, m), keep = "pref", margin = 1, notation = RCLabels::bracket_notation)
  expect_equal(actual_list[[1]], expected2)
  expect_equal(actual_list[[2]], expected2)
  
  # Try in a data frame
  DF <- tibble::tibble(m = list(m, m, m, m, m, m), keep = c("pref", "pref", "pref", "suff", "suff", "suff"), 
                       margin = list(c(1, 2), 1, 2, c(1, 2), 1, 2),
                       notation = list(RCLabels::bracket_notation),
                       expected = list(expected, expected2, expected3, expected4, expected5, expected6))
  
  res <- DF %>% 
    dplyr::mutate(
      actual = rename_to_pref_suff_byname(m, keep = keep, margin = margin, 
                                          notation = notation)
    )
  expect_equal(res$actual, res$expected)
})


test_that("setrowtype() and rowtype() work as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% 
    setrowtype("Products")
  expect_null(U %>% setrowtype(NULL) %>% rowtype())
  expect_equal(rowtype(U), "Products")
  # This also works for lists
  Ul <- setrowtype(list(U,U), rowtype = "Products")
  expect_equal(rowtype(Ul), list("Products", "Products"))
  Ul2 <- setrowtype(list(U,U), rowtype = "Junk")
  expect_equal(rowtype(Ul2), list("Junk", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setrowtype(DF$U, "Products")
  expect_equal(rowtype(DF2), list("Products", "Products"))
  DF3 <- DF %>% dplyr::mutate(newcol = setrowtype(U, "Products"))
  expect_equal(DF3$newcol %>% rowtype, list("Products", "Products"))
})


test_that("setrowtype() and rowtype() work with Matrix objects", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2,
                          dimnames = list(productnames, industrynames), 
                          rowtype = "Products")
  expect_null(U %>% 
                setrowtype(NULL) %>% 
                rowtype())
  expect_equal(rowtype(U), "Products")
  # This also works for lists
  Ul <- setrowtype(list(U,U), rowtype = "Products")
  expect_true(all(sapply(Ul, is.Matrix)))
  expect_equal(rowtype(Ul), list("Products", "Products"))
  Ul2 <- setrowtype(list(U,U), rowtype = "Junk")
  expect_equal(rowtype(Ul2), list("Junk", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setrowtype(DF$U, "Products")
  expect_equal(rowtype(DF2), list("Products", "Products"))
  DF3 <- DF %>% dplyr::mutate(newcol = setrowtype(U, "Products"))
  expect_equal(DF3$newcol %>% rowtype, list("Products", "Products"))
})


test_that("setcoltype() and coltype() work as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setcoltype("Industries")
  expect_null(U %>% setcoltype(NULL) %>% coltype())
  expect_equal(coltype(U), "Industries")
  # This also works for lists
  Ul <- setcoltype(list(U,U), coltype = "Industries")
  expect_equal(coltype(Ul), list("Industries", "Industries"))
  Ul2 <- setcoltype(list(U,U), coltype = "Junk")
  expect_equal(coltype(Ul2), list("Junk", "Junk"))
  # Check that it works when the lists are not same structure as a.
  Ul3 <- setcoltype(list(U,U), coltype = list("Junk", "Junk"))
  expect_equal(coltype(Ul3), list("Junk", "Junk"))
  Ul4 <- setcoltype(list(U,U,U), coltype = list("Junk", "Junk", "Bogus"))
  expect_equal(coltype(Ul4), list("Junk", "Junk", "Bogus"))
  Ul5 <- setcoltype(list(U,U,U), coltype = c("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  Ul6 <- setcoltype(list(U,U,U), coltype = list("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  # This one should fail, because length of coltype is neither 1 nor length(a), namely 3.
  expect_error(setcoltype(list(U,U,U), coltype = list("Bogus", "Bogus")), 
               "In prepare_.FUNdots\\(\\), when both 'a' and '.FUNdots' are lists, each top-level argument in .FUNdots must have length = 1 or length = length\\(a\\) \\(= 3\\). Found length = 2 for argument 'coltype', which is a list.")
  
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setcoltype(DF$U, "Industries")
  expect_equal(coltype(DF2), list("Industries", "Industries"))
  DF3 <- DF %>% dplyr::mutate(newcol = setcoltype(U, "Industries"))
  expect_equal(DF3$newcol %>% coltype, list("Industries", "Industries"))
})


test_that("setcoltype() and coltype() work with Matrix objects", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, 
                          dimnames = list(productnames, industrynames), 
                          coltype = "Industries") 
  expect_null(U %>% setcoltype(NULL) %>% coltype())
  expect_equal(coltype(U), "Industries")
  # This also works for lists
  Ul <- setcoltype(list(U,U), coltype = "Industries")
  expect_equal(coltype(Ul), list("Industries", "Industries"))
  Ul2 <- setcoltype(list(U,U), coltype = "Junk")
  expect_equal(coltype(Ul2), list("Junk", "Junk"))
  # Check that it works when the lists are not same structure as a.
  Ul3 <- setcoltype(list(U,U), coltype = list("Junk", "Junk"))
  expect_equal(coltype(Ul3), list("Junk", "Junk"))
  Ul4 <- setcoltype(list(U,U,U), coltype = list("Junk", "Junk", "Bogus"))
  expect_equal(coltype(Ul4), list("Junk", "Junk", "Bogus"))
  Ul5 <- setcoltype(list(U,U,U), coltype = c("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  Ul6 <- setcoltype(list(U,U,U), coltype = list("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  # This one should fail, because length of coltype is neither 1 nor length(a), namely 3.
  expect_error(setcoltype(list(U,U,U), coltype = list("Bogus", "Bogus")), 
               "In prepare_.FUNdots\\(\\), when both 'a' and '.FUNdots' are lists, each top-level argument in .FUNdots must have length = 1 or length = length\\(a\\) \\(= 3\\). Found length = 2 for argument 'coltype', which is a list.")
  
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setcoltype(DF$U, "Industries")
  expect_equal(coltype(DF2), list("Industries", "Industries"))
  DF3 <- DF %>% dplyr::mutate(newcol = setcoltype(U, "Industries"))
  expect_equal(DF3$newcol %>% coltype, list("Industries", "Industries"))
})


test_that("nrow_byname() works as expected.", {
  
  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U) %>%
    expect_equal(2)
  
  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2")
  U2 <- matrix(1:3, ncol = 2, nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U2) %>%
    expect_equal(3)
  
  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3", "p4")
  industrynames <- c("i1", "i2", "i3")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  nrow_byname(U3) %>%
    expect_equal(4)
  
  # Try with a list
  nrow_with_list <- nrow_byname(list(U, U2, U3))
  expect_equal(nrow_with_list[[1]], 2)
  expect_equal(nrow_with_list[[2]], 3)
  expect_equal(nrow_with_list[[3]], 4)
  
  # Fourth, test with a data frame with U, U2, and U3 in a column:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  number_rows <- matsbyname::nrow_byname(dfUs$matrix_byname)
  
  number_rows[[1]] %>%
    testthat::expect_equal(2)
  number_rows[[2]] %>%
    testthat::expect_equal(3)
  number_rows[[3]] %>%
    testthat::expect_equal(4)
  
  
  # Now trying with mutate:
  a <- dfUs %>%
    dplyr::mutate(
      number_of_rows = matsbyname::nrow_byname(matrix_byname)
    )
  
  testthat::expect_equal(a$number_of_rows[[1]], 2)
  testthat::expect_equal(a$number_of_rows[[2]], 3)
  testthat::expect_equal(a$number_of_rows[[3]], 4)
})


test_that("nrow_byname() works with Matrix objects", {
  
  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2,
                          dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  nrow_byname(U) %>%
    expect_equal(2)
  
  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2")
  U2 <- matsbyname::Matrix(1:3, nrow = length(productnames), ncol = 2,
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  nrow_byname(U2) %>%
    expect_equal(3)
  
  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3", "p4")
  industrynames <- c("i1", "i2", "i3")
  U3 <- matsbyname::Matrix(1:4, nrow = length(productnames), ncol = length(industrynames), 
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  nrow_byname(U3) %>%
    expect_equal(4)
  
  # Try with a list
  nrow_with_list <- nrow_byname(list(U, U2, U3))
  expect_equal(nrow_with_list[[1]], 2)
  expect_equal(nrow_with_list[[2]], 3)
  expect_equal(nrow_with_list[[3]], 4)
  
  # Fourth, test with a data frame with U, U2, and U3 in a column:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  number_rows <- matsbyname::nrow_byname(dfUs$matrix_byname)
  
  number_rows[[1]] %>%
    testthat::expect_equal(2)
  number_rows[[2]] %>%
    testthat::expect_equal(3)
  number_rows[[3]] %>%
    testthat::expect_equal(4)
  
  
  # Now trying with mutate:
  a <- dfUs %>%
    dplyr::mutate(
      number_of_rows = matsbyname::nrow_byname(matrix_byname)
    )
  
  testthat::expect_equal(a$number_of_rows[[1]], 2)
  testthat::expect_equal(a$number_of_rows[[2]], 3)
  testthat::expect_equal(a$number_of_rows[[3]], 4)
})


test_that("ncol_byname() works as expected", {
  
  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U) %>% 
    expect_equal(2)
  
  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U2) %>% 
    expect_equal(3)
  
  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U3) %>% 
    expect_equal(4)
  
  
  # Try with a list
  ncol_with_list <- ncol_byname(list(U, U2, U3))
  expect_equal(ncol_with_list[[1]], 2)
  expect_equal(ncol_with_list[[2]], 3)
  expect_equal(ncol_with_list[[3]], 4)
  
  
  # Fourth, test with a data frame with both U, U2, and U3:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  number_cols <- ncol_byname(dfUs$matrix_byname)
  
  number_cols[[1]] %>% 
    expect_equal(2)
  number_cols[[2]] %>% 
    expect_equal(3)
  number_cols[[3]] %>% 
    expect_equal(4)
  
  
  # Now trying with mutate:
  a <- dfUs %>% 
    dplyr::mutate(
      number_of_cols = ncol_byname(matrix_byname)
    )
  
  expect_equal(a$number_of_cols[[1]], 2)
  expect_equal(a$number_of_cols[[2]], 3)
  expect_equal(a$number_of_cols[[3]], 4)
})


test_that("ncol_byname() works with Matrix objects", {
  
  # First, test with a single 2x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, 
                          dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  ncol_byname(U) %>% 
    expect_equal(2)
  
  # Second, test with a 3x2 matrix:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matsbyname::Matrix(1:3, nrow = length(productnames), ncol = length(industrynames),
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  ncol_byname(U2) %>% 
    expect_equal(3)
  
  # Third, test with a 4x3 matrix:
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matsbyname::Matrix(1:4, nrow = length(productnames), ncol = length(industrynames), 
                           dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  ncol_byname(U3) %>% 
    expect_equal(4)
  
  
  # Try with a list
  ncol_with_list <- ncol_byname(list(U, U2, U3))
  expect_equal(ncol_with_list[[1]], 2)
  expect_equal(ncol_with_list[[2]], 3)
  expect_equal(ncol_with_list[[3]], 4)
  
  
  # Fourth, test with a data frame with both U, U2, and U3:
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  number_cols <- ncol_byname(dfUs$matrix_byname)
  
  number_cols[[1]] %>% 
    expect_equal(2)
  number_cols[[2]] %>% 
    expect_equal(3)
  number_cols[[3]] %>% 
    expect_equal(4)
  
  
  # Now trying with mutate:
  a <- dfUs %>% 
    dplyr::mutate(
      number_of_cols = ncol_byname(matrix_byname)
    )
  
  expect_equal(a$number_of_cols[[1]], 2)
  expect_equal(a$number_of_cols[[2]], 3)
  expect_equal(a$number_of_cols[[3]], 4)
})


test_that("create_matrix_byname() works as expected", {
  
  single_mat_with_types <- create_matrix_byname(1 %>% setrowtype("testing_rowtype") %>% setcoltype("testing_coltype"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"))
  # "all" retains row and column types
  expect_equal(single_mat_with_types, matrix(1, dimnames = list("r1", "c1")) %>%
                 setrowtype("testing_rowtype") %>%
                 setcoltype("testing_coltype"))
  
  # Test with row and column types
  single_mat_with_types <- create_matrix_byname(1 %>% setrowtype("rt") %>% setcoltype("ct"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"))
  expect_equal(rowtype(single_mat_with_types), "rt")
  expect_equal(coltype(single_mat_with_types), "ct")
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"))
  
  expect_equal(single_mat_2, matrix(c(1,2), nrow = 2, ncol = 1,
                                    dimnames = list(c("r1", "r2"), "c1")))
  
  # Try with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  expect_equal(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")))
  
  list_of_mats <- create_matrix_byname(list(1, 2) %>% setrowtype("testing_rowtypes") %>% setcoltype("testing_coltypes"),
                                       nrow = list(1, 1), ncol = list(1,1),
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  expect_equal(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")) %>%
                 setrowtype("testing_rowtypes") %>%
                 setcoltype("testing_coltypes"))
  expect_equal(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")) %>%
                 setrowtype("testing_rowtypes") %>%
                 setcoltype("testing_coltypes"))
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))))
  
  expect_equal(list_of_mats_2[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(list_of_mats_2[[2]], matrix(c(2, 3, 4, 5), nrow = 2, ncol = 2, dimnames = list(c("R1", "R2"), c("C1", "C2")), byrow = FALSE))
  
  # Try in a data frame
  df1 <- data.frame(
    dat = I(list()),
    nrows = I(list()),
    ncols = I(list()),
    dimnms = I(list())
  )
  df1[[1, "dat"]] <- 1
  df1[[2, "dat"]] <- 2
  df1[[3, "dat"]] <- c(1,2,3,4, 5, 6)
  df1[[1, "nrows"]] <- 1
  df1[[2, "nrows"]] <- 1
  df1[[3, "nrows"]] <- 2
  df1[[1, "ncols"]] <- 1
  df1[[2, "ncols"]] <- 1
  df1[[3, "ncols"]] <- 3
  df1[[1, "dimnms"]] <- list("r1", "c1")
  df1[[2, "dimnms"]] <- list("R1", "C1")
  df1[[3, "dimnms"]] <- list(c("r1", "r2"), c("c1", "c2", "c3"))
  
  res1 <- df1 %>%
    dplyr::mutate(
      mat_col = create_matrix_byname(dat,
                                     nrow = nrows,
                                     ncol = ncols,
                                     byrow = TRUE,
                                     dimnames = dimnms))
  expect_equal(res1$mat_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(res1$mat_col[[2]], matrix(2, dimnames = list("R1", "C1")))
  expect_equal(res1$mat_col[[3]], matrix(c(1, 2, 3,
                                           4, 5, 6), 
                                         byrow = TRUE,
                                         nrow = 2, ncol = 3,
                                         dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))
  
  # Next, tests using a data frame with U matrices
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list()),
    dat = I(list()),
    number_of_rows = I(list()),
    number_of_cols = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  dfUs_added_matrix <- dfUs %>% 
    dplyr::mutate(
      dat = I(list(1)),
      number_of_rows = I(matsbyname::nrow_byname(matrix_byname)),
      number_of_cols = I(matsbyname::ncol_byname(matrix_byname)),
      row_names = matsbyname::getrownames_byname(matrix_byname),
      col_names = matsbyname::getcolnames_byname(matrix_byname),
      dimension_names = purrr::map2(.x = row_names, .y = col_names, .f = list),
      row_types_col = I(list("testing_rowtypes")),
      col_types_col = I(list("testing_coltypes"))
    )
  
  res2 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names
      )
    )
  
  expect_equal(
    res2$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  expect_equal(
    res2$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  expect_equal(
    res2$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
  
  res3 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names
      )
    )
  
  expect_equal(
    res3$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  expect_equal(
    res3$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  expect_equal(
    res3$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
})


test_that("create_matrix_byname() works with matrix.class = 'Matrix'", {
  
  expect_error(create_matrix_byname(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1"), matrix.class = "bogus"), 
               "'arg' should be one of")
  
  
  single_mat_with_types <- create_matrix_byname(1 %>% 
                                                  setrowtype("testing_rowtype") %>% 
                                                  setcoltype("testing_coltype"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"), 
                                                matrix.class = "Matrix")
  expect_true(is.Matrix(single_mat_with_types))
  # "all" retains row and column types
  matsbyname:::expect_equal_matrix_or_Matrix(single_mat_with_types, 
                                             matrix(1, dimnames = list("r1", "c1")) %>%
                                               setrowtype("testing_rowtype") %>%
                                               setcoltype("testing_coltype"))
  
  # Test with row and column types
  single_mat_with_types <- create_matrix_byname(1 %>% 
                                                  setrowtype("rt") %>%
                                                  setcoltype("ct"),
                                                nrow = 1, ncol = 1,
                                                dimnames = list("r1", "c1"), 
                                                matrix.class = "Matrix")
  expect_equal(rowtype(single_mat_with_types), "rt")
  expect_equal(coltype(single_mat_with_types), "ct")
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"), 
                                       matrix.class = "Matrix")
  expect_true(is.Matrix(single_mat_2))
  matsbyname:::expect_equal_matrix_or_Matrix(single_mat_2, 
                                             matrix(c(1,2), nrow = 2, ncol = 1, 
                                                    dimnames = list(c("r1", "r2"), "c1")))

  # Try with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")), 
                                       matrix.class = "Matrix")
  
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")))
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")))
  
  list_of_mats <- create_matrix_byname(list(1, 2) %>% setrowtype("testing_rowtypes") %>% setcoltype("testing_coltypes"),
                                       nrow = list(1, 1), ncol = list(1,1),
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")), 
                                       matrix.class = "Matrix")
  
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats[[1]], matrix(1, dimnames = list("r1", "c1")) %>%
                                               setrowtype("testing_rowtypes") %>%
                                               setcoltype("testing_coltypes"))
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats[[2]], matrix(2, dimnames = list("R1", "C1")) %>%
                                               setrowtype("testing_rowtypes") %>%
                                               setcoltype("testing_coltypes"))
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))), 
                                         matrix.class = "Matrix")
  
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats_2[[1]], matrix(1, dimnames = list("r1", "c1")))
  matsbyname:::expect_equal_matrix_or_Matrix(list_of_mats_2[[2]], matrix(c(2, 3, 4, 5), nrow = 2, ncol = 2, dimnames = list(c("R1", "R2"), c("C1", "C2")), byrow = FALSE))
  
  # Try in a data frame
  df1 <- data.frame(
    dat = I(list()),
    nrows = I(list()),
    ncols = I(list()),
    dimnms = I(list())
  )
  df1[[1, "dat"]] <- 1
  df1[[2, "dat"]] <- 2
  df1[[3, "dat"]] <- c(1,2,3,4, 5, 6)
  df1[[1, "nrows"]] <- 1
  df1[[2, "nrows"]] <- 1
  df1[[3, "nrows"]] <- 2
  df1[[1, "ncols"]] <- 1
  df1[[2, "ncols"]] <- 1
  df1[[3, "ncols"]] <- 3
  df1[[1, "dimnms"]] <- list("r1", "c1")
  df1[[2, "dimnms"]] <- list("R1", "C1")
  df1[[3, "dimnms"]] <- list(c("r1", "r2"), c("c1", "c2", "c3"))
  
  res1 <- df1 %>%
    dplyr::mutate(
      mat_col = create_matrix_byname(dat,
                                     nrow = nrows,
                                     ncol = ncols,
                                     byrow = TRUE,
                                     dimnames = dimnms, 
                                     matrix.class = "Matrix"))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$mat_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$mat_col[[2]], matrix(2, dimnames = list("R1", "C1")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$mat_col[[3]], matrix(c(1, 2, 3,
                                                                         4, 5, 6), 
                                                                       byrow = TRUE,
                                                                       nrow = 2, ncol = 3,
                                                                       dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))
  
  # Next, tests using a data frame with U matrices
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2,
                          dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matsbyname:::Matrix(1:3, ncol = length(industrynames), nrow = length(productnames), 
                            dimnames = list(productnames, industrynames), 
                            rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matsbyname:::Matrix(1:4, ncol = length(industrynames), nrow = length(productnames),
                            dimnames = list(productnames, industrynames), 
                            rowtype = "Products", coltype = "Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list()),
    dat = I(list()),
    number_of_rows = I(list()),
    number_of_cols = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  dfUs_added_matrix <- dfUs %>% 
    dplyr::mutate(
      dat = I(list(1)),
      number_of_rows = I(matsbyname::nrow_byname(matrix_byname)),
      number_of_cols = I(matsbyname::ncol_byname(matrix_byname)),
      row_names = matsbyname::getrownames_byname(matrix_byname),
      col_names = matsbyname::getcolnames_byname(matrix_byname),
      dimension_names = purrr::map2(.x = row_names, .y = col_names, .f = list),
      row_types_col = I(list("testing_rowtypes")),
      col_types_col = I(list("testing_coltypes"))
    )
  
  res2 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names, 
        matrix.class = "Matrix"
      )
    )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res2$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res2$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res2$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
  
  res3 <- dfUs_added_matrix %>% 
    dplyr::mutate(
      new_matrix = matsbyname::create_matrix_byname(
        dat,
        nrow = number_of_rows,
        ncol = number_of_cols,
        dimnames = dimension_names, 
        matrix.class = "Matrix"
      )
    )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res3$new_matrix[[1]],
    matrix(1, ncol = 2, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2")))
  )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res3$new_matrix[[2]],
    matrix(1, ncol = 3, nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3")))
  )
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res3$new_matrix[[3]],
    matrix(1, ncol = 4, nrow = 3, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3", "i4")))
  )
})


test_that("create_rowvec_byname() works with Matrix objects", {
  # Try with a single number
  single_vec <- create_rowvec_byname(c(c1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), rowname = "r1", 
                                     matrix.class = "Matrix")
  expect_true(is.Matrix(single_vec))
  matsbyname:::expect_equal_matrix_or_Matrix(single_vec, 
                                             matrix(1, dimnames = list("r1", "c1")) %>%
                                               setrowtype("rt") %>% setcoltype("ct"))
  
  # Test with dimnames
  sv_dimnames <- create_rowvec_byname(1, dimnames = list("r1", "c1"), matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(sv_dimnames, matrix(1, dimnames = list("r1", "c1")))
  
  # Try with a vector of numbers
  vector_vec <- create_rowvec_byname(c(c1 = 1, c2 = 2), rowname = "r1", matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(vector_vec, 
                                             matrix(c(1,2), ncol = 2, byrow = TRUE, dimnames = list("r1", c("c1", "c2"))))
  
  # Try with a list of vectors
  vv_vec <- create_rowvec_byname(list(c(c1 = 1, c2 = 2), c(C1 = 3, C2 = 4, C3 = 5)),
                                 rowname = list("r1", "R1"), 
                                 matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(vv_vec[[1]], 
                                             matrix(c(1,2), ncol = 2, dimnames = list("r1", c("c1", "c2"))))
  
  # Try in a data frame
  dat <- list(c(c1 = 1), c(C1 = 2, C2 = 3), c(c1 = 1, c2 = 2, c3 = 3, c4 = 4, c5 = 5, c6 = 6))
  rnms <- list("r1", "R1", "r1")
  
  df1 <- tibble::tibble(dat, rnms)
  res1 <- df1 %>%
    dplyr::mutate(
      rowvec_col = create_rowvec_byname(dat, rowname = rnms, matrix.class = "Matrix")
    )
  matsbyname:::expect_equal_matrix_or_Matrix(res1$rowvec_col[[1]], 
                                             matrix(1, dimnames = list("r1", "c1")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$rowvec_col[[2]],
                                             matrix(c(2, 3), ncol = 2, dimnames = list("R1", c("C1", "C2"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$rowvec_col[[3]],
                                             matrix(c(1, 2, 3, 4, 5, 6), 
                                                    nrow = 1, ncol = 6,
                                                    dimnames = list("r1", c("c1", "c2", "c3", "c4", "c5", "c6"))))
  
  # Try in data frame with dimnames and named vector.  See which one wins.
  dimnms <- list(list("r01", "c01"), list("R01", c("C01", "C02")), list("r01", c("c01", "c02", "c03", "c04", "c05", "c06")))
  df2 <- tibble::tibble(dat, rnms, dimnms)
  res2 <- df2 %>% 
    dplyr::mutate(
      rowvec_col = create_rowvec_byname(dat, dimnames = dimnms, rowname = rnms, matrix.class = "Matrix")
    )
  # Explicitly setting dimnames should win.
  matsbyname:::expect_equal_matrix_or_Matrix(res2$rowvec_col[[1]], 
                                             matrix(1, dimnames = list("r01", "c01")))
  matsbyname:::expect_equal_matrix_or_Matrix(res2$rowvec_col[[2]],
                                             matrix(c(2, 3), ncol = 2, dimnames = list("R01", c("C01", "C02"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res2$rowvec_col[[3]],
                                             matrix(c(1, 2, 3, 4, 5, 6), 
                                                    nrow = 1, ncol = 6,
                                                    dimnames = list("r01", c("c01", "c02", "c03", "c04", "c05", "c06"))))
})


test_that("create_colvec_byname() works as expected", {
  # Try with a single number
  single_vec <- create_colvec_byname(c(r1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"),
                                     colname = "c1")
  expect_equal(single_vec, matrix(1, dimnames = list("r1", "c1")) %>% setrowtype("rt") %>% setcoltype("ct"))
  
  # Try with a vector of numbers
  vector_vec <- create_colvec_byname(c(r1 = 1, r2 = 2), colname = "c1")
  expect_equal(vector_vec, matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try with a list of vectors
  vv_vec <- create_colvec_byname(list(c(r1 = 1, r2 = 2), c(R1 = 3, R2 = 4, R3 = 5)),
                                 colname = list("c1", "C1"))
  expect_equal(vv_vec[[1]], matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try in a data frame
  dat <- list(c(r1 = 1), c(R1 = 2, R2 = 3), c(r1 = 1, r2 = 2, r3 = 3, r4 = 4, r5 = 5, r6 = 6))
  cnms <- list("c1", "C1", "c1")
  
  df1 <- tibble::tibble(dat, cnms)
  res1 <- df1 %>%
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, colname = cnms)
    )
  expect_equal(res1$colvec_col[[1]], matrix(1, dimnames = list("r1", "c1")))
  expect_equal(res1$colvec_col[[2]], matrix(c(2, 3), nrow = 2, dimnames = list(c("R1", "R2"), "C1")))
  expect_equal(res1$colvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 6, ncol = 1,
                                            dimnames = list(c("r1", "r2", "r3", "r4", "r5", "r6"), "c1")))
  
  # Try in data frame with dimnames and named vector.  See which one wins.
  dimnms <- list(list("r01", "c01"), list(c("R01", "R02"), "C01"), list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01"))
  df2 <- tibble::tibble(dat, cnms, dimnms) 
  res2 <- df2 %>% 
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, dimnames = dimnms, colname = cnms)
    )
  # Explicitly setting dimnames should win.
  expect_equal(res2$colvec_col[[1]], matrix(1, dimnames = list("r01", "c01")))
  expect_equal(res2$colvec_col[[2]], matrix(c(2, 3), nrow = 2, dimnames = list(c("R01", "R02"), "C01")))
  expect_equal(res2$colvec_col[[3]], matrix(c(1, 2, 3, 4, 5, 6), 
                                            nrow = 6, ncol = 1,
                                            dimnames = list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01")))
})


test_that("create_colvec_byname() works for Matrix objects", {
  # Try with a single number
  single_vec <- create_colvec_byname(c(r1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"),
                                     colname = "c1", matrix.class = "Matrix")
  expect_true(is.Matrix(single_vec))
  matsbyname:::expect_equal_matrix_or_Matrix(single_vec, 
                                             matrix(1, dimnames = list("r1", "c1")) %>% 
                                               setrowtype("rt") %>% setcoltype("ct"))
  
  # Try with a vector of numbers
  vector_vec <- create_colvec_byname(c(r1 = 1, r2 = 2), colname = "c1", matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(vector_vec, 
                                             matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try with a list of vectors
  vv_vec <- create_colvec_byname(list(c(r1 = 1, r2 = 2), c(R1 = 3, R2 = 4, R3 = 5)),
                                 colname = list("c1", "C1"), matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(vv_vec[[1]],
                                             matrix(c(1,2), nrow = 2, dimnames = list(c("r1", "r2"), "c1")))
  
  # Try in a data frame
  dat <- list(c(r1 = 1), c(R1 = 2, R2 = 3), c(r1 = 1, r2 = 2, r3 = 3, r4 = 4, r5 = 5, r6 = 6))
  cnms <- list("c1", "C1", "c1")
  
  df1 <- tibble::tibble(dat, cnms)
  res1 <- df1 %>%
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, colname = cnms, matrix.class = "Matrix")
    )
  matsbyname:::expect_equal_matrix_or_Matrix(res1$colvec_col[[1]],
                                             matrix(1, dimnames = list("r1", "c1")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$colvec_col[[2]],
                                             matrix(c(2, 3), nrow = 2, dimnames = list(c("R1", "R2"), "C1")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$colvec_col[[3]],
                                             matrix(c(1, 2, 3, 4, 5, 6), 
                                                    nrow = 6, ncol = 1,
                                                    dimnames = list(c("r1", "r2", "r3", "r4", "r5", "r6"), "c1")))
  
  # Try in data frame with dimnames and named vector.  See which one wins.
  dimnms <- list(list("r01", "c01"), list(c("R01", "R02"), "C01"), list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01"))
  df2 <- tibble::tibble(dat, cnms, dimnms) 
  res2 <- df2 %>% 
    dplyr::mutate(
      colvec_col = create_colvec_byname(dat, dimnames = dimnms, colname = cnms, matrix.class = "Matrix")
    )
  # Explicitly setting dimnames should win.
  matsbyname:::expect_equal_matrix_or_Matrix(res2$colvec_col[[1]],
                                             matrix(1, dimnames = list("r01", "c01")))
  matsbyname:::expect_equal_matrix_or_Matrix(res2$colvec_col[[2]],
                                             matrix(c(2, 3), nrow = 2, dimnames = list(c("R01", "R02"), "C01")))
  matsbyname:::expect_equal_matrix_or_Matrix(res2$colvec_col[[3]], 
                                             matrix(c(1, 2, 3, 4, 5, 6), 
                                                    nrow = 6, ncol = 1,
                                                    dimnames = list(c("r01", "r02", "r03", "r04", "r05", "r06"), "c01")))
})


test_that("kvec_from_template_byname() works as expected", {
  m <- matrix(42, nrow = 4, ncol = 2,
              dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2")))
  expect_equal(kvec_from_template_byname(m, colname = "mycol"), matrix(1, nrow = 4, ncol = 1, 
                                                                       dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  expect_equal(kvec_from_template_byname(m, colname = "myrow", column = FALSE), matrix(1, nrow = 1, ncol = 2, 
                                                                                       dimnames = list("myrow", c("c1", "c2"))))
  
  # Try in a data frame.
  df1 <- tibble::tibble(m = list(m, m), cnme = "mycol", rnme = "myrow", clmn = TRUE, k = c(42, 43), 
                        rtype = "rt", ctype = c("ct1", "ct2"))
  
  res1 <- df1 %>% 
    dplyr::mutate(
      irow = kvec_from_template_byname(m, colname = rnme, column = FALSE),
      icol = kvec_from_template_byname(m, colname = cnme, column = clmn), 
      kcol = kvec_from_template_byname(m, k = k, colname = cnme), 
      with_rt_ct = kvec_from_template_byname(m %>% setrowtype(rtype) %>% setcoltype(ctype), colname = cnme)
    )
  
  expect_equal(res1$irow[[1]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$irow[[2]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$icol[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  # Try with non-1 value for k
  expect_equal(res1$kcol[[1]], matrix(42, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$kcol[[2]], matrix(43, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  # Test that row and column types are transferred correctly
  expect_equal(res1$with_rt_ct[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct1"))
  expect_equal(res1$with_rt_ct[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct2"))
})


test_that("kvec_from_template_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(42, nrow = 4, ncol = 2,
                          dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2")))
  res <- kvec_from_template_byname(m, colname = "mycol")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(1, nrow = 4, ncol = 1, 
                                                    dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  matsbyname:::expect_equal_matrix_or_Matrix(kvec_from_template_byname(m, colname = "myrow", column = FALSE), 
                                             matrix(1, nrow = 1, ncol = 2, 
                                                    dimnames = list("myrow", c("c1", "c2"))))
  
  # Try in a data frame.
  df1 <- tibble::tibble(m = list(m, m), cnme = "mycol", rnme = "myrow", clmn = TRUE, k = c(42, 43), 
                        rtype = "rt", ctype = c("ct1", "ct2"))
  
  res1 <- df1 %>% 
    dplyr::mutate(
      irow = kvec_from_template_byname(m, colname = rnme, column = FALSE),
      icol = kvec_from_template_byname(m, colname = cnme, column = clmn), 
      kcol = kvec_from_template_byname(m, k = k, colname = cnme), 
      with_rt_ct = kvec_from_template_byname(m %>% 
                                               setrowtype(rtype) %>% 
                                               setcoltype(ctype), 
                                             colname = cnme)
    )
  
  matsbyname:::expect_equal_matrix_or_Matrix(res1$irow[[1]],
                                             matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$irow[[2]], 
                                             matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[1]],
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[2]],
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[2]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  # Try with non-1 value for k
  matsbyname:::expect_equal_matrix_or_Matrix(res1$kcol[[1]], 
                                             matrix(42, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$kcol[[2]],
                                             matrix(43, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  # Test that row and column types are transferred correctly
  matsbyname:::expect_equal_matrix_or_Matrix(res1$with_rt_ct[[1]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                                               setrowtype("rt") %>% setcoltype("ct1"))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$with_rt_ct[[2]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                                               setrowtype("rt") %>% setcoltype("ct2"))
})


test_that("kvec_from_template_byname() passes old i_byname tests", {
  
  # First, test with single values.
  single_mat <- create_matrix_byname(1, nrow = 1, ncol = 1,
                                     dimnames = list("r1", "c1"))
  
  expect_equal(
    kvec_from_template_byname(single_mat, colname = "output_column"), 
    matrix(1, dimnames = list("r1", "output_column"))
  )
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(
    kvec_from_template_byname(single_mat_2, colname = "output_column"), 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "output_column"))
  )
  
  # Second, test with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  res_list <- kvec_from_template_byname(list_of_mats, colname = "output_column")
  
  expect_equal(
    res_list[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list[[2]], 
    matrix(1, dimnames = list("R1", "output_column"))
  )
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))))
  
  res_list_2 <- kvec_from_template_byname(list_of_mats_2, colname = "output_column")
  
  expect_equal(
    res_list_2[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list_2[[2]], 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("R1", "R2"), "output_column"))
  )
  
  
  # Third test with data frames:
  
  # Creating data frame of matrices, with a year column and a matrix column:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  # Now creating the unity vector
  res <- dfUs %>% 
    dplyr::mutate(
      unity_vec = kvec_from_template_byname(matrix_byname, colname = "Product")
    )
  
  # Checking number of coefficients in each vector
  expect_equal(nrow(res$unity_vec[[1]]), 2)
  expect_equal(nrow(res$unity_vec[[2]]), 2)
  expect_equal(nrow(res$unity_vec[[3]]), 3)
  
  # Checking rowtypes
  expect_equal(res$unity_vec[[1]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[2]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[3]] %>% rowtype(), "Products")
  
  # Checking coltypes
  expect_equal(res$unity_vec[[1]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[2]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[3]] %>% coltype(), "Industries")
  
  # Checking single coefficient values
  expect_equal(res$unity_vec[[1]][["p1", "Product"]], 1)
  expect_equal(res$unity_vec[[2]][["p2", "Product"]], 1)
  expect_equal(res$unity_vec[[3]][["p3", "Product"]], 1)
  
  # Checking sums
  res2 <- res %>%
    dplyr::mutate(
      sum_unity = matsbyname::sumall_byname(unity_vec)
    )
  # Check
  expect_equal(res2$sum_unity[[1]], 2)
  expect_equal(res2$sum_unity[[2]], 2)
  expect_equal(res2$sum_unity[[3]], 3)
})


test_that("kvec_from_template_byname() passes old i_byname tests with Matrix objects", {
  
  # First, test with single values.
  single_mat <- create_matrix_byname(1, nrow = 1, ncol = 1,
                                     dimnames = list("r1", "c1"), matrix.class = "Matrix")
  
  res <- kvec_from_template_byname(single_mat, colname = "output_column")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, matrix(1, dimnames = list("r1", "output_column")))
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"), matrix.class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(
    kvec_from_template_byname(single_mat_2, colname = "output_column"), 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "output_column"))
  )
  
  # Second, test with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")), 
                                       matrix.class = "Matrix")
  
  res_list <- kvec_from_template_byname(list_of_mats, colname = "output_column")
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list[[2]], 
    matrix(1, dimnames = list("R1", "output_column"))
  )
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))), 
                                         matrix.class = "Matrix")
  
  res_list_2 <- kvec_from_template_byname(list_of_mats_2, colname = "output_column")
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list_2[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list_2[[2]], 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("R1", "R2"), "output_column"))
  )
  
  
  # Third test with data frames:
  
  # Creating data frame of matrices, with a year column and a matrix column:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matsbyname::Matrix(1:3, ncol = length(industrynames), nrow = length(productnames), 
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matsbyname::Matrix(1:4, ncol = length(industrynames), nrow = length(productnames), 
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  # Now creating the unity vector
  res <- dfUs %>% 
    dplyr::mutate(
      unity_vec = kvec_from_template_byname(matrix_byname, colname = "Product")
    )
  
  # Checking number of coefficients in each vector
  expect_equal(nrow(res$unity_vec[[1]]), 2)
  expect_equal(nrow(res$unity_vec[[2]]), 2)
  expect_equal(nrow(res$unity_vec[[3]]), 3)
  
  # Checking rowtypes
  expect_equal(res$unity_vec[[1]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[2]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[3]] %>% rowtype(), "Products")
  
  # Checking coltypes
  expect_equal(res$unity_vec[[1]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[2]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[3]] %>% coltype(), "Industries")
  
  # Checking single coefficient values
  expect_equal(res$unity_vec[[1]]["p1", "Product"], 1)
  expect_equal(res$unity_vec[[2]]["p2", "Product"], 1)
  expect_equal(res$unity_vec[[3]]["p3", "Product"], 1)
  
  # Checking sums
  res2 <- res %>%
    dplyr::mutate(
      sum_unity = matsbyname::sumall_byname(unity_vec)
    )
  # Check
  expect_equal(res2$sum_unity[[1]], 2)
  expect_equal(res2$sum_unity[[2]], 2)
  expect_equal(res2$sum_unity[[3]], 3)
})


test_that("vec_from_store_byname() works as expected with single matrices", {
  a <- matrix(42, nrow = 2, ncol = 3, 
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  v <- matrix(1:10, nrow = 10, ncol = 1, 
              dimnames = list(paste0("r", 1:10) %>% rev(), "c1")) %>%
    setrowtype("rt") %>% setcoltype("ct")
  expect_equal(vec_from_store_byname(a = a, v = v), 
               matrix(c(10, 9), nrow = 2, ncol = 1, 
                      dimnames = list(c("r1", "r2"), "c1")) %>%
                 setrowtype("rt") %>% setcoltype("ct"))
})


test_that("vec_from_store_byname() works with single Matrix objects", {
  a <- matsbyname::Matrix(42, nrow = 2, ncol = 3, 
                          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  v <- matsbyname::Matrix(1:10, nrow = 10, ncol = 1, 
                          dimnames = list(paste0("r", 1:10) %>% rev(), "c1"), 
                          rowtype = "rt", coltype = "ct")
  matsbyname:::expect_equal_matrix_or_Matrix(vec_from_store_byname(a = a, v = v), 
                                             matrix(c(10, 9), nrow = 2, ncol = 1, 
                                                    dimnames = list(c("r1", "r2"), "c1")) %>%
                                               setrowtype("rt") %>% setcoltype("ct"))
})


test_that("vec_from_store_byname() works as expected with single matrices and nouns", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v, a_piece = "noun"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and nouns", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                          dimnames = list(c("Electricity", 
                                            "Peat", 
                                            "Hydro", 
                                            "Crude oil",
                                            "Coal", 
                                            "Hard coal (if no detail)", 
                                            "Brown coal"), 
                                          "phi"), 
                          rowtype = "Product", coltype = "phi")
  res <- vec_from_store_byname(a, v, a_piece = "noun")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and pref suff", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  # Try with prefixes
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try with suffixes
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "from e in f", 
                                 "Hydro", 
                                 "Crude oil",
                                 "from b in c", 
                                 "Hard coal (if no detail)", 
                                 "from Production in USA"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "suff"), 
               matrix(c(5, 2, 7), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and pref suff", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                          dimnames = list(c("Electricity", 
                                            "Peat", 
                                            "Hydro", 
                                            "Crude oil",
                                            "Coal", 
                                            "Hard coal (if no detail)", 
                                            "Brown coal"), 
                                          "phi"), 
                          rowtype = "Product", coltype = "phi")
  
  # Try with prefixes
  res <- vec_from_store_byname(a, v, a_piece = "pref")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
  # Try with suffixes
  v2 <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                           dimnames = list(c("Electricity", 
                                             "from e in f", 
                                             "Hydro", 
                                             "Crude oil",
                                             "from b in c", 
                                             "Hard coal (if no detail)", 
                                             "from Production in USA"), 
                                           "phi"), 
                           rowtype = "Product", coltype = "phi")
  matsbyname:::expect_equal_matrix_or_Matrix(vec_from_store_byname(a, v2, a_piece = "suff"), 
                                             matrix(c(5, 2, 7), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and prepositions", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "USA", 
                                "c",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "in"), 
               matrix(c(4, 7, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  
  
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail)", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "from"), 
               matrix(c(7, 4, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try when the preposition (in this case "to") is not present in a.
  expect_equal(vec_from_store_byname(a, v, a_piece = "to"), 
               matrix(c(NA_real_, NA_real_, NA_real_), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  
  # Try when we use different pieces of a and v.
  a3 <- matrix(42, nrow = 3, ncol = 5, 
               dimnames = list(c("Electricity [from b in GBR]", 
                                 "Coal [from e in f]", 
                                 "Crude oil [from Production in USA]"), 
                               c("Main activity producer electricity plants", 
                                 "Wind turbines", 
                                 "Oil refineries", 
                                 "Coal mines", 
                                 "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v3 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_error(vec_from_store_byname(a3, v3, a_piece = "in", v_piece = "from"), 
               "v_pieces must be unique in vec_from_store_byname")
  
  
  v4 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat [from nowhere]", 
                                 "Production [from GHA]", 
                                 "e [from ZAF]",
                                 "Coal [from AUS]", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b [from Nebraska]"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  expect_equal(vec_from_store_byname(a3, v4, a_piece = "in", v_piece = "from"), 
               matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in GBR]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and prepositions", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  # Keep v as a matrix
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "USA", 
                                "c",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  res <- vec_from_store_byname(a, v, a_piece = "in")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(4, 7, 3), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
  
  # Again, leave v2 as a matrix
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail)", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  matsbyname:::expect_equal_matrix_or_Matrix(
    vec_from_store_byname(a, v2, a_piece = "from"), 
    matrix(c(7, 4, 3), nrow = 3, ncol = 1, 
           dimnames = list(c("Electricity [from b in c]", 
                             "Coal [from e in f]", 
                             "Crude oil [from Production in USA]"), 
                           "phi")) %>%
      setrowtype("Product") %>% setcoltype("phi"))
  # Try when the preposition (in this case "to") is not present in a.
  res_null <- vec_from_store_byname(a, v, a_piece = "to")
  expect_true(is.na(res_null[1, 1]))
  expect_true(is.na(res_null[2, 1]))
  expect_true(is.na(res_null[3, 1]))

  # Try when we use different pieces of a and v.
  a3 <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                           dimnames = list(c("Electricity [from b in GBR]", 
                                             "Coal [from e in f]", 
                                             "Crude oil [from Production in USA]"), 
                                           c("Main activity producer electricity plants", 
                                             "Wind turbines", 
                                             "Oil refineries", 
                                             "Coal mines", 
                                             "Automobiles")), 
                           rowtype = "Product", coltype = "Industry")
  v3 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_error(vec_from_store_byname(a3, v3, a_piece = "in", v_piece = "from"), 
               "v_pieces must be unique in vec_from_store_byname")
  
  
  v4 <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                           dimnames = list(c("Electricity [from USA]", 
                                             "Peat [from nowhere]", 
                                             "Production [from GHA]", 
                                             "e [from ZAF]",
                                             "Coal [from AUS]", 
                                             "Hard coal (if no detail) [from GBR]", 
                                             "b [from Nebraska]"), 
                                           "phi"), 
                           rowtype = "Product", coltype = "phi")
  res4 <- vec_from_store_byname(a3, v4, a_piece = "in", v_piece = "from")
  expect_equal(res4[1, 1], 6)
  expect_true(is.na(res4[2, 1]))
  expect_equal(res4[3, 1], 1)
})


test_that("vec_from_store_byname() works when a row vector is desired.", {
  a <- matrix(42, nrow = 3, ncol = 2, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Wind turbines", 
                                "Oil wells"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta")) %>%
    setrowtype("Industry") %>% setcoltype("eta")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref", column = FALSE), 
               matrix(c(3, 5), nrow = 1, ncol = 2, 
                      dimnames = list("eta", 
                                      c("Wind turbines", 
                                        "Oil wells"))) %>%
                 setrowtype("eta") %>% setcoltype("Industry"))
  
  # See if it works with a row vector for v.
  v_row <- matrix(1:7, nrow = 1, ncol = 7, 
                  dimnames = list("eta", 
                                  c("Electricity", 
                                    "Peat", 
                                    "Wind turbines", 
                                    "c",
                                    "Oil wells", 
                                    "Hard coal (if no detail)", 
                                    "f"))) %>%
    setrowtype("eta") %>% setcoltype("Industry")
  expect_equal(vec_from_store_byname(a, v_row, a_piece = "pref", column = FALSE), 
               matrix(c(3, 5), nrow = 1, ncol = 2, 
                      dimnames = list("eta", 
                                      c("Wind turbines", 
                                        "Oil wells"))) %>%
                 setrowtype("eta") %>% setcoltype("Industry"))
  
})


test_that("vec_from_store_byname() works when a row vector Matrix object is desired.", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 2, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Wind turbines", 
                                            "Oil wells")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta")) %>%
    setrowtype("Industry") %>% setcoltype("eta")
  
  res <- vec_from_store_byname(a, v, a_piece = "pref", column = FALSE)
  matsbyname:::expect_equal_matrix_or_Matrix(
    res, 
    matrix(c(3, 5), nrow = 1, ncol = 2, 
           dimnames = list("eta", 
                           c("Wind turbines", 
                             "Oil wells"))) %>%
      setrowtype("eta") %>% setcoltype("Industry"))
  
  # See if it works with a row vector for v.
  v_row <- matrix(1:7, nrow = 1, ncol = 7, 
                  dimnames = list("eta", 
                                  c("Electricity", 
                                    "Peat", 
                                    "Wind turbines", 
                                    "c",
                                    "Oil wells", 
                                    "Hard coal (if no detail)", 
                                    "f"))) %>%
    setrowtype("eta") %>% setcoltype("Industry")
  matsbyname:::expect_equal_matrix_or_Matrix(
    vec_from_store_byname(a, v_row, a_piece = "pref", column = FALSE), 
    matrix(c(3, 5), nrow = 1, ncol = 2, 
           dimnames = list("eta", 
                           c("Wind turbines", 
                             "Oil wells"))) %>%
      setrowtype("eta") %>% setcoltype("Industry"))
  
})


test_that("vec_from_store_byname() works when a is a Matrix and v is a matrix.", {
  a <- matrix(42, nrow = 3, ncol = 2, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Wind turbines", 
                                "Oil wells"))) %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta"), 
              rowtype = "Industry", coltype = "eta")
  
  res <- vec_from_store_byname(a, v, a_piece = "pref", column = FALSE)
  expect_equal(
    res, 
    matrix(c(3, 5), nrow = 1, ncol = 2, 
           dimnames = list("eta", 
                           c("Wind turbines", 
                             "Oil wells"))) %>%
      setrowtype("eta") %>% setcoltype("Industry"))
  
  # See if it works with a row vector for v.
  v_row <- matsbyname::Matrix(1:7, nrow = 1, ncol = 7, 
                              dimnames = list("eta", 
                                              c("Electricity", 
                                                "Peat", 
                                                "Wind turbines", 
                                                "c",
                                                "Oil wells", 
                                                "Hard coal (if no detail)", 
                                                "f")), 
                              rowtype = "eta", coltype = "Industry")
  expect_equal(
    vec_from_store_byname(a, v_row, a_piece = "pref", column = FALSE), 
    matrix(c(3, 5), nrow = 1, ncol = 2, 
           dimnames = list("eta", 
                           c("Wind turbines", 
                             "Oil wells"))) %>%
      setrowtype("eta") %>% setcoltype("Industry"))
  
})


test_that("vec_from_store_byname() works with lists", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  a_list <- list(a, a, a)
  v_list <- list(v, v, v)
  expected_list <- list(expected, expected, expected)
  
  # Try with notation and prepositions already wrapped in lists.
  res <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                               notation = list(RCLabels::bracket_notation), 
                               prepositions = list(RCLabels::prepositions_list))  
  expect_equal(res, expected_list)
  
  # Try with notation and prepositions not already wrapped in lists.
  
  res2 <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from")
  expect_equal(res2, expected_list)
  
})


test_that("vec_from_store_byname() works with Matrix objects in lists", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in GBR]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Indsutry")

  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, -9999, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  a_list <- list(a, a, a)
  v_list <- list(v, v, v)
  expected_list <- list(expected, expected, expected)
  
  # Try with notation and prepositions already wrapped in lists.
  res <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                               notation = list(RCLabels::bracket_notation), 
                               prepositions = list(RCLabels::prepositions_list), 
                               missing = -9999)  
  expect_true(all(mapply(matsbyname:::equal_matrix_or_Matrix, res, expected_list)))
  
  # Try with notation and prepositions not already wrapped in lists.
  
  res2 <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                                missing = -9999)
  expect_true(all(mapply(matsbyname:::equal_matrix_or_Matrix, res2, expected_list)))
})


test_that("vec_from_store_byname() works in a data frame", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  df <- tibble::tibble(a = list(a, a, a), 
                       v = list(v, v, v), 
                       expected = list(expected, expected, expected))
  
  with_res <- df %>%
    dplyr::mutate(
      actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from")
    )
  expect_equal(with_res$actual, with_res$expected)
})


test_that("vec_from_store_byname() works in a data frame with Matrix objects", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in GBR]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, -9999, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  df <- tibble::tibble(a = list(a, a, a), 
                       v = list(v, v, v), 
                       expected = list(expected, expected, expected))
  
  with_res <- df %>%
    dplyr::mutate(
      actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from", 
                                     missing = -9999)
    )
  expect_true(all(mapply(matsbyname:::expect_equal_matrix_or_Matrix, with_res$actual, with_res$expected)))
})


test_that("rename_to_piece_byname() works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  res1 <- rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
  expected1 <- m
  dimnames(expected1) <- list(c("a", "r2", "r3"), c("a", "c"))
  expect_equal(res1, expected1)
  
  res2 <- rename_to_piece_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
  expected2 <- m
  dimnames(expected2) <- list(c("b", "", ""), c("b", "d"))
  expect_equal(res2, expected2)
  
  # Check that it works for different margins.
  res3 <- rename_to_piece_byname(m, piece = "pref", margin = 1,
                                 notation = RCLabels::arrow_notation)
  expected3 <- m
  dimnames(expected3) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res3, expected3)
  
  res4 <- rename_to_piece_byname(m, piece = "suff", margin = 2,
                                 notation = RCLabels::arrow_notation)
  expected4 <- m
  dimnames(expected4) <- list(c("a -> b", "r2", "r3"), c("b", "d"))
  expect_equal(res4, expected4)
  
  # Check that it works in a list.
  res5 <- rename_to_piece_byname(list(m, m), piece = list("pref", "suff"), 
                                 margin = list(1, 2),
                                 notation = RCLabels::arrow_notation)
  expected5 <- list(expected3, expected4)
  expect_equal(res5, expected5)
  
  # Check that margins can be determined from types.
  m2 <- m %>%
    setrowtype("rows") %>% setcoltype("cols")
  res6 <- rename_to_piece_byname(m2, piece = "pref", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected6 <- m2
  dimnames(expected6) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res6, expected6)
  
  res7 <- rename_to_piece_byname(m2, piece = "suff", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected7 <- m2
  dimnames(expected7) <- list(c("b", "", ""), c("a -> b", "c -> d"))
  expected7 <- expected7 %>%
    setrowtype("")
  expect_equal(res7, expected7)
})


test_that("rename_to_piece_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(c(1, 2, 
                            3, 4, 
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE, 
                          dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  res1 <- rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
  expected1 <- m
  dimnames(expected1) <- list(c("a", "r2", "r3"), c("a", "c"))
  expect_true(is.Matrix(res1))
  expect_equal(res1, expected1)
  
  res2 <- rename_to_piece_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
  expected2 <- m
  dimnames(expected2) <- list(c("b", "", ""), c("b", "d"))
  expect_equal(res2, expected2)
  
  # Check that it works for different margins.
  res3 <- rename_to_piece_byname(m, piece = "pref", margin = 1,
                                 notation = RCLabels::arrow_notation)
  expected3 <- m
  dimnames(expected3) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res3, expected3)
  
  res4 <- rename_to_piece_byname(m, piece = "suff", margin = 2,
                                 notation = RCLabels::arrow_notation)
  expected4 <- m
  dimnames(expected4) <- list(c("a -> b", "r2", "r3"), c("b", "d"))
  expect_equal(res4, expected4)
  
  # Check that it works in a list.
  res5 <- rename_to_piece_byname(list(m, m), piece = list("pref", "suff"), 
                                 margin = list(1, 2),
                                 notation = RCLabels::arrow_notation)
  expected5 <- list(expected3, expected4)
  expect_equal(res5, expected5)
  
  # Check that margins can be determined from types.
  m2 <- m %>%
    setrowtype("rows") %>% setcoltype("cols")
  res6 <- rename_to_piece_byname(m2, piece = "pref", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected6 <- m2
  dimnames(expected6) <- list(c("a", "r2", "r3"), c("a -> b", "c -> d"))
  expect_equal(res6, expected6)
  
  res7 <- rename_to_piece_byname(m2, piece = "suff", margin = "rows",
                                 notation = RCLabels::arrow_notation)
  expected7 <- m2
  dimnames(expected7) <- list(c("b", "", ""), c("a -> b", "c -> d"))
  expected7 <- expected7 %>%
    setrowtype("")
  expect_equal(res7, expected7)
})


test_that("rename_to_piece_byname() works as expected when inferring notation", {
  # Test with one that fails to infer.
  bad_mat <- matrix(c(1, 2,
                      3, 4,
                      5, 6), nrow = 3, byrow = TRUE,
                    dimnames = list(c("ab", "c -> d", "e -> f"), c("g -> h", "i -> j")))
  expected <- bad_mat
  rownames(expected) <- c("ab", "c", "e")
  colnames(expected) <- c("g", "i")
  expect_equal(rename_to_piece_byname(bad_mat, piece = "pref"), expected)
  
  m <- matrix(c(1, 2,
                3, 4,
                5, 6), nrow = 3, byrow = TRUE,
              dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j")))
  res1 <- rename_to_piece_byname(m, piece = "pref")
  expected1 <- m
  dimnames(expected1) <- list(c("a", "c", "e"), c("g", "i"))
  expect_equal(res1, expected1)
  
  # Test with other inferred pieces (suffix, prepositions)
  res2 <- rename_to_piece_byname(m, piece = "suff")
  expected2 <- m
  dimnames(expected2) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res2, expected2)
  
  m2 <- matrix(c(1, 2,
                 3, 4,
                 5, 6), nrow = 3, byrow = TRUE,
               dimnames = list(c("a [in b]", "c [in d]", "e [in f]"), c("g [in h]", "i [in j]")))
  res3 <- rename_to_piece_byname(m2, piece = "pref")
  expected3 <- m2
  dimnames(expected3) <- list(c("a", "c", "e"), c("g", "i"))
  expect_equal(res3, expected3)
  
  # This one picks up the full suffix, because not choosing most specific,
  # which defaults to the first matching notation, which is bracket notation.
  res4 <- rename_to_piece_byname(m2, piece = "suff")
  expected4 <- m2
  dimnames(expected4) <- list(c("in b", "in d", "in f"), c("in h", "in j"))
  expect_equal(res4, expected4)
  
  # Now choose the most specific, which will be the in notation.
  # Thus, we will have b, d, f, h, and j as the row and column names.
  res5 <- rename_to_piece_byname(m2, piece = "suff", choose_most_specific = TRUE)
  expected5 <- m2
  dimnames(expected5) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res5, expected5)
  
  # Choose the object of the "in" preposition.
  res6 <- rename_to_piece_byname(m2, piece = "in")
  expected6 <- m2
  dimnames(expected6) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res6, expected6)
  
  # Try in a data frame with different notations.
  df <- tibble::tribble(~m, ~piece, ~cms, ~expected, 
                        m2, "pref", FALSE, expected3,
                        m2, "suff", FALSE, expected4,
                        m2, "suff", TRUE, expected5,
                        m2, "in", FALSE, expected6)
  res <- df %>% 
    dplyr::mutate(res = rename_to_piece_byname(a = m, piece = piece, choose_most_specific = cms))
  for (i in 1:nrow(df)) {
    expect_equal(res$res[[i]], res$expected[[i]])
  }
})


test_that("rename_to_piece_byname() works as expected when inferring notation with Matrix objects", {
  # Test with one that fails to infer.
  bad_mat <- matsbyname::Matrix(c(1, 2,
                                  3, 4,
                                  5, 6), nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("ab", "c -> d", "e -> f"), c("g -> h", "i -> j")), 
                                rowtype = "rows", coltype = "cols")
  expected <- bad_mat
  rownames(expected) <- c("ab", "c", "e")
  colnames(expected) <- c("g", "i")
  expect_equal(rename_to_piece_byname(bad_mat, piece = "pref"), expected)
  
  m <- matsbyname::Matrix(c(1, 2,
                            3, 4,
                            5, 6), nrow = 3, ncol = 2, byrow = TRUE,
                          dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j")), 
                          rowtype = "rows", coltype = "cols")
  res1 <- rename_to_piece_byname(m, piece = "pref")
  expected1 <- m
  dimnames(expected1) <- list(c("a", "c", "e"), c("g", "i"))
  expect_equal(res1, expected1)
  
  # Test with other inferred pieces (suffix, prepositions)
  res2 <- rename_to_piece_byname(m, piece = "suff")
  expected2 <- m
  dimnames(expected2) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res2, expected2)
  
  m2 <- matsbyname::Matrix(c(1, 2,
                             3, 4,
                             5, 6), nrow = 3, ncol = 2, byrow = TRUE,
                           dimnames = list(c("a [in b]", "c [in d]", "e [in f]"), c("g [in h]", "i [in j]")))
  res3 <- rename_to_piece_byname(m2, piece = "pref")
  expected3 <- m2
  dimnames(expected3) <- list(c("a", "c", "e"), c("g", "i"))
  expect_equal(res3, expected3)
  
  # This one picks up the full suffix, because not choosing most specific,
  # which defaults to the first matching notation, which is bracket notation.
  res4 <- rename_to_piece_byname(m2, piece = "suff")
  expected4 <- m2
  dimnames(expected4) <- list(c("in b", "in d", "in f"), c("in h", "in j"))
  expect_equal(res4, expected4)
  
  # Now choose the most specific, which will be the in notation.
  # Thus, we will have b, d, f, h, and j as the row and column names.
  res5 <- rename_to_piece_byname(m2, piece = "suff", choose_most_specific = TRUE)
  expected5 <- m2
  dimnames(expected5) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res5, expected5)
  
  # Choose the object of the "in" preposition.
  res6 <- rename_to_piece_byname(m2, piece = "in")
  expected6 <- m2
  dimnames(expected6) <- list(c("b", "d", "f"), c("h", "j"))
  expect_equal(res6, expected6)
  
  # Try in a data frame with different notations.
  df <- tibble::tribble(~m, ~piece, ~cms, ~expected, 
                        m2, "pref", FALSE, expected3,
                        m2, "suff", FALSE, expected4,
                        m2, "suff", TRUE, expected5,
                        m2, "in", FALSE, expected6)
  res <- df %>% 
    dplyr::mutate(res = rename_to_piece_byname(a = m, piece = piece, choose_most_specific = cms))
  for (i in 1:nrow(df)) {
    expect_equal(res$res[[i]], res$expected[[i]])
  }
})


test_that("rename_to_piece_byname() works with inferred margins", {
  m1 <- matrix(c(1, 2, 3,
                 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
               dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                               c("Motors -> MD", "Cars -> MD", "LED lamps -> Light"))) %>% 
    setrowtype("Product [from Product]") %>% setcoltype("Industry -> Product")
  res1 <- rename_to_piece_byname(m1, 
                                 piece = "pref",
                                 margin = "Product [from Product]", 
                                 choose_most_specific = TRUE)
  expected1 <- m1
  expected1 <- setrownames_byname(expected1, c("Electricity", "Electricity"))
  expected1 <- setrowtype(expected1, "Product")
  expect_equal(res1, expected1)
  
  # Make sure it also works when the type doesn't have the same structure
  # as the row and column names.
  m2 <- m1 %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  res2 <- rename_to_piece_byname(m2, piece = "pref", margin = "Product")
  expected2 <- m2
  expected2 <- setrownames_byname(expected2, c("Electricity", "Electricity"))
  expect_equal(res2, expected2)
})


test_that("rename_to_piece_byname() works with inferred margins in Matrix objects", {
  m1 <- matsbyname::Matrix(c(1, 2, 3,
                             4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                           dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                                           c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")), 
                           rowtype = "Product [from Product]", coltype = "Industry -> Product")
  res1 <- rename_to_piece_byname(m1, 
                                 piece = "pref",
                                 margin = "Product [from Product]", 
                                 choose_most_specific = TRUE)
  expected1 <- m1
  expected1 <- setrownames_byname(expected1, c("Electricity", "Electricity"))
  expected1 <- setrowtype(expected1, "Product")
  expect_equal(res1, expected1)
  
  # Make sure it also works when the type doesn't have the same structure
  # as the row and column names.
  m2 <- m1 %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  res2 <- rename_to_piece_byname(m2, piece = "pref", margin = "Product")
  expected2 <- m2
  expected2 <- setrownames_byname(expected2, c("Electricity", "Electricity"))
  expect_equal(res2, expected2)
})


test_that("margin_from_types_byname() works as expected", {
  # Try with a single matrix
  m <- matrix(1) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  expect_equal(margin_from_types_byname(m, "Product"), 1)
  expect_equal(margin_from_types_byname(m, "Industry"), 2)
  expect_equal(margin_from_types_byname(m, c("Product", "Industry")), c(1, 2))
  expect_equal(margin_from_types_byname(m, c("Industry", "Product")), c(1, 2))
  
  # Try with a type that isn't in the row or column types.
  expect_equal(margin_from_types_byname(m, "bogus"), NA_integer_)
  
  # Try with one type that IS in the row or column types and one type that is not.
  expect_equal(margin_from_types_byname(m, c("bogus", "Product")), 1)
  
  # Try with a non-character types argument
  expect_equal(margin_from_types_byname(m, c(1, 2)), c(1, 2))
  
  # Try with a list of matrices
  expect_equal(margin_from_types_byname(list(m, m), types = "Product"), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = "Industry"), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Product")), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Industry", "Industry")), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"), 
                                                                 c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
  
  # Try in a data frame.
  m2 <- matrix(2) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  df <- tibble::tibble(m = list(m, m2), 
                       types1 = list("Product", "Industry"), 
                       types2 = list(c("Product", "Industry"), "Industry"), 
                       types3 = "bogus")
  res <- df %>%
    dplyr::mutate(
      margin1 = margin_from_types_byname(m, types1), 
      margin2 = margin_from_types_byname(m, types2), 
      margin3 = margin_from_types_byname(m, types3)
    )
  
  expect_equal(res$margin1, list(1, 1))
  expect_equal(res$margin2, list(c(1, 2), 1))
  expect_equal(res$margin3, list(NA_integer_, NA_integer_))
})


test_that("margin_from_types_byname() works for Matrix objects", {
  # Try with a single matrix
  m <- matsbyname::Matrix(1) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  expect_equal(margin_from_types_byname(m, "Product"), 1)
  expect_equal(margin_from_types_byname(m, "Industry"), 2)
  expect_equal(margin_from_types_byname(m, c("Product", "Industry")), c(1, 2))
  expect_equal(margin_from_types_byname(m, c("Industry", "Product")), c(1, 2))
  
  # Try with a type that isn't in the row or column types.
  expect_equal(margin_from_types_byname(m, "bogus"), NA_integer_)
  
  # Try with one type that IS in the row or column types and one type that is not.
  expect_equal(margin_from_types_byname(m, c("bogus", "Product")), 1)
  
  # Try with a non-character types argument
  expect_equal(margin_from_types_byname(m, c(1, 2)), c(1, 2))
  
  # Try with a list of matrices
  expect_equal(margin_from_types_byname(list(m, m), types = "Product"), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = "Industry"), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Product")), 
               list(1, 1))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Industry", "Industry")), 
               list(2, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = c("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list("Product", "Industry")), 
               list(1, 2))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
  expect_equal(margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"), 
                                                                 c("Product", "Industry"))), 
               list(c(1, 2), c(1, 2)))
  
  # Try in a data frame.
  m2 <- matsbyname::Matrix(2) %>%
    setrowtype("Industry") %>% setcoltype("Product")
  df <- tibble::tibble(m = list(m, m2), 
                       types1 = list("Product", "Industry"), 
                       types2 = list(c("Product", "Industry"), "Industry"), 
                       types3 = "bogus")
  res <- df %>%
    dplyr::mutate(
      margin1 = margin_from_types_byname(m, types1), 
      margin2 = margin_from_types_byname(m, types2), 
      margin3 = margin_from_types_byname(m, types3)
    )
  
  expect_equal(res$margin1, list(1, 1))
  expect_equal(res$margin2, list(c(1, 2), 1))
  expect_equal(res$margin3, list(NA_integer_, NA_integer_))
})


test_that("prep_vector_arg() works as expected", {
  a <- matsbyname::Matrix(1)
  res <- prep_vector_arg(a, list(RCLabels::notations_list))
  expect_equal(res, RCLabels::notations_list)
  
  # Now try with a list of 2 matrices
  a <- list(matsbyname::Matrix(1), matsbyname::Matrix(1))
  res <- prep_vector_arg(a, list(RCLabels::notations_list))
  expect_equal(res, list(RCLabels::notations_list))
})


test_that("list_of_rows_or_cols() works as expected", {
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
  DF <- DF %>% dplyr::mutate(
    extracted_rows = list_of_rows_or_cols(m, margin = 1), 
    extracted_cols = list_of_rows_or_cols(m, margin = 2)
  )
  expect_equal(DF$extracted_rows, list(expected_margin_1, expected_margin_1))
  expect_equal(DF$extracted_cols, list(expected_margin_2, expected_margin_2))
})


test_that("list_of_rows_or_cols() works with Matrix objects", {
  m <- matsbyname::Matrix(data = c(1:6), nrow = 2, ncol = 3, dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>% 
    setrowtype(rowtype = "Products") %>% setcoltype(coltype = "Industries")
  expected_margin_1 <- list(p1 = matsbyname::Matrix(seq(1, 5, by = 2), nrow = 3, ncol = 1, dimnames = list(c("i1", "i2", "i3"), "p1")) %>% 
                              setrowtype("Industries") %>% setcoltype("Products"), 
                            p2 = matsbyname::Matrix(seq(2, 6, by = 2), nrow = 3, ncol = 1, dimnames = list(c("i1", "i2", "i3"), "p2")) %>% 
                              setrowtype("Industries") %>% setcoltype("Products"))
  expected_margin_2 <- list(i1 = matsbyname::Matrix(1:2, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i1")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"),
                            i2 = matsbyname::Matrix(3:4, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i2")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"),
                            i3 = matsbyname::Matrix(5:6, nrow = 2, ncol = 1, dimnames = list(c("p1", "p2"), "i3")) %>% 
                              setrowtype("Products") %>% setcoltype("Industries"))
  expect_equal(list_of_rows_or_cols(m, margin = 1), expected = expected_margin_1)
  expect_equal(list_of_rows_or_cols(m, margin = 2), expected = expected_margin_2)
  
  # Using data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  DF <- DF %>% dplyr::mutate(
    extracted_rows = list_of_rows_or_cols(m, margin = 1), 
    extracted_cols = list_of_rows_or_cols(m, margin = 2)
  )
  expect_equal(DF$extracted_rows, list(expected_margin_1, expected_margin_1))
  expect_equal(DF$extracted_cols, list(expected_margin_2, expected_margin_2))
})


test_that("organize_args() works as expected", {
  # If only one argument is a list, make the other argument also a list of equal length.
  expect_equal(matsbyname:::organize_args(a = list(1,2), b = 3), list(a = list(1,2), b = list(3,3)))
  expect_equal(matsbyname:::organize_args(a = 3, b = list(1,2)), list(a = list(3,3), b = list(1,2)))
  
  # If both arguments are lists, ensure that they are same length.
  expect_equal(matsbyname:::organize_args(a = list(1,2,3), b = list(4,5,6)), list(a = list(1,2,3), b = list(4,5,6)))
  expect_error(matsbyname:::organize_args(a = list(1,2,3), b = list(4,5,6,7)), "length\\(a\\) == length\\(b\\) is not TRUE") 
  
  # If one argument is a matrix and the other is a constant, make the constant into a matrix.
  m <- matrix(c(1,2,3,4), nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(matsbyname:::organize_args(a = m, b = 2), 
               list(a = m, b = matrix(2, nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries")))
  
  # Ensures that row and column types match
  # Completes and sorts the matrices
  n <- matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  # Neither row nor column types match, but error will say column types are mismatched.
  expect_error(matsbyname:::organize_args(a = m %>% setrowtype("rows"), b = n), 
               "rowtype\\(a\\) \\(rows\\) != rowtype\\(b\\) \\(Products\\).")
  # By setting the rowtype to match, the error should shift to mismatched column types
  expect_error(matsbyname:::organize_args(a = m, b = n %>% setcoltype("cols")), 
               "coltype\\(a\\) \\(Industries\\) != coltype\\(b\\) \\(cols\\).")
  # This should work, because the rowtype and coltype are now same for both
  expect_equal(matsbyname:::organize_args(a = m, b = n), 
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
  expect_equal(matsbyname:::organize_args(a = list(1, 2), b = m), list(a = list(1, 2), b = list(m, m)))
  expect_equal(matsbyname:::organize_args(a = n, b = list(m, m)), list(a = list(n, n), b = list(m, m)))
  
  # Test the match_type argument
  p <- transpose_byname(n)
  # If we don't specify match_type = "matmult", 
  # organize_args will try to ensure that rows of m and rows of p are same type. 
  # organize_args will also try to ensure that cols of m and cols of p are same type.
  # These aren't true, so this will error.
  expect_error(matsbyname:::organize_args(a = m, b = p), 
               "rowtype\\(a\\) \\(Products\\) != rowtype\\(b\\) \\(Industries\\).")
  # When we say match_type = "matmult", we indicate that the columns of a and the rows of b must match.
  expect_equal(matsbyname:::organize_args(a = m, b = p, match_type = "matmult"), list(a = m, b = p))
  
  # Test that values are filled appropriately.
  expect_equal(matsbyname:::organize_args(a = NULL, b = 5, fill = 42), list(a = 42, b = 5))
  expect_equal(matsbyname:::organize_args(b = 5, fill = 42), list(a = 42, b = 5))
  expect_equal(matsbyname:::organize_args(a = 3, b = NULL, fill = 42), list(a = 3, b = 42))
  expect_equal(matsbyname:::organize_args(a = 3, fill = 42), list(a = 3, b = 42))
})


test_that("organize_args() works with Matrix objects", {
  
  # If one argument is a matrix and the other is a constant, make the constant into a matrix.
  m <- matsbyname::Matrix(c(1,2,3,4), nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(matsbyname:::organize_args(a = m, b = 2), 
               list(a = m, b = matsbyname::Matrix(2, nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries")))
  
  # Ensures that row and column types match
  # Completes and sorts the matrices
  n <- matsbyname::Matrix(c(1:6), nrow = 3, ncol = 2, dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  # Neither row nor column types match, but error will say column types are mismatched.
  expect_error(matsbyname:::organize_args(a = m %>% setrowtype("rows"), b = n), 
               "rowtype\\(a\\) \\(rows\\) != rowtype\\(b\\) \\(Products\\).")
  # By setting the rowtype to match, the error should shift to mismatched column types
  expect_error(matsbyname:::organize_args(a = m, b = n %>% setcoltype("cols")), 
               "coltype\\(a\\) \\(Industries\\) != coltype\\(b\\) \\(cols\\).")
  # This should work, because the rowtype and coltype are now same for both
  expect_equal(matsbyname:::organize_args(a = m, b = n), 
               list(a = matsbyname::Matrix(c(1,3,
                                             2,4,
                                             0,0),
                                           nrow = 3, ncol = 2, byrow = TRUE,
                                           dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries"), 
                    b = matsbyname::Matrix(c(1,4,
                                             2,5,
                                             3,6),
                                           nrow = 3, ncol = 2, byrow = TRUE,
                                           dimnames = list(c("p1", "p2", "p3"), c("i1", "i2"))) %>% 
                      setrowtype("Products") %>% setcoltype("Industries")))
  
  # If one argument is a list and the other argument is a matrix, duplicate the matrix to match the length of the list
  expect_equal(matsbyname:::organize_args(a = list(1, 2), b = m), list(a = list(1, 2), b = list(m, m)))
  expect_equal(matsbyname:::organize_args(a = n, b = list(m, m)), list(a = list(n, n), b = list(m, m)))
  
  # Test the match_type argument
  p <- transpose_byname(n)
  # If we don't specify match_type = "matmult", 
  # organize_args will try to ensure that rows of m and rows of p are same type. 
  # organize_args will also try to ensure that cols of m and cols of p are same type.
  # These aren't true, so this will error.
  expect_error(matsbyname:::organize_args(a = m, b = p), 
               "rowtype\\(a\\) \\(Products\\) != rowtype\\(b\\) \\(Industries\\).")
  # When we say match_type = "matmult", we indicate that the columns of a and the rows of b must match.
  expect_equal(matsbyname:::organize_args(a = m, b = p, match_type = "matmult"), list(a = m, b = p))
})

