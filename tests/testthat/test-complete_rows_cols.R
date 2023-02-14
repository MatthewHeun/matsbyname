test_that("complete_rows_cols() works as expected", {
  
  # a is NULL, matrix is NULL.  Error.
  expect_error(complete_rows_cols(a = NULL), "Both a and mat are NULL in complete_rows_cols.")
  
  # a is NULL, matrix is present.  
  # Create a matrix of same size as matrix with names of matrix and values equal to fill.
  expect_equal(complete_rows_cols(a = NULL, mat = matrix(1, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))), fill = 42), 
               matrix(42, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))))
  
  # a is a single number, matrix is NULL.  a is returned.
  expect_equal(complete_rows_cols(a = 42), 42)
  
  # a is a single number, a named matrix is present, and each item has length 1.  
  # This should fail. 
  # There is no way to know which row or column names are already present.
  # Therefore, there is now way to know how to complete the number a.
  expect_error(complete_rows_cols(a = 42, mat = matrix(0, nrow = 1, ncol = 1, dimnames = list("row", "col"))))
  
  # If you want to take a single number and convert it to 
  # a matrix with named rows and columns, use
  # complete_rows_cols(a = NULL, mat = matrix(0, nrow = 1, ncol = 1, dimnames = list("row", "col"), fill = <<your number>>).
  expect_equal(complete_rows_cols(a = NULL, 
                                  mat = matrix(0, nrow = 1, ncol = 1, dimnames = list("row", "col")), 
                                  fill = 42), 
               matrix(42, nrow = 1, ncol = 1, dimnames = list("row", "col")))
  
  # a is a matrix without dimnames, names is NULL, matrix is NULL.  a is returned.
  expect_equal(complete_rows_cols(a = matrix(42, nrow = 3, ncol = 2)), matrix(42, nrow = 3, ncol = 2))
  
  # a is a matrix without dimnames, a matrix names is present.  
  # This is a degenerate case.
  # We don't know what row and column names are already present in a.
  # So we can't know how to complete a with names from matrix.
  # This should give an error.
  expect_error(complete_rows_cols(a = matrix(42, nrow = 3, ncol = 2), 
                                  mat = matrix(0, nrow = 3, ncol = 2, 
                                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))), 
               "Can't complete a that is missing dimnames with non-NULL dimnames on mat.")
  
  # a is a matrix with dimnames, mat is NULL.  a is completed relative to itself
  expect_equal(complete_rows_cols(a = matrix(42, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))), 
               matrix(c(42, 0, 0, 
                        42, 0, 0, 
                         0, 0, 0), byrow = TRUE, 
                      nrow = 3, ncol = 3, dimnames = list(c("r1", "r2", "c1"), c("c1", "r1", "r2"))))
  
  # a is a matrix with dimnames, names is NULL, but matrix is present.  Take names from matrix to complete a.
  expect_equal(complete_rows_cols(a = matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))),
                                  mat = matrix(42, nrow = 4, ncol = 3, 
                                               dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2", "c3")))), 
               matrix(c(1, 4, 0, 
                        2, 5, 0,
                        3, 6, 0,
                        0, 0, 0), nrow = 4, ncol = 3, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2", "c3"))))
  
  # In this example, a is non-NULL and has dimnames.
  # But we have no names. 
  # There is no "names" argument, and matrix has no row or column names.
  # So there is no way to know how to complete a.
  # So we give a warning and complete m1 relative to itself.
  m1 <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  m2 <- matrix(c(7:12), ncol = 3)
  expect_warning(complete_rows_cols(a = m1, mat = m2), 
                 "NULL names in complete_rows_cols\\(\\), despite 'mat' being specified. Completing a relative to itself.")
  
  res <- complete_rows_cols(a = m1, mat = m2) %>% 
    suppressWarnings()
  
  # Now test that the result is correct.
  expect_equal(res, matrix(c(1, 4, 0, 0, 0,
                             2, 5, 0, 0, 0, 
                             3, 6, 0, 0, 0, 
                             0, 0, 0, 0, 0, 
                             0, 0, 0, 0, 0), byrow = TRUE,
                           nrow = 5, ncol = 5, 
                           dimnames = list(c("r1", "r2", "r3", "c1", "c2"), c("c1", "c2", "r1", "r2", "r3"))))
  
  # Now give m2 some dimnames.
  m2 <- matrix(c(7:12), ncol = 3, dimnames = list(c("r2", "r3"), c("c2", "c3", "c4")))
  complete_m1_m2 <- matrix(c(1,4,0,0,
                             2,5,0,0,
                             3,6,0,0),
                           nrow = 3, byrow = TRUE, 
                           dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4")))
  # Adds empty columns c3 and c4
  expect_equal(complete_rows_cols(m1, m2), complete_m1_m2)
  # Creates columns r2, r3; rows c2, c3, c4
  expect_equal(complete_rows_cols(m1, t(m2)),
               matrix(c(1,4,0,0,
                        2,5,0,0,
                        3,6,0,0,
                        0,0,0,0,
                        0,0,0,0,
                        0,0,0,0), 
                      nrow = 6, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3", "c2", "c3", "c4"), c("c1", "c2", "r2", "r3"))))
  # No changes because r2 and r3 already present in m1
  expect_equal(complete_rows_cols(m1, m2, margin = 1), m1)
  # Adds empty columns c3 and c4
  expect_equal(complete_rows_cols(m1, m2, margin = 2), 
               matrix(c(1,4,0,0,
                        2,5,0,0,
                        3,6,0,0),
                      nrow = 3, byrow = TRUE,
                      dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))))
  # Adds empty rows c2, c3, c4
  expect_equal(complete_rows_cols(m1, t(m2), margin = 1), 
               matrix(c(1,4,
                        2,5,
                        3,6,
                        0,0,
                        0,0,
                        0,0), 
                      nrow = 6, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3", "c2", "c3", "c4"), c("c1", "c2"))))
  # Adds columns c3 and c4 with 100's
  expect_equal(complete_rows_cols(m1, m2, fill = 100), 
               matrix(c(1,4,100,100,
                        2,5,100,100,
                        3,6,100,100),
                      nrow = 3, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))))
  # Doesn't work with data frames.  Need a matrix.
  expect_error(complete_rows_cols(data.frame(m1), data.frame(m2)), 
               "a cannot be a data frame in complete_rows_cols.")
  # Nothing added, because everything already present
  expect_equal(complete_rows_cols(m1, m1), m1)
  # Adds empty rows c1, c2 ; Adds empty columns r1, r2, r3 
  expect_equal(complete_rows_cols(m1, t(m1)), 
               matrix(c(1,4,0,0,0,
                        2,5,0,0,0,
                        3,6,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0),
                      nrow = 5, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3", "c1", "c2"), c("c1", "c2", "r1", "r2", "r3"))))
  # Same as previous. With missing matrix, complete relative to transpose of itself.
  expect_equal(complete_rows_cols(m1), 
               matrix(c(1,4,0,0,0,
                        2,5,0,0,0,
                        3,6,0,0,0,
                        0,0,0,0,0,
                        0,0,0,0,0),
                      nrow = 5, byrow = TRUE, 
                      dimnames = list(c("r1", "r2", "r3", "c1", "c2"), c("c1", "c2", "r1", "r2", "r3"))))
  # Adds rows r10, r11; cols c10, c11
  complete_m1_m2_new_names <- matrix(c(1,4,0,0,
                                       2,5,0,0,
                                       3,6,0,0,
                                       0,0,0,0,
                                       0,0,0,0),
                                     nrow = 5, byrow = TRUE,
                                     dimnames = list(c("r1", "r2", "r3", "r10", "r11"), c("c1", "c2", "c10", "c11")))
  expect_equal(complete_rows_cols(m1, mat = matrix(0, nrow = 2, ncol = 2, 
                                                   dimnames = list(c("r10", "r11"), c("c10", "c11")))), 
               complete_m1_m2_new_names)
  
  
  # Also works with lists
  expect_equal(complete_rows_cols(a = list(m1,m1), margin = list(c(1, 2))), list(complete_rows_cols(m1), complete_rows_cols(m1)))
  expect_equal(complete_rows_cols(a = list(m1,m1), margin = list(c(1, 2)), mat = list(m2,m2)), list(complete_m1_m2, complete_m1_m2))
  # No changes because r2, r3 already present in m1
  expect_equal(complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 1), list(m1, m1))
  expect_equal(complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 2), 
               list(complete_m1_m2, complete_m1_m2))
  
  # Check what happens when a is a list but m1 is a single matrix.
  expect_equal(complete_rows_cols(a = list(m1,m1), mat = m2, margin = 1), list(m1, m1))
  
  # Test what happens when matrices are missing row or column names or both.
  A <- matrix(1:4, nrow = 2)
  expect_equal(complete_rows_cols(A), A)
  expect_equal(complete_rows_cols(A %>% magrittr::set_rownames(c("r1", "r2")) %>% magrittr::set_colnames(c("c1", "c2"))), 
               matrix(c(1, 3, 0, 0, 
                        2, 4, 0, 0, 
                        0, 0, 0, 0,
                        0, 0, 0, 0), byrow = TRUE, nrow = 4, ncol = 4,
                      dimnames = list(c("r1", "r2", "c1", "c2"), 
                                      c("c1", "c2", "r1", "r2")))
  )
  
  # Test with list for x and NULL for matrix
  expect_equal(complete_rows_cols(a = list(A, A)), list(A, A))
  B <- A %>% magrittr::set_rownames(c("r1", "r2")) %>% magrittr::set_colnames(c("c1", "c2")) %>% 
    setrowtype("row") %>% setcoltype("col")
  B_completed <- matrix(c(1, 3, 0, 0, 
                          2, 4, 0, 0, 
                          0, 0, 0, 0, 
                          0, 0, 0, 0), byrow = TRUE, nrow = 4, ncol = 4,
                        dimnames = list(c("r1", "r2", "c1", "c2"), c("c1", "c2", "r1", "r2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(complete_rows_cols(a = B), B_completed)
  expect_equal(complete_rows_cols(a = list(B, B)), list(B_completed, B_completed))
  
  B_filled <- matrix(42, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(complete_rows_cols(mat = list(B, B), fill = 42), list(B_filled, B_filled))
})


test_that("completing fails when dimnames aren't present for a", {
  a <- matrix(1:4, byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(NULL, c("c1", "c2")))
  mata <- matrix(1:6, byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(complete_rows_cols(a = a, mat = mata, margin = 1), 
               "NULL dimnames for margin = 1 on a")
  
  b <- matrix(1:4, byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), NULL))
  matb <- matrix(1:6, byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(complete_rows_cols(a = b, mat = matb, margin = 2), 
               "NULL dimnames for margin = 2 on a")
})


test_that("complete_rows_cols() works when list is present and lists and ... have different lengths", {
  m1 <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  m2 <- matrix(c(7:12), ncol = 3, dimnames = list(c("r2", "r3"), c("c2", "c3", "c4")))
  complete_m1_m2 <- matrix(c(1,4,0,0,
                             2,5,0,0,
                             3,6,0,0),
                           nrow = 3, byrow = TRUE, 
                           dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4")))
  # Adds empty columns c3 and c4
  expect_equal(complete_rows_cols(m1, m2), complete_m1_m2)
  # Try in a list
  expect_equal(complete_rows_cols(a = list(m1, m1, m1), 
                                  mat = list(m2, m2, m2)), 
               list(complete_m1_m2, complete_m1_m2, complete_m1_m2))
  # Now try with different arguments
  expect_equal(complete_rows_cols(m1, m2, fill = 0, margin = c(1,2)), complete_m1_m2)
  # And in a list
  expect_equal(complete_rows_cols(list(m1, m1, m1), list(m2, m2, m2), fill = 0, margin = c(1,2)), 
               list(complete_m1_m2, complete_m1_m2, complete_m1_m2))
})


test_that("complete_rows_cols() works correctly when x is unspecified and fillrow is specified", {
  mat <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  # Try with wrong dimnames
  expect_error(complete_rows_cols(mat = mat, fillrow = matrix(42, nrow = 1, ncol = 2, dimnames = list("r42", c("c3", "c4")))), 
               "colnames of fillrow must match colnames of mat in complete_rows_cols.")
  expect_equal(complete_rows_cols(mat = mat, fillrow = matrix(c(11, 12), nrow = 1, ncol = 2, dimnames = list("r42", c("c1", "c2")))), 
               matrix(c(11, 12, 
                        11, 12), byrow = TRUE, nrow = 2, ncol = 2, dimnames = dimnames(mat)))
})


test_that("complete_rows_cols() works correctly when x is unspecified and fillcol is specified", {
  mat <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  # Try with wrong dimnames
  expect_error(complete_rows_cols(mat = mat, fillcol = matrix(42, nrow = 2, ncol = 1, dimnames = list(c("r3", "r4"), "c42"))), 
               "rownames of fillcol must match rownames of mat in complete_rows_cols.")
  # Try with correct dimnames
  expect_equal(complete_rows_cols(mat = mat, fillcol = matrix(c(11, 21), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c42"))), 
               matrix(c(11, 11, 
                        21, 21), byrow = TRUE, nrow = 2, ncol = 2, dimnames = dimnames(mat)))
})


test_that("complete_rows_cols() works correctly when fillrow is specified.", {
  a <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matrix(c(1:6), byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  # Test for problematic cases.
  # fillrow is not a matrix
  expect_error(complete_rows_cols(a, b, fillrow = 42), "fillrow must be a matrix or a Matrix in complete_rows_cols.")
  # Number of rows is greater than 1
  fillrow_tall <- matrix(c(42, 42), nrow = 2, ncol = 1, dimnames = list(c("r43", "r44"), "c2"))
  expect_error(complete_rows_cols(a, b, fillrow = fillrow_tall), "fillrow must be a matrix or a Matrix with one row in complete_rows_cols.")
  # Number of columns doesn't match
  fillrow_wide <- matrix(42, nrow = 1, ncol = 3, dimnames = list("r42", c("c1", "c2", "c3")))
  expect_equal(complete_rows_cols(a, b, fillrow = fillrow_wide), 
               matrix(c(11, 12, 
                        21, 22, 
                        42, 42), byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))))
  # Column names of fillrow don't match column names of x
  fillrow_badnames <- matrix(c(31, 32), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("r42", c("c3", "c4")))
  expect_error(complete_rows_cols(a = a, mat = b, fillrow = fillrow_badnames), "Some columns of matrix a are not present in matrix fillrow in complete_rows_cols.")
  # Test a case that should work
  fillrow <- matrix(c(31, 32), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("r42", c("c1", "c2")))
  expect_equal(complete_rows_cols(a = a, mat = b, fillrow = fillrow), 
               matrix(c(11, 12,
                        21, 22,
                        31, 32), byrow = TRUE, nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))))
})


test_that("complete_rows_cols() works correctly when fillcol is specified.", {
  a <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matrix(c(1:6), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  # Test for problematic cases.
  # fillcol is not a matrix
  expect_error(complete_rows_cols(a, b, fillcol = 42), "fillcol must be a matrix or a Matrix in complete_rows_cols.")
  # Number of columns is greater than 1
  fillcol_wide <- matrix(c(42, 42), nrow = 1, ncol = 2, dimnames = list("r42", c("c42", "c43")))
  expect_error(complete_rows_cols(a, b, fillcol = fillcol_wide), "fillcol must be a matrix or a Matrix with one column in complete_rows_cols.")
  # Number of rows doesn't match
  fillcol_tall <- matrix(42, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "c3"))
  expect_equal(complete_rows_cols(a, b, fillcol = fillcol_tall), 
               matrix(c(11, 12, 42, 
                        21, 22, 42), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))
  # Row names of fillcol don't match row names of x
  fillcol_badnames <- matrix(c(13, 23), nrow = 2, ncol = 1, dimnames = list(c("r3", "r4"), "c1"))
  expect_error(complete_rows_cols(a = a, mat = b, fillcol = fillcol_badnames), "Some rows of matrix a are not present in matrix fillcol in complete_rows_cols.")
  # Test a case that should work
  fillcol <- matrix(c(13, 23), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c42"))
  expect_equal(complete_rows_cols(a = a, mat = b, fillcol = fillcol), 
               matrix(c(11, 12, 13,
                        21, 22, 23), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3"))))
})


test_that("complete_rows_cols() works as expected with fillrow, fillcol collisions", {
  mat <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  fillrow <- matrix(c(11, 12), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("r42", c("c1", "c2")))
  fillcol <- matrix(c(11, 21), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c42"))
  expect_equal(complete_rows_cols(mat = mat, fillrow = fillrow, fillcol = fillcol), 
               matrix(c(11, 12,
                        11, 12), byrow = TRUE, nrow = 2, ncol = 2, dimnames = dimnames(mat)))
})


test_that("complete_rows_cols() works when orders are different for row fill", {
  a <- matrix(c(21, 22), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("r2", c("c1", "c2")))
  mat <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  fillrow <- matrix(c(1, 2), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("row", c("c1", "c2")))
  # fillrow is added to the bottom, as expected.
  expect_equal(complete_rows_cols(a = a, mat = mat, fillrow = fillrow, margin = 1), 
               matrix(c(21, 22, 1, 2), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r2", "r1"), c("c1", "c2"))))
  # Now try with a fillrow that has different column names. Should give an error.
  fillrow <- matrix(c(2, 1), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("row", c("c1", "c42")))
  expect_error(complete_rows_cols(a = a, mat = mat, fillrow = fillrow, margin = 1), 
               "Some columns of matrix a are not present in matrix fillrow in complete_rows_cols.")
  # Now try with a fillrow that has the correct column names, but they are out of order.
  # This works, because complete_rows_cols sorts the columns of fillrow to match
  # the columns of a.
  fillrow <- matrix(c(2, 1), byrow = TRUE, nrow = 1, ncol = 2, dimnames = list("row", c("c2", "c1")))
  expect_equal(complete_rows_cols(a = a, mat = mat, fillrow = fillrow, margin = 1), 
               matrix(c(21, 22, 1, 2), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r2", "r1"), c("c1", "c2"))))
  # Try with duplicated column names in matrix a.
  a_dup <- matrix(c(1, 2), nrow = 1, ncol = 2, dimnames = list("r1", c("c1", "c1")))
  expect_error(complete_rows_cols(a = a_dup, mat = mat, fillrow = fillrow, margin = 1), 
               "Duplicated column names found in matrix a in complete_rows_cols.")
  # Try with duplicated column names in fillrow.
  fillrow_dup <- matrix(c(1, 2), nrow = 1, ncol = 2, dimnames = list("row", c("c1", "c1")))
  expect_error(complete_rows_cols(a = a, mat = mat, fillrow = fillrow_dup, margin = 1), 
               "Duplicated column names found in matrix fillrow in complete_rows_cols.")
})


test_that("complete_rows_cols() works when orders are different for column fill", {
  a <- matrix(c(11, 21), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c2"))
  mat <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  fillcol <- matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "col"))
  # fillcol is added at the right, as expected.
  expect_equal(complete_rows_cols(a = a, mat = mat, fillcol = fillcol, margin = 2), 
               matrix(c(11, 1, 21, 2), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c2", "c1"))))
  # Now try with a fillcol that has different column names. Should give an error.
  fillcol <- matrix(c(2, 1),nrow = 2, ncol = 1, dimnames = list(c("r2", "r42"), "col"))
  expect_error(complete_rows_cols(a = a, mat = mat, fillcol = fillcol, margin = 2), 
               "Some rows of matrix a are not present in matrix fillcol in complete_rows_cols.")
  # Now try with a fillcol that has the correct row names, but they are out of order.
  # This works, because complete_rows_cols sorts the rows of fillcol to match
  # the columns of a.
  fillcol <- matrix(c(2, 1), nrow = 2, ncol = 1, dimnames = list(c("r2", "r1"), "col"))
  expect_equal(complete_rows_cols(a = a, mat = mat, fillcol = fillcol, margin = 2), 
               matrix(c(11, 1, 
                        21, 2), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c2", "c1"))))
  # Try with duplicated row names in matrix a.
  a_dup <- matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r1"), "c1"))
  expect_error(complete_rows_cols(a = a_dup, mat = mat, fillcol = fillcol, margin = 2), 
               "Duplicated row names found in matrix a in complete_rows_cols.")
  # Try with duplicated row names in fillcol.
  fillcol_dup <- matrix(c(1, 2), nrow = 2, ncol = 1, dimnames = list(c("r1", "r1"), "col"))
  expect_error(complete_rows_cols(a = a, mat = mat, fillcol = fillcol_dup, margin = 2), 
               "Duplicated row names found in matrix fillcol in complete_rows_cols.")
})


test_that("complete_rows_cols() works with a Matrix", {
  # A is a Matrix with dimnames, mat is NULL.  A is completed relative to itself
  A <- matsbyname::Matrix(42, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c1"))
  res1 <- complete_rows_cols(A)
  expected1 <- matsbyname::Matrix(c(42, 0, 0, 
                                    42, 0, 0, 
                                    0, 0, 0), byrow = TRUE, 
                                  nrow = 3, ncol = 3, dimnames = list(c("r1", "r2", "c1"), c("c1", "r1", "r2")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1, expected1)

  # Try with 2 Matrix objects
  # Add one row (r3) and one column (c2)
  B <- matsbyname::Matrix(0, nrow = 1, ncol = 1, dimnames = list("r3", "c2"))
  res2 <- complete_rows_cols(A, B)
  expected2 <- matsbyname::Matrix(c(42, 0, 
                                    42, 0, 
                                    0, 0), byrow = TRUE, 
                                  nrow = 3, ncol = 2, 
                                  dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  matsbyname:::expect_equal_matrix_or_Matrix(res2, expected2)
})


test_that("complete_rows_cols() works with a 0x0 matrix", {
  m <- matrix(c(1), dimnames = list("r1", "c1"))
  # Make a 0x0 matrix
  a <- m[0, 0]
  
  mat <- matrix(c(1, 2,
                  3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
                dimnames = list(c("r1", "r2"), c("c1", "c2")))
  
  expect_equal(complete_rows_cols(a, mat), 
               matrix(c(0, 0, 
                        0, 0), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("r1", "r2"), c("c1", "c2"))))
})


test_that("complete_rows_cols() works with a Matrix with no dimnames", {
  # A is a Matrix with dimnames, mat is NULL.  
  # A should be completed relative to itself, but there is nothing to do.
  A <- Matrix::Matrix(42, nrow = 2, ncol = 1)
  res1 <- complete_rows_cols(A)
  expected1 <- Matrix::Matrix(c(42, 
                                42), byrow = TRUE, 
                              nrow = 2, ncol = 1)
  expect_equal(res1, expected1)
  
  # Try with 2 Matrix objects
  # Add one row (r3) and one column (c2)
  B <- Matrix::Matrix(0, nrow = 1, ncol = 1)
  res2 <- complete_rows_cols(A, B)
  expect_equal(res2, A)
})
