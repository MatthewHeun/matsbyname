# Contains tests for the CompletingMatrices.R file in the byname package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expected.

library(dplyr)
library(byname)
library(parallel)
library(magrittr)
library(testthat)

###########################################################
context("complete_rows_cols")
###########################################################

test_that("complete_rows_cols works as expected", {
  m1 <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
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
  # Also works with data frames
  expect_equal(complete_rows_cols(data.frame(m1), data.frame(m2)), 
               data.frame(c1 = c(1,2,3), c2 = c(4,5,6), c3 = c(0,0,0), c4 = c(0,0,0)) %>% 
                 set_rownames(c("r1", "r2", "r3")))
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
  # Same as previous. With missing matrix, complete relative to transpose of x.
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
  expect_equal(complete_rows_cols(m1, names = list(c("r10", "r11"), c("c10", "c11"))), complete_m1_m2_new_names)
  # Also works with lists
  complete_rows_cols(x = list(m1,m1))
  expect_equal(complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2)), list(complete_m1_m2, complete_m1_m2))
  # No changes because r2, r3 already present in m1
  expect_equal(complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2), margin = 1), list(m1, m1))
  expect_equal(complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2), margin = 2), list(complete_m1_m2, complete_m1_m2))
  expect_equal(complete_rows_cols(x = list(m1,m1), names = make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2, lenx = 1)), 
               list(complete_m1_m2_new_names, complete_m1_m2_new_names))
  
  # Test that an error is given when matrices are missing row or column names or both.
  a <- matrix(1:4, nrow = 2)
  b <- matrix(1:4, nrow = 2)
  expect_error(complete_rows_cols(a), "NULL dimnames for margin = 1 on x")
  # expect_error(complete_rows_cols(a %>% set_rownames(c("r1", "r2")) %>% set_colnames(c("c1", "c2")), b), "NULL dimnames for margin = 1 on matrix")
})


###########################################################
context("complete_and_sort")
###########################################################

test_that("complete_and_sort works as expected", {
  m1 <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
  m2 <- matrix(c(7:12), ncol = 3, dimnames = list(c("r3", "r4"), c("c2", "c3", "c4")))
  m1_completed <- matrix(c(0,0,0,0,0,
                           0,0,0,0,0,
                           4,1,0,0,0,
                           5,2,0,0,0,
                           6,3,0,0,0), 
                         nrow = 5, ncol = 5, byrow = TRUE, 
                         dimnames = list(c("c1", "c2", "r1", "r2", "r3"), c("c1", "c2", "r1", "r2", "r3")))
  m2_completed <- matrix(c(0,0,0,0,0,
                           0,0,0,0,0,
                           0,0,0,0,0,
                           7,9,11,0,0,
                           8,10,12,0,0), 
                         nrow = 5, ncol = 5, byrow = TRUE,
                         dimnames = list(c("c2", "c3", "c4", "r3", "r4"), c("c2", "c3", "c4", "r3", "r4")))
  m1m2_completed <- list(m1 = matrix(c(4,1,0,0,
                                       5,2,0,0,
                                       6,3,0,0,
                                       0,0,0,0),
                                     nrow = 4, ncol = 4, byrow = TRUE,
                                     dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2", "c3", "c4"))), 
                         m2 = matrix(c(0,0,0,0,
                                       0,0,0,0,
                                       0,7,9,11,
                                       0,8,10,12),
                                     nrow = 4, ncol = 4, byrow = TRUE,
                                     dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2", "c3", "c4"))))
  
  # Complete relative to itself
  expect_equal(complete_and_sort(m1), m1_completed)
  expect_equal(complete_and_sort(m2), m2_completed)
  
  # Complete relative to each other
  expect_equal(complete_and_sort(m1, m2), m1m2_completed)
  
  # Specify row order. Unspecified rows are dropped
  expect_equal(complete_and_sort(m1, m2, roworder = c("r3", "r2", "r1")), 
               list(m1 = matrix(c(6,3,0,0,
                                  5,2,0,0,
                                  4,1,0,0),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list(c("r3", "r2", "r1"), c("c1", "c2", "c3", "c4"))), 
                    m2 = matrix(c(0,7,9,11,
                                  0,0,0,0,
                                  0,0,0,0),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list(c("r3", "r2", "r1"), c("c1", "c2", "c3", "c4")))))
  
  # Specify column order. Unspecified columns are dropped
  expect_equal(complete_and_sort(m1, m2, colorder = c("c4", "c3")), 
               list(m1 = matrix(c(0,0,
                                  0,0,
                                  0,0,
                                  0,0),
                                nrow = 4, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3", "r4"), c("c4", "c3"))), 
                    m2 = matrix(c(0,0,
                                  0,0,
                                  11,9,
                                  12,10),
                                nrow = 4, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3", "r4"), c("c4", "c3")))))
  
  # Complete and sort for rows only
  expect_equal(complete_and_sort(m1, m2, margin = 1), 
               list(m1 = matrix(c(1,4,
                                  2,5,
                                  3,6,
                                  0,0),
                                nrow = 4, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3", "r4"), c("c2", "c1"))), 
                    m2 = matrix(c(0,0,0,
                                  0,0,0,
                                  7,9,11,
                                  8,10,12),
                                nrow = 4, ncol = 3, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3", "r4"), c("c2", "c3", "c4")))))
  
  # Complete and sort for columns only
  expect_equal(complete_and_sort(m1, m2, margin = 2), 
               list(m1 = matrix(c(4,1,0,0,
                                  5,2,0,0,
                                  6,3,0,0),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))), 
                    m2 = matrix(c(0,7,9,11,
                                  0,8,10,12),
                                nrow = 2, ncol = 4, byrow = TRUE,
                                dimnames = list(c("r3", "r4"), c("c1", "c2", "c3", "c4")))))
  
  # transpose m2. Get lots of rows and columns.
  expect_equal(complete_and_sort(m1, t(m2)), 
               list(m1 = matrix(c(0,0,0,0,
                                  0,0,0,0,
                                  0,0,0,0,
                                  4,1,0,0,
                                  5,2,0,0,
                                  6,3,0,0),
                                nrow = 6, ncol = 4, byrow = TRUE,
                                dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c1", "c2", "r3", "r4"))), 
                    m2 = matrix(c(0,0,7,8,
                                  0,0,9,10,
                                  0,0,11,12,
                                  0,0,0,0,
                                  0,0,0,0,
                                  0,0,0,0),
                                nrow = 6, ncol = 4, byrow = TRUE,
                                dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c1", "c2", "r3", "r4")))))
  
  expect_equal(complete_and_sort(m1, t(m2), margin = 1), 
               list(m1 = matrix(c(0,0,
                                  0,0,
                                  0,0,
                                  1,4,
                                  2,5,
                                  3,6),
                                nrow = 6, ncol = 2, byrow = TRUE,
                                dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c2", "c1"))), 
                    m2 = matrix(c(7,8,
                                  9,10,
                                  11,12,
                                  0,0,
                                  0,0,
                                  0,0),
                                nrow = 6, ncol = 2, byrow = TRUE,
                                dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("r3", "r4")))))
  
  expect_equal(complete_and_sort(m1, t(m2), margin = 2), 
               list(m1 = matrix(c(4,1,0,0,
                                  5,2,0,0,
                                  6,3,0,0),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "r3", "r4"))), 
                    m2 = matrix(c(0,0,7,8,
                                  0,0,9,10,
                                  0,0,11,12),
                                nrow = 3, ncol = 4, byrow = TRUE,
                                dimnames = list(c("c2", "c3", "c4"), c("c1", "c2", "r3", "r4")))))
  
  v <- matrix(1:6, ncol = 2, dimnames = list(c("r3", "r1", "r2"), c("c2", "c1")))
  expect_equal(complete_and_sort(v, v), 
               list(m1 = matrix(c(5,2,
                                  6,3,
                                  4,1),
                                nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))), 
                    m2 = matrix(c(5,2,
                                  6,3,
                                  4,1),
                                nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))))
  
  # Also works with lists
  expect_equal(complete_and_sort(list(m1,m1), list(m2,m2)), 
               list(m1 = list(m1m2_completed$m1, m1m2_completed$m1), m2 = list(m1m2_completed$m2, m1m2_completed$m2)))
  
  # Should return unmodified matrices if row or column names are missing.
  m_bare <- matrix(c(1:4), nrow = 2)
  expect_error(complete_and_sort(m_bare, margin = 1), "NULL dimnames for margin = 1")
  expect_error(complete_and_sort(m_bare, margin = 2), "NULL dimnames for margin = 2")
  m_rownames <- matrix(c(1:4), nrow = 2, dimnames = list(c("r2", "r1"), NULL))
  expect_equal(complete_and_sort(m_rownames, m1, margin = 1), 
               list(m1 = matrix(c(2,4,
                                  1,3,
                                  0,0),
                                nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), NULL)),
                    m2 = matrix(c(1,4,
                                  2,5,
                                  3,6),
                                nrow = 3, ncol = 2, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))))
  m_colnames <- matrix(c(1:6), nrow = 2, dimnames = list(NULL, c("c3", "c2", "c1")))
  expect_equal(complete_and_sort(m_colnames, m1, margin = 2),
               list(m1 = matrix(c(5,3,1,
                                  6,4,2),
                                nrow = 2, ncol = 3, byrow = TRUE,
                                dimnames = list(NULL, c("c1", "c2", "c3"))),
                    m2 = matrix(c(4,1,0,
                                  5,2,0,
                                  6,3,0),
                                nrow = 3, ncol = 3, byrow = TRUE,
                                dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))))
  # Now try with matrices that have no dimnames
  a <- matrix(1:4, nrow = 2)
  b <- matrix(1:4, nrow = 2)
  expect_error(complete_and_sort(a, b), "NULL dimnames for margin = 1 on x")
})


###########################################################
context("utilities")
###########################################################

test_that("make_list works as expected", {
  m <- matrix(c(1:6), nrow = 3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))

  expect_equal(make_list(m, n = 1), list(m))
  expect_equal(make_list(m, n = 2), list(m, m))
  expect_equal(make_list(m, n = 5), list(m, m, m, m, m))
  l1 <- list(c(1,2), c(3,4))
  # Expect c(1,2), c(3,4), c(1,2), c(3,4)
  expect_equal(make_list(l1, n = 4), c(l1, l1))
  # Expect [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)], [c(1,2), c(3,4)]
  expect_equal(make_list(l1, n = 4, lenx = 1), list(l1, l1, l1, l1))
  # Expect a warning, because length isn't a multiple
  expect_warning(make_list(l1, n = 3), "n not evenly divisible by length\\(x\\)")
  
  m1 <- matrix(1:4, nrow = 2)
  m2 <- m + 100
  l2 <- list(m1, m2)
  expect_equal(make_list(l2, n = 4), c(l2, l2))
  expect_warning(make_list(l2, n = 1), "n not evenly divisible by length\\(x\\)")
  expect_warning(make_list(l2, n = 5), "n not evenly divisible by length\\(x\\)")
  
  l3 <- list(c("r10", "r11"), c("c10", "c11"))
  expect_equal(make_list(l3, n = 2), l3) # Confused by x being a list
  expect_equal(make_list(l3, n = 2, lenx = 1), list(l3, l3)) # Fix by setting lenx = 1
})
  
