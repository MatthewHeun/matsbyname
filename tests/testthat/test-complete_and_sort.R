# Contains tests for the CompletingMatrices.R file in the byname package.




test_that("complete_and_sort() works as expected", {
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
  m1m2_completed <- list(a = matrix(c(4,1,0,0,
                                      5,2,0,0,
                                      6,3,0,0,
                                      0,0,0,0),
                                    nrow = 4, ncol = 4, byrow = TRUE,
                                    dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2", "c3", "c4"))), 
                         b = matrix(c(0,0,0,0,
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
               list(a = matrix(c(6,3,0,0,
                                 5,2,0,0,
                                 4,1,0,0),
                               nrow = 3, ncol = 4, byrow = TRUE,
                               dimnames = list(c("r3", "r2", "r1"), c("c1", "c2", "c3", "c4"))), 
                    b = matrix(c(0,7,9,11,
                                 0,0,0,0,
                                 0,0,0,0),
                               nrow = 3, ncol = 4, byrow = TRUE,
                               dimnames = list(c("r3", "r2", "r1"), c("c1", "c2", "c3", "c4")))))
  
  # Specify column order. Unspecified columns are dropped
  expect_equal(complete_and_sort(m1, m2, colorder = c("c4", "c3")), 
               list(a = matrix(c(0,0,
                                 0,0,
                                 0,0,
                                 0,0),
                               nrow = 4, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3", "r4"), c("c4", "c3"))), 
                    b = matrix(c(0,0,
                                 0,0,
                                 11,9,
                                 12,10),
                               nrow = 4, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3", "r4"), c("c4", "c3")))))
  
  # Complete and sort for rows only
  expect_equal(complete_and_sort(m1, m2, margin = 1), 
               list(a = matrix(c(1,4,
                                 2,5,
                                 3,6,
                                 0,0),
                               nrow = 4, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3", "r4"), c("c2", "c1"))), 
                    b = matrix(c(0,0,0,
                                 0,0,0,
                                 7,9,11,
                                 8,10,12),
                               nrow = 4, ncol = 3, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3", "r4"), c("c2", "c3", "c4")))))
  
  # Complete and sort for columns only
  expect_equal(complete_and_sort(m1, m2, margin = 2), 
               list(a = matrix(c(4,1,0,0,
                                 5,2,0,0,
                                 6,3,0,0),
                               nrow = 3, ncol = 4, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3", "c4"))), 
                    b = matrix(c(0,7,9,11,
                                 0,8,10,12),
                               nrow = 2, ncol = 4, byrow = TRUE,
                               dimnames = list(c("r3", "r4"), c("c1", "c2", "c3", "c4")))))
  
  # transpose m2. Get lots of rows and columns.
  expect_equal(complete_and_sort(m1, t(m2)), 
               list(a = matrix(c(0,0,0,0,
                                 0,0,0,0,
                                 0,0,0,0,
                                 4,1,0,0,
                                 5,2,0,0,
                                 6,3,0,0),
                               nrow = 6, ncol = 4, byrow = TRUE,
                               dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c1", "c2", "r3", "r4"))), 
                    b = matrix(c(0,0,7,8,
                                 0,0,9,10,
                                 0,0,11,12,
                                 0,0,0,0,
                                 0,0,0,0,
                                 0,0,0,0),
                               nrow = 6, ncol = 4, byrow = TRUE,
                               dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c1", "c2", "r3", "r4")))))
  
  expect_equal(complete_and_sort(m1, t(m2), margin = 1), 
               list(a = matrix(c(0,0,
                                 0,0,
                                 0,0,
                                 1,4,
                                 2,5,
                                 3,6),
                               nrow = 6, ncol = 2, byrow = TRUE,
                               dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("c2", "c1"))), 
                    b = matrix(c(7,8,
                                 9,10,
                                 11,12,
                                 0,0,
                                 0,0,
                                 0,0),
                               nrow = 6, ncol = 2, byrow = TRUE,
                               dimnames = list(c("c2", "c3", "c4", "r1", "r2", "r3"), c("r3", "r4")))))
  
  expect_equal(complete_and_sort(m1, t(m2), margin = 2), 
               list(a = matrix(c(4,1,0,0,
                                 5,2,0,0,
                                 6,3,0,0),
                               nrow = 3, ncol = 4, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "r3", "r4"))), 
                    b = matrix(c(0,0,7,8,
                                 0,0,9,10,
                                 0,0,11,12),
                               nrow = 3, ncol = 4, byrow = TRUE,
                               dimnames = list(c("c2", "c3", "c4"), c("c1", "c2", "r3", "r4")))))
  
  v <- matrix(1:6, ncol = 2, dimnames = list(c("r3", "r1", "r2"), c("c2", "c1")))
  expect_equal(complete_and_sort(v, v), 
               list(a = matrix(c(5,2,
                                 6,3,
                                 4,1),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2"))), 
                    b = matrix(c(5,2,
                                 6,3,
                                 4,1),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))))
  
  # Also works with lists
  expect_equal(complete_and_sort(list(m1,m1), list(m2,m2), margin = list(c(1, 2))), 
               list(a = list(m1m2_completed$a, m1m2_completed$a), b = list(m1m2_completed$b, m1m2_completed$b)))
  
  # Should return unmodified matrices if row or column names are missing.
  m_bare <- matrix(c(1:4), nrow = 2)
  # Given no dimnames on m_bare, it is returned unmodified.
  expect_equal(complete_and_sort(m_bare, margin = 1), m_bare)
  expect_equal(complete_and_sort(m_bare, margin = 2), m_bare)
  m_rownames <- matrix(c(1:4), nrow = 2, dimnames = list(c("r2", "r1"), NULL))
  expect_equal(complete_and_sort(m_rownames, m1, margin = 1), 
               list(a = matrix(c(2,4,
                                 1,3,
                                 0,0),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), NULL)),
                    b = matrix(c(1,4,
                                 2,5,
                                 3,6),
                               nrow = 3, ncol = 2, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))))
  m_colnames <- matrix(c(1:6), nrow = 2, dimnames = list(NULL, c("c3", "c2", "c1")))
  expect_equal(complete_and_sort(m_colnames, m1, margin = 2),
               list(a = matrix(c(5,3,1,
                                 6,4,2),
                               nrow = 2, ncol = 3, byrow = TRUE,
                               dimnames = list(NULL, c("c1", "c2", "c3"))),
                    b = matrix(c(4,1,0,
                                 5,2,0,
                                 6,3,0),
                               nrow = 3, ncol = 3, byrow = TRUE,
                               dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))))
  # Now try with matrices that have no dimnames
  a <- matrix(1:4, nrow = 2)
  b <- matrix(1:4, nrow = 2)
  expect_equal(complete_and_sort(a, b), list(a = a, b = b))
})


test_that("complete_and_sort() preserves row and column types", {
  # a and b are same matrices.
  # Completing and sorting against each other should produce a (or b).
  a <- matrix(c(1:4), nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  b <- matrix(c(1:4), nrow = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  expect_equal(complete_rows_cols(a, b), a)
  expect_equal(complete_rows_cols(b, a), b)
  expect_equal(complete_and_sort(a, b), list(a = a, b = a))
  expect_equal(complete_and_sort(a, b), list(a = b, b = a))
})


test_that("complete_and_sort() works with Matrix objects", {
  A <- matsbyname::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  B <- matsbyname::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r3", "r4"), c("c3", "c4")))
  res1 <- complete_and_sort(A, B)
  expectedA <- matsbyname::Matrix(c(1, 1, 0, 0,
                                    1, 1, 0, 0, 
                                    0, 0, 0, 0,
                                    0, 0, 0, 0),
                                  nrow = 4, ncol = 4, byrow = TRUE, 
                                  dimnames = list(c("r1", "r2", "r3", "r4"), 
                                                  c("c1", "c2", "c3", "c4")))
  expectedB <- matsbyname::Matrix(c(0, 0, 0, 0,
                                    0, 0, 0, 0, 
                                    0, 0, 1, 1,
                                    0, 0, 1, 1),
                                  nrow = 4, ncol = 4, byrow = TRUE, 
                                  dimnames = list(c("r1", "r2", "r3", "r4"), 
                                                  c("c1", "c2", "c3", "c4")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$a, expectedA)
  matsbyname:::expect_equal_matrix_or_Matrix(res1$b, expectedB)
})


test_that("complete_and_sort() works with unnamed Matrix objects", {
  A <- Matrix::Matrix(1, nrow = 2, ncol = 2)
  B <- Matrix::Matrix(1, nrow = 2, ncol = 2)
  res1 <- complete_and_sort(A, B)
  expect_equal(res1$a, A)
  expect_equal(res1$b, B)
})

