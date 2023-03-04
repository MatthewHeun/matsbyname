
test_that("sort_rows_cols() works as expected", {
  m <- matrix(c(1:6), nrow = 3, dimnames = list(c("r3", "r5", "r1"), c("c4", "c2")))
  msorted <- matrix(c(6, 3,
                      4, 1,
                      5, 2), byrow = TRUE, nrow = 3, 
                    dimnames = list(c("r1", "r3", "r5"), c("c2", "c4")))
  expect_equal(sort_rows_cols(m), msorted)
  expect_equal(sort_rows_cols(t(m)), 
               matrix(c(6, 4, 5,
                        3, 1, 2), byrow = TRUE, nrow = 2, 
                      dimnames = list(c("c2", "c4"), c("r1", "r3", "r5"))))
  # Sort rows only
  expect_equal(sort_rows_cols(m, margin = 1), 
               matrix(c(3, 6,
                        1, 4,
                        2, 5), byrow = TRUE, nrow = 3,
                      dimnames = list(c("r1", "r3", "r5"), c("c4", "c2"))))
  # Sort columns only
  expect_equal(sort_rows_cols(m, margin = 2), 
               matrix(c(4, 1,
                        5, 2,
                        6, 3), byrow = TRUE, nrow = 3,
                      dimnames = list(c("r3", "r5", "r1"), c("c2", "c4"))))
  # Try with a column vector
  v <- matrix(c(1:5), ncol = 1, dimnames = list(rev(paste0("r", 1:5)), "c1"))
  sortedv <- matrix(c(5, 
                      4, 
                      3, 
                      2, 
                      1), byrow = TRUE, nrow = 5, 
                    dimnames = list(c("r1", "r2", "r3", "r4", "r5"), "c1"))
  expect_equal(sort_rows_cols(v), sortedv)
  expect_equal(sort_rows_cols(v, margin = 1), sortedv)
  # No effect: only one column
  expect_equal(sort_rows_cols(v, margin = 2), v) 
  # Now try with a row vector
  r <- matrix(c(1:4), nrow = 1, dimnames = list("r1", rev(paste0("c", 1:4)))) 
  sortedr <- matrix(c(4:1), byrow = TRUE, nrow = 1, dimnames = list(c("r1"), c("c1", "c2", "c3", "c4")))
  # Sorts columns
  expect_equal(sort_rows_cols(r), sortedr)
  # No row name
  nrn <- matrix(c(1,2), nrow = 1, dimnames = list(NULL, c("c2", "c1"))) 
  # Sorts columns, because only one row.
  expect_equal(sort_rows_cols(nrn), matrix(2:1, nrow = 1, dimnames = list(NULL, c("c1", "c2"))))
  # No column name
  ncn <- matrix(c(1,2), ncol = 1, dimnames = list(c("r2", "r1"), NULL)) 
  # Sorts rows, because only one column.
  expect_equal(sort_rows_cols(ncn), matrix(c(2, 1), ncol = 1, dimnames = list(c("r1", "r2"), NULL)))
  # Also works with lists
  # Sorts rows and columns for both m's.
  sortedm <- matrix(c(6, 3,
                      4, 1,
                      5, 2), byrow = TRUE, nrow = 3, dimnames = list(c("r1", "r3", "r5"), c("c2", "c4")))
  expect_equal(sort_rows_cols(list(m,m), margin = list(c(1, 2))), list(sortedm, sortedm))
  # Sort only row with a special order.
  sorted1 <- matrix(c(2, 5, 
                      1, 4,
                      3, 6), byrow = TRUE, nrow = 3, dimnames = list(c("r5", "r3", "r1"), c("c4", "c2")))
  expect_equal(sort_rows_cols(a = list(m,m), margin = 1, roworder = list(c("r5", "r3", "r1"))), 
               list(sorted1, sorted1))
  # Columns are sorted as default, because no colorder is given.
  sorted2 <- matrix(c(4, 1, 
                      5, 2,
                      6, 3), byrow = TRUE, nrow = 3, dimnames = list(c("r3", "r5", "r1"), c("c2", "c4")))
  expect_equal(sort_rows_cols(a = list(m,m), margin = 2, roworder = list(c("r5", "r3", "r1"))), 
               list(sorted2, sorted2))
  # Both columns and rows sorted, rows by the list, columns in natural order.
  sorted3 <- matrix(c(5, 2,
                      4, 1,
                      6, 3), byrow = TRUE, nrow = 3, dimnames = list(c("r5", "r3", "r1"), c("c2", "c4")))
  expect_equal(sort_rows_cols(a = list(m,m), margin = list(c(1,2)), roworder = list(c("r5", "r3", "r1"))), 
               list(sorted3, sorted3))
  
  # Ensure that rowtypes and coltypes, if present, are maintained
  mtypes <- m %>% setrowtype("row") %>% setcoltype("col")
  expect_equal(sort_rows_cols(mtypes), msorted %>% setrowtype("row") %>% setcoltype("col"))
})


test_that("sort_rows_cols() works with different-length arguments for lists", {
  m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r2", "r1"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  m_sorted <- matrix(c(2, 1, 4, 3), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  mlist <- list(m, m)
  expect_equal(sort_rows_cols(mlist, margin = list(c(1,2)), roworder = list(c("r1", "r2")), colorder = NA), 
               list(m_sorted, m_sorted))
  # Now try in a matrix with a list of lists
  DF <- data.frame(m = I(list()), margin = I(list()))
  DF[[1, "m"]] <- mlist
  DF[[2, "m"]] <- mlist
  DF[[3, "m"]] <- mlist
  marginlist <- list(c(1, 2), c(1, 2))
  DF[[1, "margin"]] <- marginlist
  DF[[2, "margin"]] <- marginlist
  DF[[3, "margin"]] <- marginlist
  res <- DF %>% 
    dplyr::mutate(
      # sorted = sort_rows_cols(m, margin = list(c(1, 2)))
      sorted = sort_rows_cols(m, margin = margin)
    )
  expect_equal(res$sorted[[1]][[1]], m_sorted)
  expect_equal(res$sorted[[1]][[2]], m_sorted)
  expect_equal(res$sorted[[2]][[1]], m_sorted)
  expect_equal(res$sorted[[1]][[2]], m_sorted)
  expect_equal(res$sorted[[3]][[1]], m_sorted)
  expect_equal(res$sorted[[1]][[2]], m_sorted)
})


test_that("sort_rows_cols() works when specifying a row or col name that isn't present in a", {
  m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r2", "r1"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  sorted_m <- matrix(c(2, 4, 1, 3), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(sort_rows_cols(m), sorted_m)
  # It should not matter that "r3" doesn't exist in m.
  expect_equal(sort_rows_cols(m, roworder = c("r1", "r2", "r3")), sorted_m)
  # It should not matter that "c3" doesn't exist in m.
  expect_equal(sort_rows_cols(m, colorder = c("c1", "c2", "c3")), sorted_m)
  expect_equal(sort_rows_cols(m, colorder = c("c100", "c42", "c2", "c1", "cmillion")), 
               matrix(c(4, 2, 3, 1), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c2", "c1"))) %>% 
                 setrowtype("row") %>% setcoltype("col"))
  expect_equal(sort_rows_cols(m, colorder = c("c2")), 
               matrix(c(4, 3), nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "c2")) %>% 
                 setrowtype("row") %>% setcoltype("col"))
})


test_that("sort_rows_cols() fails when row and col names are not unique", {
  mrowprob <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r1", "r1"), c("c1", "c1"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_error(sort_rows_cols(mrowprob), "Row names not unique")
  mcolprob <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c1"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_error(sort_rows_cols(mcolprob), "Column names not unique")
})


test_that("sort_rows_cols() duplicates a row when requested", {
  m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r2", "r1"), c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  expect_equal(sort_rows_cols(m, roworder = c("r1", "r1")), 
               matrix(c(2, 4,
                        2, 4), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r1"), c("c1", "c2"))) %>% 
                 setrowtype("row") %>% setcoltype("col"))
})  


test_that("sort_rows_cols() works when sorting rows but rows are not named", {
  m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(NULL, c("c1", "c2"))) %>% 
    setrowtype("row") %>% setcoltype("col")
  # In this situation, nothing is done.  Expect that m will be returned.
  expect_equal(sort_rows_cols(m, margin = 1), m)
})


test_that("sort_rows_cols() works for a Matrix", {
  # Try with sparse Matrix objects
  A <- matsbyname::Matrix(c(0, 1, 0,
                            0, 0, 0), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r2", "r1"), c("c1", "c2", "c3")))
  
  res1 <- sort_rows_cols(A)
  expected1 <- matsbyname::Matrix(c(0, 0, 0,
                                    0, 1, 0), byrow = TRUE, nrow = 2, ncol = 3, dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(res1, expected1)
  
  # Try to dense Matrix objects
  B <- matsbyname::Matrix(c(1, 1, 1, 
                            2, 1, 1), byrow = TRUE, nrow = 2, ncol = 3, 
                          dimnames = list(c("r1", "r2"), c("c2", "c1", "c3")))
  res2 <- sort_rows_cols(B)
  expected2 <- matsbyname::Matrix(c(1, 1, 1, 
                                    1, 2, 1), byrow = TRUE, nrow = 2, ncol = 3, 
                                  dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  expect_equal(res2, expected2)
})
