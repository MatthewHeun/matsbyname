test_that("trim_rowscols() works with NULL a", {
  mat <- matrix(1, nrow = 1, ncol = 1)
  res <- trim_rows_cols(NULL, mat)
  expect_null(res)
  
  
})


test_that("trim_rowscols() works as expected", {
  a <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  mat <- matrix(c(1, 2, 3,
                  4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
    setrowtype("rowtype") %>% setcoltype("coltype")
  
  res <- trim_rows_cols(a, mat, margin = 1)
  expect_equal(res, matrix(c(1, 2, 3), nrow = 1, ncol = 3, byrow = TRUE, 
                           dimnames = list(c("r1"), c("c1", "c2", "c3"))) %>% 
                 setrowtype("rowtype") %>% setcoltype("coltype"))
})