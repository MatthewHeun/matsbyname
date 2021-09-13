test_that("trim_rowscols() works with NULL a", {
  mat <- matrix(1, nrow = 1, ncol = 1)
  res <- trim_rows_cols(NULL, mat)
  expect_null(res)
  
  
})
