
test_that("is.matrix_or_Matrix() works correctly", {
  expect_false(matsbyname:::is.matrix_or_Matrix(42))
  expect_false(matsbyname:::is.matrix_or_Matrix("42"))
  
  expect_true(matsbyname:::is.matrix_or_Matrix(matrix(1)))
  expect_true(matsbyname:::is.matrix_or_Matrix(Matrix::Matrix(1)))
  
  expect_equal(matsbyname:::is.matrix_or_Matrix(list(matrix(1), matrix(2))), c(TRUE, TRUE))

  expect_equal(matsbyname:::is.matrix_or_Matrix(list(matrix(1), Matrix::Matrix(2), "42", 42)), c(TRUE, TRUE, FALSE, FALSE))
})
