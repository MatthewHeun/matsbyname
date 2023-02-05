# This file contains helper functions 
# that enable objects of class matrix and class Matrix to be
# used in matsbyname functions.
# The need for these helper functions comes from the fact
# that not all base functions work with Matrix objects.
# For example, t(A) fails when A is a Matrix, but 
# Matrix::t(A) works.
# 
# These functions are all private functions, 
# because they are not intended for use
# outside of this package.
# 
# None of these functions is vectorized.
# 
# --- MKH, 5 Feb 2023

is_matrix_or_Matrix <- function(a) {
  is.matrix(a) | inherits(a, "Matrix")
}


t_matrix_or_Matrix <- function(a) {
  if (is.matrix(a)) {
    return(t(a))
  } else if (inherits(a, "Matrix")) {
    return(Matrix::t(a))
  }
}


cbind_matrix_or_Matrix <- function(a, b) {
  res <- cbind(a, b)
  if (inherits(a, "Matrix") | inherits(b, "Matrix")) {
    # Create a new Matrix object that 
    # takes on sparse characteristics, if possible.
    return(Matrix::Matrix(res))
  }
  return(res)
}


rbind_matrix_or_Matrix <- function(a, b) {
  res <- rbind(a, b)
  if (inherits(a, "Matrix") | inherits(b, "Matrix")) {
    # Create a new Matrix object that 
    # takes on sparse characteristics, if possible.
    return(Matrix::Matrix(res))
  }
  return(res)
}


equal_matrix_or_Matrix <- function(a, b) {
  # Matrix objects can have various types
  # Perform tests that are independent of 
  # the actual Matrix type.
  if (!all(a == b)) {
    return(FALSE)
  }
  if (!all.equal(dimnames(a), dimnames(b))) {
    return(FALSE)
  }
  return(TRUE)
}