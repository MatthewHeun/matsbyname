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
# None of these functions are vectorized.
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


equal_matrix_or_Matrix <- function(a, b, tolerance = 1e-16) {
  # Matrix objects can have various actual classes.
  # Perform tests that are independent of 
  # the actual Matrix type.

  # Check numbers
  if (!all(abs(a - b) < tolerance)) {
    return(FALSE)
  }
  # Check row and column names
  if (!isTRUE(all.equal(dimnames(a), dimnames(b)))) {
    return(FALSE)
  }
  # Check row types
  if (xor(is.null(rowtype(a)), is.null(rowtype(b)))) {
    # If one is NULL but the other is non-NULL, 
    # they are not equal.
    return(FALSE)
  }
  if (!is.null(rowtype(a)) & !is.null(rowtype(b))) {
    # If both are non-NULL, check for equality
    if (!(rowtype(a) == rowtype(b))) {
      return(FALSE)
    }
  }
  # Check column types
  if (xor(is.null(coltype(a)), is.null(coltype(b)))) {
    # If one is NULL but the other is non-NULL, 
    # they are not equal.
    return(FALSE)
  }
  if (!is.null(coltype(a)) & !is.null(coltype(b))) {
    # If both are non-NULL, check for equality
    if (!(coltype(a) == coltype(b))) {
      return(FALSE)
    }
  }
  
  return(TRUE)
}


expect_equal_matrix_or_Matrix <- function(a, b, tolerance = 1e-16) {
  testthat::expect_true(equal_matrix_or_Matrix(a, b, tolerance))
}


solve_matrix_or_Matrix <- function(a_mat, 
                                   method = c("solve", "QR", "SVD"),
                                   tol = .Machine$double.eps) {
  tryCatch({
    if (method == "solve") {
      if (inherits(a_mat, "Matrix")) {
        rnames <- rownames(a_mat)
        cnames <- colnames(a_mat)
        # Matrix::solve() does not preserve dimnames.
        out <- Matrix::solve(a_mat, tol = tol)
        # Transpose row and column names on output.
        rownames(out) <- cnames
        colnames(out) <- rnames
      } else {
        # Probably a regular matrix object.
        out <- solve(a_mat, tol = tol)
      }
    } else if (method == "QR") {
      qr_res <- qr(a_mat)
      out <- solve.qr(qr_res, tol = tol)
      colnames(out) <- rownames(a_mat)
      if (inherits(a_mat, "Matrix")) {
        out <- Matrix::Matrix(out)
      }
    } else if (method == "SVD") {
      out <- matrixcalc::svd.inverse(a_mat)
      rownames(out) <- colnames(a_mat)
      colnames(out) <- rownames(a_mat)
    }
    return(out)
  }, error = function(e) {
    if (startsWith(e$message, "Lapack routine dgesv: system is exactly singular:") | 
        startsWith(e$message, "singular matrix 'a' in 'solve'")) {
      # Find any zero rows and columns
      zero_rows_cols <- getzerorowcolnames_byname(a_mat)
      # Create a helpful error message
      err_msg <- paste0("Attempt to invert a singular matrix. Zero rows and columns: ", paste0(zero_rows_cols, collapse = ", "), ".")
      stop(err_msg)
    }
    stop(e$message)
  })
}
  
  
  
  
  
  
  
  
  
  
  
