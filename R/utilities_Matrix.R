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


#' Create a Matrix amenable to use in the `matsbyname` package
#' 
#' The `matsbyname` package uses `Matrix` objects for its
#' default data representation, taking advantage 
#' of the sparse matrix capabilities of `Matrix` compared
#' to the base `matrix` class. 
#' This function routes to `Matrix::Matrix()`, with some important
#' differences. See details. 
#' 
#' This function NEVER creates a symmetric matrix, 
#' because symmetric matrices do not respect some future changes to `dimnames`,
#' which can cause information loss in the `matsbyname` context. 
#' A non-symmetric `Matrix` is assured by calling `as(out, "generalMatrix")`
#' on the outgoing `Matrix` object.
#' 
#' This function has different defaults compared to `Matrix::Matrix()`, including
#' 
#' <ul>
#' <li> Here, the default for `doDiag` is `FALSE`,
#'      while the default for `doDiag` is `TRUE` for `Matrix::Matrix()`.
#' <li> When `dimnames = NULL` (the default), `dimnames = NULL` is the result, 
#'      to maintain compatibility with `matrix()`.
#'      `Matrix::Matrix()` sets `dimnames = list(NULL, NULL)`.
#' </ul>
#'
#' @param data An optional numeric data vector or matrix.
#' @param nrow When data is not a matrix, the desired number of rows. 
#'             Default is `1`.
#' @param ncol When data is not a matrix, the desired number of columns.
#'             Default is `1`.
#' @param byrow A boolean. If `FALSE` (the default) the matrix is filled by columns, otherwise the matrix is filled by rows.
#' @param dimnames A dimnames attribute for the matrix: a list of two character components. They are set if not NULL (as per default).
#' @param sparse A boolean or `NULL`. Specifies whether the result should be sparse or not. 
#'               By default (`NULL`), the Matrix is made sparse when more than half 
#'               of the entries are `0`.
#' @param doDiag A boolean indicating if a `diagonalMatrix` object should be returned
#'               when the resulting matrix is diagonal (mathematically). 
#'               Default is `FALSE`, which is different from `Matrix::Matrix()`.
#' @param forceCheck A boolean indicating if the checks for structure should happen
#'                   when `data` is already a `Matrix` object.
#'                   Default is `FALSE`.
#'
#' @return A `Matrix` object.
#' 
#' @export
#'
#' @examples
#' # matsbyname::Matrix() will not create a Matrix with a symmetric subclass ...
#' matsbyname::Matrix(c(1, 0, 2, 
#'                      0, 0, 0, 
#'                      2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#' # ... but Matrix::Matrix will create a symmetric matrix.
#' Matrix::Matrix(c(1, 0, 2, 
#'                  0, 0, 0, 
#'                  2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#' # matsbyname::Matrix() will not create a diagonal matrix ...
#' matsbyname::Matrix(c(1, 0, 
#'                      0, 1), byrow = TRUE, nrow = 2, ncol = 2)
#' # ... but Matrix::Matrix will create a diagonal matrix.
#' Matrix::Matrix(c(1, 0, 
#'                  0, 1), byrow = TRUE, nrow = 2, ncol = 2)
Matrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL,
                   sparse = NULL, doDiag = FALSE, forceCheck = FALSE) {
  if (inherits(data, "Matrix") | inherits(data, "matrix")) {
    out <- Matrix::Matrix(data = data, 
                          dimnames = dimnames, sparse = sparse, doDiag = doDiag, forceCheck = forceCheck)
  } else {
    out <- Matrix::Matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow,
                          dimnames = dimnames, sparse = sparse, doDiag = doDiag, forceCheck = forceCheck)
  }
    # Ensure no symmetric matrices are created.
    as(out, "generalMatrix")
}


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


invert_matrix_or_Matrix <- function(a_mat, 
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
      if (inherits(a_mat, "Matrix")) {
        # Use the Matrix function.
        qr_res <- Matrix::qr(a_mat)
      } else {
        qr_res <- qr(a_mat)
      }
      # Does not preserve column names
      out <- solve.qr(qr_res, tol = tol)
      colnames(out) <- rownames(a_mat)
      if (inherits(a_mat, "Matrix")) {
        # If we came in with a Matrix, 
        # send a Matrix back.
        out <- Matrix::Matrix(out)
      }
    } else if (method == "SVD") {
      convert_to_Matrix <- FALSE
      if (inherits(a_mat, "Matrix")) {
        # There is no Matrix::svd.inverse function.
        # So convert to a matrix.
        a_mat <- as.matrix(a_mat)
        convert_to_Matrix <- TRUE
      }
      out <- matrixcalc::svd.inverse(a_mat)
      rownames(out) <- colnames(a_mat)
      colnames(out) <- rownames(a_mat)
      # If we came in as a Matrix, go out as a Matrix
      if (convert_to_Matrix) {
        out <- Matrix::Matrix(out)
      }
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
  
  
  
  
  
  
  
  
  
  
  
