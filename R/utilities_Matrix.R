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
#' This function enables setting row and column types at the time of construction
#' with the `rowtype` and `coltype` arguments.
#' 
#' This function has different defaults compared to `Matrix::Matrix()`, including
#' 
#' * Here, the default for `doDiag` is `FALSE`,
#'   while the default for `doDiag` is `TRUE` for `Matrix::Matrix()`.
#' * Preserves rowtype and coltype on `data`.
#'
#' @param data An optional numeric data vector or matrix.
#' @param nrow When `data` is not a `matrix` or a `Matrix`, the desired number of rows. 
#'             Default is `1`.
#' @param ncol When `data` is not a `matrix` or a `Matrix`, the desired number of columns.
#'             Default is `1`.
#' @param byrow A boolean. If `FALSE` (the default) the Matrix is filled by columns, otherwise the Matrix is filled by rows.
#' @param dimnames A dimnames attribute for the Matrix: a list of two character components. 
#'                 Default is `base::dimnames(data)`.
#' @param sparse A boolean or `NULL`. Specifies whether the result should be sparse or not. 
#'               By default (`NULL`), the Matrix is made sparse when more than half 
#'               of the entries are `0`.
#' @param doDiag A boolean indicating if a `diagonalMatrix` object should be returned
#'               when the resulting Matrix is diagonal (mathematically). 
#'               Default is `FALSE`, which is different from `Matrix::Matrix()`.
#' @param forceCheck A boolean indicating if the checks for structure should happen
#'                   when `data` is already a `Matrix` object.
#'                   Default is `FALSE`.
#' @param rowtype The rowtype for the result. Default is `matsbyname::rowtype(data)`.
#' @param coltype The coltype for the result. Default is `matsbyname::coltype(data)`.
#'
#' @return A `Matrix` object.
#' 
#' @export
#'
#' @examples
#' # matsbyname::Matrix() will not create a Matrix with a symmetric subclass.
#' # dgCMatrix is a general matrix.
#' matsbyname::Matrix(c(1, 0, 2, 
#'                      0, 0, 0, 
#'                      2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#' # But Matrix::Matrix() will create a symmetric matrix.
#' # dsCMatrix is a symmetric matrix.
#' Matrix::Matrix(c(1, 0, 2, 
#'                  0, 0, 0, 
#'                  2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#' # matsbyname::Matrix() will not create a diagonal matrix.
#' # dgeMatrix is a general matrix.
#' matsbyname::Matrix(c(1, 0, 
#'                      0, 1), byrow = TRUE, nrow = 2, ncol = 2)
#' # But Matrix::Matrix() will create a diagonal matrix.
#' # ddiMatrix is a diagonal matrix.
#' Matrix::Matrix(c(1, 0, 
#'                  0, 1), byrow = TRUE, nrow = 2, ncol = 2)
Matrix <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = base::dimnames(data),
                   sparse = NULL, doDiag = FALSE, forceCheck = FALSE, 
                   rowtype = matsbyname::rowtype(data), coltype = matsbyname::coltype(data)) {
  if (is_matrix_or_Matrix(data)) {
    # Specifying nrow, ncol, or byrow results in a warning.
    out <- Matrix::Matrix(data = data, 
                          sparse = sparse, doDiag = doDiag, forceCheck = forceCheck)
  } else {
    out <- Matrix::Matrix(data = data, 
                          nrow = nrow, ncol = ncol, byrow = byrow,
                          sparse = sparse, doDiag = doDiag, 
                          forceCheck = forceCheck)
  }
  
  # The call to Matrix::Matrix() can result in a symmetricMatrix,
  # in which case, dimnames are not correctly preserved.
  # Coerce to a generalMatrix, which can 
  # correctly store dimnames.
  out <- out %>% 
    methods::as("generalMatrix")
  # Now set the dimnames based on the dimnames argument (if not NULL)
  if (!(identical(dimnames, list(NULL, NULL)) | is.null(dimnames))) {
    dimnames(out) <- dimnames
  } 
  # Finally, set row and column types based on the arguments.
  out %>% 
    setrowtype(rowtype) %>% setcoltype(coltype)
}


#' Is an object a Matrix?
#' 
#' Arguably, this function should be in the `Matrix` package, 
#' but it is not. 
#' We include it here for convenience.
#' 
#' This function is not vectorized.
#' 
#' `is.Matrix()` is a wrapper for `inherits(a, "Matrix)`.
#' 
#' @param a The object to be queried if it is Matrix.
#'
#' @return A boolean. `TRUE` if `a` is a `Matrix`, `FALSE` otherwise.
#' 
#' @export
#'
#' @examples
#' is.Matrix(matrix(42))
#' is.Matrix(Matrix::Matrix(42))
is.Matrix <- function(a) {
  inherits(a, "Matrix")
}


#' Tells whether an object is one of a matrix or a Matrix
#' 
#' Often, it helps to know whether an object is a `matrix` or a `Matrix`, 
#' and you don't care which. 
#' This function helps in those situations.
#'
#' @param a The object about which we want to know if it is a `matrix` or a `Matrix`.
#'
#' @return `TRUE` when `a` is a `matrix` or a `Matrix`. `FALSE` otherwise.
#' 
#' @export
#'
#' @examples
#' is_matrix_or_Matrix(42)
#' is_matrix_or_Matrix(matrix(42))
#' is_matrix_or_Matrix(Matrix::Matrix(42))
#' is_matrix_or_Matrix(matsbyname::Matrix(42))
is_matrix_or_Matrix <- function(a) {
  is.matrix(a) | is.Matrix(a)
}


t_matrix_or_Matrix <- function(a) {
  if (is.matrix(a)) {
    out <- t(a)
  } else if (is.Matrix(a)) {
    out <- Matrix::t(a)
  }
  # Also need to swap the rowtype and coltype on a.
  out %>% 
    setrowtype(coltype(a)) %>% setcoltype(rowtype(a))
}


rbind_matrix_or_Matrix <- function(a, b) {
  # Originally, it seemed that we needed to distinguish between matrix and Matrix.
  # But rbind seems to work for both.
  checked <- check_row_col_types(a, b)
  rbind(checked[[1]], checked[[2]]) %>% 
    # At this point, row and column types are guaranteed to be same, due to check_row_col_types()
    setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
}


cbind_matrix_or_Matrix <- function(a, b) {
  # Originally, it seemed that we needed to distinguish between matrix and Matrix.
  # But cbind seems to work for both.
  checked <- check_row_col_types(a, b)
  cbind(checked[[1]], checked[[2]]) %>% 
    # At this point, row and column types are guaranteed to be same, due to check_row_col_types()
    setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
}


check_row_col_types <- function(a, b) {
  rta <- rowtype(a)
  cta <- coltype(a)
  rtb <- rowtype(b)
  ctb <- coltype(b)
  
  if (xor(is.null(rta), is.null(rtb))) {
    # One of rta and rtb is NULL. Set to the non-NULL one.
    if (is.null(rta)) {
      a <- setrowtype(a, rtb)
    } else {
      b <- setrowtype(b, rta)
    }
  }
  if (xor(is.null(cta), is.null(ctb))) {
    # One of cta and ctb is NULL
    if (is.null(cta)) {
      a <- setcoltype(a, ctb)
    } else {
      b <- setcoltype(b, cta)
    }
  }
  
  # At this point, rowtype could be NULL on both a and b.
  # In that case, we're fine.
  # In the event that both are not NULL, we
  # need to make sure rowtypes are the same. 
  if (!is.null(rta) & !is.null(rtb)) {
    if (rta != rtb) {
      stop(paste("Incompatible row types:", rta, "and", rtb))
    }
  }
  # Same for coltypes
  if (!is.null(cta) & !is.null(ctb)) {
    if (cta != ctb) {
      stop(paste("Incompatible column types:", cta, "and", ctb))
    }
  }
  return(list(a, b))
}


rowSums_matrix_or_Matrix <- function(a) {
  if (is.Matrix(a)) {
    # Use the Matrix function.
    out <- Matrix::rowSums(a) %>% 
      matsbyname::Matrix(nrow = nrow(a), ncol = 1, dimnames = list(rownames(a), coltype(a)))
  } else {
    out <- rowSums(a) %>% 
      matrix(nrow = nrow(a), ncol = 1, dimnames = list(rownames(a), coltype(a)))
  }
  out %>% 
    setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
}


colSums_matrix_or_Matrix <- function(a) {
  a %>% 
    t_matrix_or_Matrix() %>% 
    rowSums_matrix_or_Matrix() %>% 
    t_matrix_or_Matrix()
}


equal_matrix_or_Matrix <- function(a, b, tolerance = 1e-16) {
  # Matrix objects can have various actual classes.
  # Perform tests that are independent of 
  # the actual Matrix type.

  # Check numbers
  if (!(all(abs(a - b) <= tolerance))) {
    return(FALSE)
  }
  # Check row and column names
  if (!isTRUE(all.equal(dimnames(a), dimnames(b)))) {
    # If no dimnames have been set, 
    # a Matrix object always has list(NULL, NULL) for dimnames.
    # If no dimnames have been set, 
    # a matrix object will have NULL for dimnames.
    # We eon't want to return FALSE in that situation.
    dnaNULL <- all(sapply(dimnames(a), is.null))
    dnbNULL <- all(sapply(dimnames(b), is.null))
    if (!(dnaNULL & dnbNULL)) {
      return(FALSE)
    }
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
      if (is.Matrix(a_mat)) {
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
      if (is.Matrix(a_mat)) {
        # Use the Matrix function.
        qr_res <- Matrix::qr(a_mat)
      } else {
        qr_res <- qr(a_mat)
      }
      # Does not preserve column names
      out <- solve.qr(qr_res, tol = tol)
      colnames(out) <- rownames(a_mat)
      if (is.Matrix(a_mat)) {
        # If we came in with a Matrix, 
        # send a Matrix back.
        out <- Matrix::Matrix(out)
      }
    } else if (method == "SVD") {
      convert_to_Matrix <- FALSE
      if (is.Matrix(a_mat)) {
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
  


  
  
  
  
  
  
  
  
  
  
