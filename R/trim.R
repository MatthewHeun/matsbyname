#' Trim rows and/or columns from a matrix
#' 
#' By default, the `matsbyname` package expends matrices
#' with `0` rows or columns prior to matrix operations
#' to ensure that rows and columns match.
#' There are times when trimming rows or columns is preferred 
#' over the default behavior.
#' This function trims rows or columns in `a` to match 
#' the rows or columns of `mat`.
#' The return value will have rows or columns of `a` removed if they do not appear in `mat`.
#' 
#' If `a` is `NULL`, `NULL` is returned.
#' If `mat` is `NULL`, `a` is returned unmodified.
#' If `mat` has `NULL` dimnames, `a` is returned unmodified.
#'
#' @param a A matrix to be trimmed.
#' @param mat The matrix 
#' @param margin The dimension of `a` to be trimmed. `1` means rows; `2` means columns.
#'               Default is `c(1,2)`.
#'
#' @return Matrix `a` with rows or columns trimmed to match `mat`.
#' 
#' @export
#'
#' @examples
trim_rows_cols <- function(a = NULL, mat = NULL, margin = c(1,2)) {
  
  
  if (is.null(a) & is.null(mat)) {
    stop("Both a and mat are NULL in complete_rows_cols.")
  }
  if (is.data.frame(a)) {
    stop("a cannot be a data frame in complete_rows_cols.")
  }
  
  if (is.list(a) & !is.data.frame(a) & !is.matrix(a)) {
    # Assume we have a list of matrices for a, not a single matrix.
    # Double-check that we have what we need in the mat argument.
    if (is.null(mat)) {
      # mat is a single NULL value.  But we need a list of
      # NULL values for the Map function.
      # Make a list of same length as the list of a.
      # Each value is NA.
      mat <- make_list(NULL, length(a))
    } else if (is.matrix(mat)) {
      # We have a single matrix for matrix.
      # Duplicate it to be a list with same length as a.
      mat <- make_list(mat, length(a))
    }
  } else if (is.null(a) & is.list(mat) & !is.data.frame(mat) & !is.matrix(mat)) {
    # a is NULL, and assume we have a list of matrices in the mat argument.
    # Under these conditions, we return matrices with same row and column names as each mat, but
    # filled with the "fill" value.
    # For that to work, we need to ensure that each of the other arguments are lists.
    a = make_list(NULL, length(mat))
    margin <- make_list(margin, length(mat), lenx = 1)
  }
  
  # Double-check that we have what we need for the margin argument.
  margin <- prep_vector_arg(a, margin)
  
  trim_func <- function(a = NULL, mat = NULL, margin) {
    # When we get here, we should not have lists for any of the arguments.
    # We should have single matrices for a and/or matrix. 
     
    if (is.null(a)) {
      return(NULL)
    }
    if (is.null(mat)) {
      return(a)
    }
    
    dimnamesmat <- dimnames(mat)
    if (is.null(dimnamesmat)) {
      # dimnamesmat is null, even after trying to gather row and column names from mat.  
      # We can't do anything, so issue a warning and return a
      warning("NULL names in trim_rows_cols, despite 'mat' being specified. Returning 'a' unmodified.")
      return(a)
    }
    
        
  }
  
  binaryapply_byname(trim_func, a = a, b = mat, .FUNdots = list(margin = margin), 
                     match_type = "all", set_rowcoltypes = TRUE, .organize = FALSE)
  
}