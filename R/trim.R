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
#' If `mat` has `NULL` for dimnames on `margin`, an error is returned.
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
#' a <- matrix(c(1, 2, 3, 
#'               4, 5, 6, 
#'               7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
#'  setrowtype("rowtype") %>% setcoltype("coltype")
#' mat <- matrix(c(1, 2, 3,
#'               4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
#'  setrowtype("rowtype") %>% setcoltype("coltype")
#' trim_rows_cols(a, mat, margin = 1)
#' trim_rows_cols(a, mat, margin = 2)
#' trim_rows_cols(a, mat)
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
    
    if (!is.matrix(a)) {
      stop("a must be a matrix in trim_rows_cols.")
    }
    
    dimnamesmat <- dimnames(mat)
    if (is.null(dimnamesmat)) {
      # dimnamesmat is null, even after trying to gather row and column names from mat.  
      # We can't do anything, so issue a warning and return a
      warning("NULL names in trim_rows_cols, despite 'mat' being specified. Returning 'a' unmodified.")
      return(a)
    }
    
    # Preserve row and column type.
    rt <- rowtype(a)
    ct <- coltype(a)
    
    # At this point, we should have 
    #   * a single matrix a,
    #   * names with which to trim the dimnames of a (dimnamesmat),
    #   * an indication of which margin(s) over which to trim the names (in the margin argument).
    
    for (mar in margin) {
      # Check that row or column names are available for the margin to be trimmed
      # If not, this is almost certainly an unintended error by the caller.
      if (is.null(dimnamesmat[[mar]])) {
        stop(paste("NULL dimnames for margin =", mar, "on 'mat'"))
      }
    }
    
    out <- a
    if (2 %in% margin) {
      out <- trim_func(a = transpose_byname(out), 
                       mat = transpose_byname(mat),
                       margin = 1) %>% 
        transpose_byname()
    }
    
    if (1 %in% margin) {
      # Here's where we do the business!
      
      # Get the row names of mat
      rows_to_keep <- dimnames(out)[[1]] %in% dimnamesmat[[1]]
      out <- out[rows_to_keep, , drop = FALSE]
    }
    return(out)
  }
  
  binaryapply_byname(trim_func, a = a, b = mat, .FUNdots = list(margin = margin), 
                     match_type = "all", set_rowcoltypes = TRUE, .organize = FALSE)
  
}