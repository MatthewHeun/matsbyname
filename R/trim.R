#' Trim rows and/or columns from a matrix
#' 
#' By default, the `matsbyname` package expands matrices
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
#' A common use case for this function is to trim `a`, because it has too many 
#' entries on `margin`s compared to `mat`.
#' This trimming will result in a smaller result for any mathematical operations 
#' involving `a` and `mat`.
#' Typically, `a` should cover all the entries in `mat` on `margin`.
#' Thus, by default, this function warns if `a` is missing entries on `margin`
#' that are present in `mat`.
#' To turn off this checking behavior, set `warn_if_a_incomplete = FALSE`.
#' 
#' `a_piece` and `mat_piece`
#' control which part of row and column names are 
#' compared before trimming.
#' The default values for `a_piece` and `mat_piece` are "all", 
#' meaning that the entire label should be matched.
#' Other options for `a_piece` and `mat_piece` are "pref" and "suff",
#' which will match the prefix or suffix of the labels.
#' Alternatively, prepositions can be given such that 
#' objects of prepositions will be matched.
#' Examples include "from" or "in".
#' See [RCLabels::get_piece()] for details.
#'
#' @param a A matrix to be trimmed.
#' @param mat The matrix to be used as the template for rows and/or columns of `a`.
#' @param margin The dimension of `a` to be trimmed. `1` means rows; `2` means columns.
#'               Default is `c(1,2)`.
#' @param warn_if_a_incomplete When `TRUE` (the default), a warning is emitted
#'                             if `a` is missing entries on `margin` that are present in `mat`.
#'                             Default is `TRUE`.
#' @param a_piece The portion of `a` labels to be used for comparison. Default is "all".
#' @param mat_piece The portion of `mat` labels to be used for comparison. Default is "all".
#' @param notation The notation for row and column labels. 
#'                 Default is `RCLabels::bracket_notation`.
#' @param prepositions The strings to be treated as prepositions in row and column labels.
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return Matrix `a` with rows or columns trimmed to match `mat`.
#' 
#' @export
#' 
#' @seealso `RCLabels::get_piece()`, which is used internally.
#'
#' @examples
#' a <- matrix(c(1, 2, 3, 
#'               4, 5, 6, 
#'               7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
#'  setrowtype("rowtype") %>% setcoltype("coltype")
#' mat <- matrix(c(1, 2, 3,
#'                 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
#'  setrowtype("rowtype") %>% setcoltype("coltype")
#' trim_rows_cols(a, mat, margin = 1)
#' trim_rows_cols(a, mat, margin = 2)
#' trim_rows_cols(a, mat)
trim_rows_cols <- function(a = NULL, mat = NULL, 
                           margin = c(1,2), warn_if_a_incomplete = TRUE, 
                           a_piece = "all", mat_piece = "all", 
                           notation = RCLabels::bracket_notation, 
                           prepositions = RCLabels::prepositions_list) {
  
  if (is.null(a) & is.null(mat)) {
    stop("Both a and mat are NULL in complete_rows_cols.")
  }
  if (is.data.frame(a)) {
    stop("a cannot be a data frame in complete_rows_cols.")
  }
  
  if (is.list(a) & !is.data.frame(a) & !is_matrix_or_Matrix(a)) {
    # Assume we have a list of matrices for a, not a single matrix.
    # Double-check that we have what we need in the mat argument.
    if (is.null(mat)) {
      # mat is a single NULL value.  But we need a list of
      # NULL values for the Map function.
      # Make a list of same length as the list of a.
      # Each value is NA.
      mat <- RCLabels::make_list(NULL, length(a))
    } else if (is_matrix_or_Matrix(mat)) {
      # We have a single matrix for matrix.
      # Duplicate it to be a list with same length as a.
      mat <- RCLabels::make_list(mat, length(a))
    }
  }
  
  # Double-check that we have what we need for the margin argument.
  margin <- prep_vector_arg(a, margin)
  
  # Ensure that we have what we need for notation and prepositions
  notation <- prep_vector_arg(a, notation)
  prepositions <- prep_vector_arg(a, prepositions)
  
  trim_func <- function(a_mat = NULL, mat_mat = NULL, margin) {
    # When we get here, we should not have lists for any of the arguments.
    # We should have single matrices for a and/or mat. 
    
    if (is.null(a_mat)) {
      return(NULL)
    }
    if (is.null(mat_mat)) {
      return(a_mat)
    }
    
    if (!is_matrix_or_Matrix(a_mat)) {
      stop("a_mat must be a matrix or a Matrix in matsbyname::trim_rows_cols().")
    }
    
    assertthat::assert_that(all(margin %in% c(1, 2)), 
                            msg = "margin must be 1, 2 or both in matsbyname::trim_rows_cols()")
    
    dimnames_mat_mat <- dimnames(mat_mat)
    if (is.null(dimnames_mat_mat) | identical(dimnames_mat_mat, list(NULL, NULL))) {
      # dimnamesmat is NULL (for a matrix) or list(NULL, NULL) for a Matrix, 
      # even after trying to gather row and column names from mat.  
      # We can't do anything, so issue a warning and return a_mat
      warning("NULL names in trim_rows_cols, despite 'mat_mat' being specified. Returning 'a_mat' unmodified.")
      return(a_mat)
    }
    
    # Preserve row and column type to re-apply later.
    rt <- rowtype(a_mat)
    ct <- coltype(a_mat)
    
    # At this point, we should have 
    #   * a single matrix a_mat,
    #   * names with which to trim the dimnames of a (dimnamesmat),
    #   * an indication of which margin(s) over which to trim the names (in the margin argument).
    
    for (mar in margin) {
      # Check that row or column names are available for the margin to be trimmed
      # If not, this is almost certainly an unintended error by the caller.
      if (is.null(dimnames_mat_mat[[mar]])) {
        stop(paste("NULL dimnames for margin =", mar, "on 'mat'"))
      }
    }
    
    out <- a_mat
    if (2 %in% margin) {
      out <- trim_func(a_mat = transpose_byname(out), 
                       mat_mat = transpose_byname(mat_mat),
                       margin = 1) %>% 
        transpose_byname()
    }
    
    if (1 %in% margin) {
      # If we get here, margin = 1 is desired. Do some business!
      
      # Get the row names of mat
      # rows_to_keep <- dimnames(out)[[1]] %in% dimnamesmat[[1]]
      a_pieces <- RCLabels::get_piece(dimnames(out)[[1]], piece = a_piece, 
                                      notation = notation, prepositions = prepositions)
      mat_pieces <- RCLabels::get_piece(dimnames_mat_mat[[1]], piece = mat_piece, 
                                        notation = notation, prepositions = prepositions)
      rows_to_keep <- a_pieces %in% mat_pieces
      out <- out[rows_to_keep, , drop = FALSE]
      
      # Check if a is missing rows of R. 
      # This condition is likely to be an error, 
      # so emit a warning.
      if (warn_if_a_incomplete) {
        missing_in_a <- setdiff(mat_pieces, a_pieces)
        if (length(missing_in_a) > 0) {
          warning(paste("In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat':",
                        paste(missing_in_a, collapse = ", ")))
        }
      }
      
    }
    return(out)
  }
  
  binaryapply_byname(trim_func, a = a, b = mat, .FUNdots = list(margin = margin), 
                     match_type = "all", set_rowcoltypes = TRUE, .organize = FALSE)
  
}
