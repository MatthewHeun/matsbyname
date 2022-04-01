#' Change row and column name notation
#' 
#' This function switches matrix row and/or column names from one type of notation to another 
#' based on the `from` and `to` arguments.
#' Optionally, prefix and suffix can be `flip`ped.
#'
#' @param a A matrix or list of matrices whose row and/or column notation is to be changed.
#' @param margin `1` For rows, `2` for columns, or `c(1, 2)` for both rows and columns. Default is `c(1, 2)`.
#' @param from The `notation` to switch _away from_.
#' @param to The `notation` to switch _to_.
#' @param flip A boolean that tells whether to also flip the notation. Default is `FALSE`.
#'
#' @return Matrices with row and column names with switched notation, per arguments.
#'
#' @export
#' 
#' @examples
#' m <- matrix(c(1, 2, 
#'               3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("b [a]", "d [c]"), c("f [e]", "h [g]"))) %>% 
#'   setrowtype("Products [Industries]") %>% setcoltype("Industries [Products]")
#' m
#' switch_notation_byname(m, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation, 
#'                        flip = TRUE)
#' # Also works for lists.
#' # Note that margin must be specified as a list here.
#' switch_notation_byname(list(m, m), margin = list(c(1, 2)), 
#'                        from = RCLabels::bracket_notation, 
#'                        to = RCLabels::arrow_notation, flip = TRUE)
switch_notation_byname <- function(a, margin = c(1, 2), from, to, flip = FALSE) {
  margin <- prep_vector_arg(a, margin)
  from <- prep_vector_arg(a, from)
  to <- prep_vector_arg(a, to)
  flip <- prep_vector_arg(a, flip)
  switch_func <- function(a_mat, margin, from, to, flip) {
    # When we get here, we should have a single matrix a_mat.
    assertthat::assert_that(all(margin %in% c(1, 2)), msg = paste0("In switch_notation_byname, margin must be 1, 2, or both. ", 
                                                                   "Found margin = ", paste(margin, collapse = ", ")))
    
    out <- a_mat
    if (2 %in% margin) {
      # Transpose the matrices
      transposed <- matsbyname::transpose_byname(out)
      # re-call with margin = 1 to change from arrow to paren notation on the rows (which are really columns)
      switched <- switch_notation_byname(transposed, margin = 1, from = from, to = to, flip = flip)
      # Transpose
      out <- transpose_byname(switched)
    }
    if (1 %in% margin) {
      # Get the row names
      old_rownames <- getrownames_byname(out)
      # call func on old row names to create new row names
      new_rownames <- RCLabels::switch_notation(old_rownames, from = from, to = to, flip = flip)
      # Set row names to the new row names
      out <- setrownames_byname(out, new_rownames)
      # Perform the same transformation on the row type, but only if we had a rowtype in a_mat
      if (!is.null(rowtype(out))) {
        old_rowtype <- rowtype(out)
        new_rowtype <- RCLabels::switch_notation(old_rowtype, 
                                                 from = from, to = to, flip = flip)
        out <- out %>% setrowtype(new_rowtype)
      }
    }
    # Return the result
    return(out)
  }

  unaryapply_byname(switch_func, a, 
                    .FUNdots = list(margin = margin, from = from, to = to, flip = flip), 
                    # We control row and column types in this function, so 
                    # prevent unaryapply_byname from setting them.
                    rowcoltypes = "none")
}
