#' Reallocate values from one row or column to another
#'
#' There are situations where it is helpful to 
#' reallocate values from one row or column to another,
#' in proportion to remaining values in corresponding columns or rows.
#' This function performs the reallocation operation.
#' See examples.
#' 
#' This function will provide answers, but 
#' it is unlikely that the answers will be meaningful, when the
#' remaining data (the rows or columns not being distributed)
#' contain negative numbers.
#'
#' @param a A matrix or a list of matrices.
#' @param rowcolnames The names of the rows or columns to be redistributed.
#' @param margin The margin of the matrix on which the `rowcolnames` are located.
#'               Default is `c(1, 2)`, meaning that both rows (`1`) and columns (`2`)
#'               will be checked for `rowcolnames` and redistributed.
#' @param pattern_type The pattern type desired. Default is "exact". 
#'                     Other options are "leading", "trailing", "anywhere", 
#'                     and "literal". 
#'                     See [RCLabels::make_or_pattern()] for details.
#' @param prepositions Prepositions used by [matsbyname::select_rowcol_piece_byname()]
#'                     for row and column name matching.
#'                     Default is [RCLabels::prepositions_list].
#' @param notation The row or column notation used by [matsbyname::select_rowcol_piece_byname()]
#'                     for row and column name matching.
#'                     Default is [RCLabels::notations_list].
#'
#' @return A modified version of `a` with `rowcolnames` redistributed.
#'
#' @export
#'
#' @examples
#' m <- matrix(c(1, 2, 3,
#'               4, 5, 6,
#'               7, 8, 9), 
#'             nrow = 3, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("r1", "r2", "r3"), 
#'             c("c1", "c2", "c3")))
#' m
#' # Move row 3 into the other rows proportionally
#' reallocate_byname(m, rowcolnames = "r3", margin = 1)
#' # Move column 2 into the other columns proportionally
#' reallocate_byname(m, rowcolnames = "c2", margin = 2)
reallocate_byname <- function(a, 
                                rowcolnames = NULL,
                                margin = c(1, 2), 
                                piece = "all", 
                                pattern_type = "exact", 
                                prepositions = RCLabels::prepositions_list, 
                                notation = RCLabels::notations_list) {
  margin <- prep_vector_arg(a, margin)
  redistribute_func <- function(a_mat, margin) {
    if (length(margin) != length(unique(margin))) {
      stop("margin must contain unique integers in matsbyname::redistribute_byname()")
    }
    if (!length(margin) %in% c(1,2)) {
      stop("margin must have length 1 or 2 in matsbyname::redistribute_byname()")
    }
    
    if (!all(sapply(margin, function(mar) {mar %in% c(1,2)}))) {
      stop("margin must be 1, 2, or c(1, 2) in matsbyname::redistribute_byname()")
    }

    out <- a_mat
    if (2 %in% margin) {
      out <- out |> 
        matsbyname::transpose_byname() |> 
        redistribute_func(margin = 1) |> 
        matsbyname::transpose_byname()
    }
    if (1 %in% margin) {
      # These are the rows to be redistributed
      redistrows <- out |> 
        matsbyname::select_rowcol_piece_byname(retain = rowcolnames, 
                                               margin = margin, 
                                               piece = piece, 
                                               pattern_type = pattern_type, 
                                               prepositions = prepositions, 
                                               notation = notation)
      # Calculate the diagonal matrix that will multiply into keepfracs
      hatmat <- redistrows |> 
        matsbyname::colsums_byname() |> 
        matsbyname::hatize_byname(keep = "colnames")
      
      # These are the rows into which the redistribution will happen
      keeprows <- out |>
        matsbyname::select_rowcol_piece_byname(remove = rowcolnames, 
                                               margin = margin, 
                                               piece = piece, 
                                               pattern_type = pattern_type, 
                                               prepositions = prepositions, 
                                               notation = notation)
      
      # Find which columns in keeprows have all zero values.
      keeprows_zerocol <- apply(keeprows, 
                                MARGIN = 2, 
                                FUN = function(thiscol) {
                                  all(thiscol == 0)
                                })
      # Find which redistribution values are non-zero
      non_zero_redist_rows <- redistrows[1, ] != 0
      # We will have a potential problem for those columns
      # where both are true.
      # In this case, we can't know how to do the redistribution.
      problem_cols <- keeprows_zerocol & non_zero_redist_rows
      
      # Give a warning when there are any problem_cols
      if (any(problem_cols)) {
        names_problem_cols <- names(problem_cols[which(problem_cols)])
        rows_or_cols <- ifelse(margin == 1, "rows", "columns")
        warning(paste0("The following cannot be redistributed due to all zero values in the receiving ", 
                       rows_or_cols, 
                       ": ", 
                       paste0(names_problem_cols, collapse = ", ")))
      }
      
      # Calculate fractions of redistribution that should be placed in each keeprows
      keepfracs <- keeprows |> 
        matsbyname::fractionize_byname(margin = 2)
      
      # Calculate the matrix that should be added to keeprows
      addmat <- matsbyname::matrixproduct_byname(keepfracs, hatmat)
      
      # Return the sum of keeprows and addmat
      out <- matsbyname::sum_byname(keeprows, addmat)
    }
    
    return(out)
  }
  unaryapply_byname(redistribute_func, a = a,
                    .FUNdots = list(margin = margin), 
                    rowcoltypes = "all")
}