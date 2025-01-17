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
#' remaining data (the rows or columns not being allocated)
#' contain negative numbers.
#' 
#' When the remaining rows or columns not being reallocated
#' contain zeroes, the result is determined by `.zero_behaviour`.
#' Options are one of:
#' * "error" (the default) to throw an error.
#' * "warning" to issue a warning but continue execution. Be careful with this option!
#' * "zeroes" to return zeroes in the row or column with zeroes. Note that "zeroes" and "warning" return the same value. "zeroes" does so without a warning.
#' * "allocate equally" to equally allocate across remaining rows or columns.
#'
#' @param a A matrix or a list of matrices.
#' @param rowcolnames The names of the rows or columns to be redistributed.
#' @param margin The margin of the matrix on which the `rowcolnames` are located.
#'               Default is `c(1, 2)`, meaning that both rows (`1`) and columns (`2`)
#'               will be checked for `rowcolnames` and redistributed.
#' @param .zero_behaviour Tells how to proceed when remaining (i.e., unallocated) 
#'                        rows or columns are all zero.
#'                        Default is "error", which throws an error.
#'                        See details for other options.
#' @param piece The piece of row or column names to be assessed.
#'              Default is "all", indicating that the entire label will be assessed.
#' @param pattern_type The pattern type desired. Default is "exact". 
#'                     Other options are "leading", "trailing", "anywhere", 
#'                     and "literal". 
#'                     See [RCLabels::make_or_pattern()] for details.
#' @param prepositions Prepositions used by [matsbyname::select_rowcol_piece_byname()]
#'                     for row and column name matching.
#'                     Default is [RCLabels::prepositions_list].
#' @param notation The row or column notation used by [matsbyname::select_rowcol_piece_byname()]
#'                 for row and column name matching.
#'                 Default is [RCLabels::notations_list].
#' @param inf_notation A boolean used by [matsbyname::select_rowcol_piece_byname()]
#'                     that tells whether to infer notation for rows and columns.
#'                     Default is `TRUE`.
#'                     See [RCLabels::infer_notation()] for details.
#' @param choose_most_specific A boolean used by [matsbyname::select_rowcol_piece_byname()]
#'                             that tells whether to choose the most specific
#'                             notation from `notation` when inferring notation.
#'                             Default is `FALSE` so that a less specific notation can be
#'                             inferred.
#'                             In combination with [RCLabels::notations_list],
#'                             the default value of `FALSE` means that
#'                             [RCLabels::bracket_notation] will be selected instead of
#'                             anything more specific, such as
#'                             [RCLabels::from_notation].
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
#' # Move row 3 into the other rows (r1 and r2) proportionally
#' reallocate_byname(m, rowcolnames = "r3", margin = 1)
#' # Move column 2 into the other columns (c1 and c3) proportionally
#' reallocate_byname(m, rowcolnames = "c2", margin = 2)
#' # Demonstrate different options for reallocating when zeroes remain.
#' m2 <- matrix(c(1, 2,  0,
#'                4, 5,  0,
#'                7, 8, 10), 
#'              nrow = 3, ncol = 3, byrow = TRUE, 
#'              dimnames = list(c("r1", "r2", "r3"), 
#'              c("c1", "c2", "c3")))
#' m2
#' reallocate_byname(m2, rowcolnames = "r3", margin = 1, 
#'                   .zero_behaviour = "zeroes")
#' reallocate_byname(m2, rowcolnames = "r3", margin = 1, 
#'                   .zero_behaviour = "allocate equally")
reallocate_byname <- function(a, 
                              rowcolnames = NULL,
                              margin = c(1, 2), 
                              .zero_behaviour = c("error", "warning", "zeroes", "allocate equally"),
                              piece = "all", 
                              pattern_type = "exact", 
                              prepositions = RCLabels::prepositions_list, 
                              notation = RCLabels::notations_list, 
                              inf_notation = TRUE, 
                              choose_most_specific = FALSE) {
  margin <- prep_vector_arg(a, margin)
  .zero_behaviour <- match.arg(.zero_behaviour, several.ok = FALSE)
  reallocate_func <- function(a_mat, margin) {
    if (length(margin) != length(unique(margin))) {
      stop("margin must contain unique integers in matsbyname::reallocate_byname()")
    }
    if (!length(margin) %in% c(1,2)) {
      stop("margin must have length 1 or 2 in matsbyname::reallocate_byname()")
    }
    
    if (!all(sapply(margin, function(mar) {mar %in% c(1,2)}))) {
      stop("margin must be 1, 2, or c(1, 2) in matsbyname::reallocate_byname()")
    }

    out <- a_mat
    if (2 %in% margin) {
      out <- out |> 
        matsbyname::transpose_byname() |> 
        reallocate_func(margin = 1) |> 
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
                                               notation = notation, 
                                               inf_notation = inf_notation, 
                                               choose_most_specific = choose_most_specific)
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
                                               notation = notation, 
                                               inf_notation = inf_notation, 
                                               choose_most_specific = choose_most_specific)
      
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
        rows_or_cols <- ifelse(margin == 1, "columns", "rows")
        msg <- paste0(paste0(rowcolnames, collapse = ", "), 
                      " cannot be reallocated due to all zero values remaining in ", 
                      rows_or_cols, 
                      ": ", 
                      paste0(names_problem_cols, collapse = ", "))
        if (.zero_behaviour == "error") {
          stop(msg)
        } else if (.zero_behaviour == "warning") {
          warning(msg)  
        } else if (.zero_behaviour == "zeroes") {
          # Nothing to be done. 
        } else if (.zero_behaviour == "allocate equally") {
          # Change the 0s to 1s to obtain equal allocations
          # Will need later to subtract the 1.
          keeprows_problem_cols <- keeprows |> 
            matsbyname::select_cols_byname(retain_pattern = names(which(problem_cols)), fixed = TRUE)
          keeprows_problem_cols_1 <- keeprows_problem_cols + 1
          keeprows <- matsbyname::sum_byname(keeprows, keeprows_problem_cols_1)
        }
      }
      
      # Calculate fractions of redistribution that should be placed in each keeprows
      keepfracs <- keeprows |> 
        matsbyname::fractionize_byname(margin = 2)
      
      # Calculate the matrix that should be added to keeprows
      addmat <- matsbyname::matrixproduct_byname(keepfracs, hatmat)
      
      # Return the sum of keeprows and addmat
      out <- matsbyname::sum_byname(keeprows, addmat)
      
      # Subtract the 1s, if needed
      if (any(problem_cols) & .zero_behaviour == "allocate equally") {
        out <- matsbyname::difference_byname(out, keeprows_problem_cols_1)
      }
    }
    
    return(out)
  }
  unaryapply_byname(reallocate_func, a = a,
                    .FUNdots = list(margin = margin), 
                    rowcoltypes = "all")
}