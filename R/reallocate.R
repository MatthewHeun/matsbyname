#' Reallocate values from one row or column to others
#'
#' There are situations where it is helpful to 
#' reallocate values from one row or column to another,
#' in proportion to remaining values in corresponding columns or rows.
#' This function performs the reallocation operation.
#' See examples.
#' 
#' This function will provide answers, but 
#' it is unlikely that the answers will be meaningful when the
#' remaining data (the rows or columns not being reallocated)
#' contain negative numbers.
#' 
#' The value of `margin` affects the interpretation of 
#' `rownames` and `colnames`. 
#' If `margin = 1`, `rownames` identifies the rows to be reallocated to other rows. 
#' `colnames` identifies the columns to be reallocated, where
#' `NULL` (the default) means that all columns are reallocated.
#' If `margin = 2`, `colnames` identifies the columns to be reallocated to other columns. 
#' `rownames` identifies the rows to be reallocated, where
#' `NULL` (the default) means that all rows are reallocated.
#'
#' When the remaining rows or columns not being reallocated
#' contain exclusively zeroes, the result is determined by `.zero_behaviour`.
#' Options are one of:
#' * "error" (the default) to throw an error.
#' * "warning" to issue a warning but continue execution. Be careful with this option!
#' * "zeroes" to return zeroes in the row or column with zeroes. Note that "zeroes" and "warning" return the same value. "zeroes" does so without a warning.
#' * "allocate equally" to equally allocate across remaining rows or columns.
#' 
#' @param a A matrix or a list of matrices.
#' @param rownames The row names to reallocate. 
#'                 `NULL` (the default) means include all rows.
#' @param colnames The column names to reallocate.
#'                 `NULL` (the default) means include all rows.
#' @param margin An integer vector of length 1 or a vector of integers
#'               where each entry has length 1. 
#'               The margin of the matrix over which the reallocation should occur. 
#'               The only valid values are 
#'               `1` (reallocate to other rows) or
#'               `2` (reallocate to other columns).
#'               To reallocate both rows and columns, 
#'               call the function twice.
#' @param .zero_behaviour Tells how to proceed when remaining (i.e., unallocated) 
#'                        rows or columns are all zero.
#'                        Default is "error", which throws an error.
#'                        See details for other options.
#'                        If `a` is a `list`, applies to all items in the list.
#' @param piece_rownames,piece_colnames The piece of row or column names to be assessed.
#'                       Default is "all", indicating that the entire label will be assessed.
#'                        If `a` is a `list`, applies to all items in the list.
#' @param pattern_type_rownames,pattern_type_colnames The pattern type desired for row and column names.
#'                              Default is "exact". 
#'                              Other options are "leading", "trailing", "anywhere", 
#'                              and "literal". 
#'                              See [RCLabels::make_or_pattern()] for details.
#'                              If `a` is a `list`, applies to all items in the list.
#' @param prepositions_rownames,prepositions_colnames Prepositions used by [matsbyname::select_rowcol_piece_byname()]
#'                              for row and column name matching.
#'                               Default is [RCLabels::prepositions_list].
#'                               If `a` is a `list`, applies to all items in the list.
#' @param notation_rownames,notation_colnames The row or column notation used by [matsbyname::select_rowcol_piece_byname()]
#'                          for row and column name matching.
#'                          Default is [RCLabels::notations_list].
#'                          If `a` is a `list`, applies to all items in the list.
#' @param inf_notation_rownames,inf_notation_colnames A boolean used by [matsbyname::select_rowcol_piece_byname()]
#'                              that tells whether to infer notation for rows and columns.
#'                              Default is `TRUE`.
#'                              See [RCLabels::infer_notation()] for details.
#'                              If `a` is a `list`, applies to all items in the list.
#' @param choose_most_specific_rownames,choose_most_specific_colnames A boolean used by [matsbyname::select_rowcol_piece_byname()]
#'                                      that tells whether to choose the most specific
#'                                      notation from `notation` when inferring notation.
#'                                      Default is `FALSE` so that a less specific notation can be
#'                                      inferred.
#'                                      In combination with `notations_list`s,
#'                                      the default value of `FALSE` means that
#'                                      [RCLabels::bracket_notation] will be selected instead of
#'                                      anything more specific, such as
#'                                      [RCLabels::from_notation].
#'                                      If `a` is a `list`, applies to all items in the list.
#'                     
#' @return A modified version of `a` with `rownames` or `colnames` redistributed.
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
#' reallocate_byname(m, rownames = "r3", margin = 1)
#' # Move column 2 into the other columns (c1 and c3) proportionally
#' reallocate_byname(m, colnames = "c2", margin = 2)
#' # Demonstrate different options for reallocating when zeroes remain.
#' m2 <- matrix(c(1, 2,  0,
#'                4, 5,  0,
#'                7, 8, 10), 
#'              nrow = 3, ncol = 3, byrow = TRUE, 
#'              dimnames = list(c("r1", "r2", "r3"), 
#'              c("c1", "c2", "c3")))
#' m2
#' reallocate_byname(m2, rownames = "r3", margin = 1, 
#'                   .zero_behaviour = "zeroes")
#' reallocate_byname(m2, rownames = "r3", margin = 1, 
#'                   .zero_behaviour = "allocate equally")
#' \dontrun{
#' # "error" will cause an error to be emitted.
#' reallocate_byname(m2, rownames = "r3", margin = 1, 
#'                   .zero_behaviour = "error")
#' # "warning" will cause a warning to be emitted
#' # and will return a result that is the same as "zeroes".
#' reallocate_byname(m2, rownames = "r3", margin = 1, 
#'                   .zero_behaviour = "warning")
#' }
reallocate_byname <- function(a, 
                              rownames = NULL,
                              colnames = NULL,
                              margin, 
                              .zero_behaviour = c("error", "warning", "zeroes", "allocate equally"),
                              piece_rownames = "all", 
                              pattern_type_rownames = "exact", 
                              prepositions_rownames = RCLabels::prepositions_list, 
                              notation_rownames = RCLabels::notations_list, 
                              inf_notation_rownames = TRUE, 
                              choose_most_specific_rownames = FALSE,
                              piece_colnames = "all", 
                              pattern_type_colnames = "exact", 
                              prepositions_colnames = RCLabels::prepositions_list, 
                              notation_colnames = RCLabels::notations_list, 
                              inf_notation_colnames = TRUE, 
                              choose_most_specific_colnames = FALSE) {
  
  margin <- prep_vector_arg(a, margin)
  rownames <- prep_vector_arg(a, rownames)
  colnames <- prep_vector_arg(a, colnames)
  .zero_behaviour <- match.arg(.zero_behaviour, several.ok = FALSE)
  
  reallocate_func <- function(a_mat, rownames, colnames, margin) {
    if (length(margin) != 1) {
      stop("margin must have length 1 in matsbyname::reallocate_byname()")
    }
    
    if (! (margin %in% c(1, 2))) {
      stop("margin must be 1 or 2 in matsbyname::reallocate_byname()")
    }

    out <- a_mat
    if (margin == 2) {
      out <- out |> 
        matsbyname::transpose_byname() |> 
        reallocate_byname(rownames = colnames, 
                          colnames = rownames,
                          margin = 1, 
                          piece_rownames = piece_colnames,
                          pattern_type_rownames = pattern_type_colnames, 
                          prepositions_rownames = prepositions_colnames,
                          notation_rownames = notation_colnames, 
                          inf_notation_rownames = inf_notation_colnames, 
                          choose_most_specific_rownames = choose_most_specific_colnames, 
                          piece_colnames = piece_rownames, 
                          pattern_type_colnames = pattern_type_rownames, 
                          prepositions_colnames = prepositions_rownames,
                          notation_colnames = notation_rownames, 
                          inf_notation_colnames = inf_notation_rownames, 
                          choose_most_specific_colnames = choose_most_specific_rownames) |> 
        matsbyname::transpose_byname()
    }
    if (margin == 1) {
      
      # submat is the submatrix on which we will do the redistribution.
      # submat has only the columns that we want to redistribute
      if (is.null(colnames)) {
        submat <- out
      } else {
        submat <- out |> 
          matsbyname::select_rowcol_piece_byname(retain = colnames, 
                                                 margin = 2, 
                                                 piece = piece_colnames, 
                                                 pattern_type = pattern_type_colnames, 
                                                 prepositions = prepositions_colnames, 
                                                 notation = notation_colnames, 
                                                 inf_notation = inf_notation_colnames, 
                                                 choose_most_specific = choose_most_specific_colnames)
      }
      # If submat is NULL, there is no redistribution to be done.
      # Just return a_mat.
      if (is.null(submat)) {
        return(a_mat)
      }

      # These are the rows of submat to be redistributed
      redistrows <- submat |> 
        matsbyname::select_rowcol_piece_byname(retain = rownames, 
                                               margin = 1, 
                                               piece = piece_rownames, 
                                               pattern_type = pattern_type_rownames, 
                                               prepositions = prepositions_rownames, 
                                               notation = notation_rownames, 
                                               inf_notation = inf_notation_rownames, 
                                               choose_most_specific = choose_most_specific_rownames)
      # Calculate the diagonal matrix that will multiply into keepfracs
      hatmat <- redistrows |> 
        matsbyname::colsums_byname() |> 
        matsbyname::hatize_byname(keep = "colnames")
      
      # These are the rows into which the redistribution will happen
      keeprows <- submat |>
        matsbyname::select_rowcol_piece_byname(remove = rownames, 
                                               margin = 1, 
                                               piece = piece_rownames, 
                                               pattern_type = pattern_type_rownames, 
                                               prepositions = prepositions_rownames, 
                                               notation = notation_rownames, 
                                               inf_notation = inf_notation_rownames, 
                                               choose_most_specific = choose_most_specific_rownames)
      
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
      
      # Deal with warnings or errors when there are any problem_cols
      if (any(problem_cols)) {
        names_problem_cols <- names(problem_cols[which(problem_cols)])
        msg <- paste0(paste0(rownames, collapse = ", "), 
                      " cannot be reallocated due to all zero values remaining on the other margin: ", 
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
      submat <- matsbyname::sum_byname(keeprows, addmat)

      # Subtract the 1s, if needed
      if (any(problem_cols) & .zero_behaviour == "allocate equally") {
        submat <- matsbyname::difference_byname(submat, keeprows_problem_cols_1)
      }
      
      if (is.null(colnames)) {
        # Nothing to do here.
        out <- submat
      } else {
        # We selected only some columns in which to do the reallocation.
        # Reassemble the full matrix.
        out <- a_mat |> 
          # Start with the undisturbed columns
          matsbyname::select_rowcol_piece_byname(remove = colnames, 
                                                 margin = 2, 
                                                 piece = piece_colnames, 
                                                 pattern_type = pattern_type_colnames, 
                                                 prepositions = prepositions_colnames, 
                                                 notation = notation_colnames, 
                                                 inf_notation = inf_notation_colnames, 
                                                 choose_most_specific = choose_most_specific_colnames) |> 
          # Now add back submat
          matsbyname::sum_byname(submat)
      }
    }

    return(out)
  }
  unaryapply_byname(reallocate_func, a = a,
                    .FUNdots = list(rownames = rownames, colnames = colnames, margin = margin), 
                    rowcoltypes = "all")
}