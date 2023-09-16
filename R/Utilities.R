#' Organize binary arguments
#'
#' Organizes arguments of binary (2 arguments) `_byname` functions.
#' Actions performed are:
#' \itemize{
#'  \item{if only one argument is a list, make the other argument also a list of equal length.}
#'  \item{if both arguments are lists, ensure that they are same length.}
#'  \item{if one argument is a matrix and the other is a constant, make the constant into a matrix.}
#'  \item{ensures that row and column types match for \code{typematch_margins}.}
#'  \item{ensures that list item names match if both \code{a} and \code{b} are lists; 
#'        no complaints are made if neither \code{a} nor \code{b} has names.}
#'  \item{completes and sorts the matrices.}
#' }
#'
#' @param a the first argument to be organized
#' @param b the second argument to be organized
#' @param match_type one of \code{"all"}, \code{"matmult"}, \code{"none"}.
#' When both \code{a} and \code{b} are matrices,
#' "\code{all}" (the default) indicates that
#' rowtypes of \code{a} must match rowtypes of \code{b} and
#' coltypes of \code{a} must match coltypes of \code{b}.
#' If "\code{matmult}",
#' coltypes of \code{a} must match rowtypes of \code{b}.
#' @param fill a replacement value for \code{a} or \code{b} if either is missing or \code{NULL}.
#'
#' @return a list with two elements (named \code{a} and \code{b}) containing organized versions of the arguments
organize_args <- function(a, b, match_type = "all", fill){
  if (missing(a)) {
    if (missing(fill)) {
      stop("Missing argument a with no fill in organize_args.")
    } else {
      a <- fill
    }
  }
  if (is.null(a)) {
    if (missing(fill)) {
      stop("Null argument a with no fill in organize_args.")
    } else {
      a <- fill
    }
  }
  if (missing(b)) {
    if (missing(fill)) {
      stop("Missing argument b with no fill in organize_args.")
    } else {
      b <- fill
    }
  }
  if (is.null(b)) {
    if (missing(fill)) {
      stop("Null argument b with no fill in organize_args.")
    } else {
      b <- fill
    }
  }
  if (is.list(a) | is.list(b)) {
    if (!is.list(a)) {
      # b is a list, but a is not.  Make a into a list and give it same names as b.
      a <- RCLabels::make_list(a, n = length(b)) %>% magrittr::set_names(names(b))
    }
    if (!is.list(b)) {
      # a is a list, but b is not.  Make b into a list and give it same names as a.
      b <- RCLabels::make_list(b, n = length(a)) %>% magrittr::set_names(names(a))
    }
  }
  if (is.list(a) & is.list(b)) {
    # Both a and b are lists. Ensure they're the same length.
    stopifnot(length(a) == length(b))
    # Ensure that a and b have same length of names
    stopifnot(length(names(a)) == length(names(b)))
    # Ensure that a and b have same names if either has names
    stopifnot(names(a) == names(b))
    # Now return the lists.
    return(list(a = a, b = b))
  }
  
  # Neither a nor b are lists.
  # if (!is.matrix(a) & !is.matrix(b)) {
  if (!is_matrix_or_Matrix(a) & !is_matrix_or_Matrix(b)) {
    # Neither a nor b are matrices. Assume we have two constants. Return the constants in a vector.
    return(list(a = a, b = b))
  }
  
  # Neither a nor b are lists.
  # First check whether matchtype is a known value.
  if (!match_type %in% c("all", "matmult", "none"))  {
    stop(paste("Unknown match_type", match_type, "in organize_args."))
  }
  # We don't know if one or both a and b is a matrix.
  # If one is not a matrix, assume it is a constant and try to make it into an appropriate-sized matrix.
  # if (!is.matrix(a) & is.matrix(b)) {
  if (!is_matrix_or_Matrix(a) & is_matrix_or_Matrix(b)) {
    if (is.Matrix(b)) {
      a <- matsbyname::Matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b))
    } else {
      a <- matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b))
    }
    if (match_type == "all") {
      a <- a %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
    } 
    if (match_type == "matmult") {
      a <- a %>% setcoltype(rowtype(b))
    }
    # If matchtype == "none", we don't to anything.
  } else if (is_matrix_or_Matrix(a) & !is_matrix_or_Matrix(b)) {
    if (is.Matrix(a)) {
      b <- matsbyname::Matrix(b, nrow = nrow(a), ncol = ncol(a), dimnames = dimnames(a))
    } else {
      b <- matrix(b, nrow = nrow(a), ncol = ncol(a), dimnames = dimnames(a))
    }
    if (match_type == "all") {
      b <- b %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    }
    if (match_type == "matmult") {
      b <- b %>% setrowtype(coltype(a))
    }
    # If matchtype == "none", we don't to anything.
  }
  
  # Assume that both a and b are now matrices.
  
  # Verify that row and column types are appropriate.
  if (match_type == "all") {
    # If neither rowtype nor coltype are set,
    # skip these tests
    if (!is.null(rowtype(a)) & !is.null(coltype(a)) & !is.null(rowtype(b)) & !is.null(coltype(b))) {
      # Verify that the row type of a and b are the same.
      if (rowtype(a) != rowtype(b)) {
        stop(paste0("rowtype(a) (", rowtype(a), ") != rowtype(b) (", rowtype(b),")."))
      }
      # Verify that the column type of a and b are the same.
      if (coltype(a) != coltype(b)) {
        stop(paste0("coltype(a) (", coltype(a), ") != coltype(b) (", coltype(b),")."))
      }
    }
  } 
  if (match_type == "matmult") {
    # If neither coltype(a) nor rowtype(b) are set,
    # skip this test
    if (!is.null(coltype(a)) & !is.null(rowtype(b))) {
      # Verify that the column type of a and the row type of b are the same.
      if (coltype(a) != rowtype(b)) {
        stop(paste0("coltype(a) != rowtype(b): ", coltype(a), " != ", rowtype(b),"."))
      }
    }
  } 
  
  # We already ensured that match_type was one of the known types. 
  # Ensure that matrices have correct row and column names and are in same order.
  if (match_type == "all") {
    matrices <- complete_and_sort(a, b)
    outa <- matrices$a %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    outb <- matrices$b %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (match_type == "matmult") {
    # When the match_type is "matmult", we need to ensure that the columns of a match the rows of b.
    # To do so, we transpose b prior to completing and sorting, and we complete and sort on columns.
    matrices <- complete_and_sort(a, transpose_byname(b), margin = 2)
    outa <- matrices$a %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    # Before sending back, we need to re-transpose b.
    outb <- matrices$b %>% transpose_byname %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (match_type == "none") {
    outa <- a
    outb <- b
  }
  return(list(a = outa, b = outb))
}


#' Prepare a vector argument
#' 
#' This is a helper function for many `*_byname` functions.
#' 
#' It is potentially ambiguous to specify a vector or matrix argument, say, `margin = c(1, 2)` when applying
#' the `*_byname` functions to unary list of `a`.
#' Rather, one should specify, say, `margin = list(c(1, 2))`
#' to avoid ambiguity.
#' If `a` is a list, 
#' `vector_arg` is not a list and has length > 1 and length not equal to the length of a,
#' this function returns a list value for `vector_arg`.
#' If `a` is not a list and `vector_arg` is a list, 
#' this function returns an un-recursive, unlisted version of `vector_arg`.
#' 
#' Note that if `vector_arg` is a single matrix, it is automatically enclosed by a list when `a` is a list.
#'
#' @param a A matrix or list of matrices.
#' @param vector_arg The vector argument over which to apply a calculation.
#'
#' @return `vector_arg`, possibly modified when `a` is a list.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(2, 2))
#' prep_vector_arg(m, vector_arg = c(1,2))
#' prep_vector_arg(list(m), vector_arg = c(1,2))
#' prep_vector_arg(list(m, m), vector_arg = c(1,2))
#' prep_vector_arg(list(m, m, m), vector_arg = c(1,2))
prep_vector_arg <- function(a, vector_arg) {
  if (is.list(a)) {
    if (is.matrix(vector_arg) | (!is.list(vector_arg) & length(vector_arg) > 1 & length(vector_arg) != length(a))) {
      # We probably want to make vector_arg into a list.
      vector_arg <- list(vector_arg)
    }
  } else {
    # a is not a list
    if (is.list(vector_arg)) {
      # We can unlist this vector_arg to use it directly.
      vector_arg <- unlist(vector_arg, recursive = FALSE)
    }
  }

  vector_arg
}


#' Named list of rows or columns of matrices
#' 
#' This function takes matrix \code{m} and converts it to a list of 
#' single-row (if \code{margin == 1}) or single-column(if \code{margin == 2})
#' matrices.
#' Each item in the list is named for its row (if \code{margin == 1}) 
#' or column (if \code{margin == 2}).
#'
#' Note that the result provides column vectors, regardless of the value of \code{margin}.
#'
#' @param a a matrix or list of matrices (say, from a column of a data frame)
#' @param margin the margin of the matrices to be extracted (\code{1} for rows, \code{2} for columns)
#'
#' @return a named list of rows or columns extracted from \code{m}
#' 
#' @export
#' 
#' @examples
#' m <- matrix(data = c(1:6), 
#'             nrow = 2, ncol = 3, 
#'             dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>%
#'   setrowtype(rowtype = "Products") %>% setcoltype(coltype = "Industries")
#' list_of_rows_or_cols(m, margin = 1)
#' list_of_rows_or_cols(m, margin = 2)
list_of_rows_or_cols <- function(a, margin){
  margin <- prep_vector_arg(a, margin)
  
  lrc_func <- function(a_mat, margin){
    stopifnot(length(margin) == 1)
    stopifnot(margin == 1 | margin == 2)
    stopifnot(is_matrix_or_Matrix(a_mat))
    # Strategy: perform all operations with margin to be split into a list in columns.
    if (margin == 1) {
      # Caller requested rows to be split into list items.
      # Transpose so operations will be easier.
      out <- transpose_byname(a_mat)
    } else {
      out <- a_mat
    }
    out <- lapply(seq_len(ncol(out)), function(i){
      if (is.Matrix(a_mat)) {
        result <- matsbyname::Matrix(out[,i], nrow = nrow(out), ncol = 1, 
                                     dimnames = list(rownames(out), colnames(out)[[i]]), 
                                     rowtype = rowtype(out), coltype = coltype(out))
      } else {
        result <- matrix(out[,i], nrow = nrow(out), ncol = 1, dimnames = list(rownames(out), colnames(out)[[i]])) %>%
          setrowtype(rowtype(out)) %>% setcoltype(coltype(out))
      }
      return(result)
    }) %>%
      magrittr::set_names(colnames(out))
    return(out)
  }
  unaryapply_byname(lrc_func, a = a, .FUNdots = list(margin = margin), 
                    rowcoltypes = "none")
}


#' Gets row names
#'
#' Gets row names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param a The matrix or data frame on which row names are to be retrieved
#'
#' @return row names of \code{a}
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' getrownames_byname(m)
#' # This also works for lists
#' getrownames_byname(list(m,m))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' getrownames_byname(DF$m)
getrownames_byname <- function(a){
  unaryapply_byname(rownames, a = a, rowcoltypes = "none")
}


#' Gets column names
#'
#' Gets column names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param a The matrix or data frame from which column names are to be retrieved
#'
#' @return Column names of `m`.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' getcolnames_byname(m)
#' # This also works for lists
#' getcolnames_byname(list(m,m))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' getcolnames_byname(DF$m)
getcolnames_byname <- function(a){
  unaryapply_byname(colnames, a = a, rowcoltypes = "none")
}


#' Sets row names
#'
#' Sets row names in a way that is amenable to use in piping operations in a functional programming way.
#' If \code{a} is \code{NULL}, \code{NULL} is returned.
#' If \code{a} is a constant, it is converted to a matrix and \code{rownames} are applied.
#' If \code{a} is a matrix, \code{rownames} should be a vector of new row names
#' that is as long as the number of rows in \code{a}.
#' If \code{a} is a list of matrices, 
#' \code{rownames} can also be a list, and it should be as long \code{a}.
#' Or \code{rownames} can be a vector of row names which will be applied to every matrix in
#' the list of \code{a}.
#' Each item in the list should be a vector containing row names for the corresponding 
#' matrix in \code{a}.
#'
#' @param a A matrix or a list of matrices in which row names are to be set
#' @param rownames A vector of new row names or a list of vectors of new row names
#'
#' @return a copy of \code{m} with new row names
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' setrownames_byname(m, c("a", "b"))
#' setrownames_byname(m %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
#' m %>% setrownames_byname(NULL)
#' m %>% setrownames_byname(c(NA, NA))
#' 2 %>% setrownames_byname("row")
#' # This also works for lists
#' setrownames_byname(list(m,m), list(c("a", "b")))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' setrownames_byname(DF$m, list(c("r1", "r2")))
#' setrownames_byname(DF$m, list(c("c", "d")))
#' DF <- DF %>% mutate(m = setrownames_byname(m, list(c("r1", "r2"))))
#' DF$m[[1]]
setrownames_byname <- function(a, rownames){
  rownames <- prep_vector_arg(a, rownames)
  if (is.null(a)) {
    return(NULL)
  }
  rowname_func <- function(a, rownames){
    if (is.null(dim(a))) {
      # a has no dimensions. It is a constant.
      # Turn it into a matrix and set the row names.
      out <- matrix(a, nrow = 1, ncol = 1)
    } else {
      out <- a
    }
    if (is.null(rownames)) {
      # replace with default row names
      rownames(out) <- NULL
    } else {
      rownames(out) <- rownames
    }
    return(out)
  }
  unaryapply_byname(rowname_func, a = a, .FUNdots = list(rownames = rownames), 
                    rowcoltypes = "all")
}


#' Sets column names
#'
#' Sets column names in a way that is amenable to use in piping operations in a functional programming way.
#' if \code{a} is \code{NULL}, \code{NULL} is returned.
#' If \code{a} is a constant, it is converted to a matrix and \code{colnames} are applied.
#' If \code{a} is a matrix, \code{colnames} should be a vector of new column names
#' that is as long as the number of columns in \code{a}.
#' If \code{a} is a list of matrices, 
#' \code{colnames} can also be a list, and it should be as long as \code{a}.
#' Or \code{colnames} can be a vector of column names which will be applied to every matrix in
#' the list of \code{a}.
#' Each item in the list should be a vector containing column names for the corresponding 
#' matrix in \code{a}.
#'
#' @param a A matrix or a list of matrices in which column names are to be set
#' @param colnames A vector of new column names or a list of vectors of new column names
#'
#' @return a copy of \code{a} with new column names
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' setcolnames_byname(m, c("a", "b", "c"))
setcolnames_byname <- function(a, colnames){
  if (is.null(a)) {
    return(NULL)
  }
  if (is.list(a) & !is.list(colnames)) {
    colnames <- list(colnames)
  }
  a %>% 
    transpose_byname() %>% 
    setrownames_byname(rownames = colnames) %>% 
    transpose_byname()
}


#' Rename matrix rows and columns by prefix and suffix
#' 
#' `r lifecycle::badge("superseded")`
#' It can be convenient to rename rows or columns of matrices 
#' based on retaining prefixes or suffixes.
#' This function provides that capability.
#' 
#' A prefix is defined by an opening string (`prefix_open`) and a closing string (`prefix_close`).
#' A suffix is defined by an opening string (`suffix_open`) and a closing string (`suffix_close`).
#' If `sep` is provided and none of `prefix_open`, `prefix_close`, `suffix_open`, and `suffix_close` are provided,
#' default arguments become:
#'     * `prefix_open`: "",
#'     * `prefix_close`: `sep`, 
#'     * `suffix_open`: `sep`, and
#'     * `suffix_close`: "".
#'     
#' The `keep` parameter tells which portion to retain (prefixes or suffixes), 
#' 
#' If prefixes or suffixes are not found in a row and/or column name, that name is unchanged.
#' 
#' @param a a matrix or list of matrices whose rows or columns will be renamed.
#' @param keep one of "prefix" or "suffix" indicating which part of the row or column name to retain.
#' @param margin one of `1`, `2`, or `c(1, 2)` where `1` indicates rows and `2` indicates columns.
#' @param notation See `notation_vec()`.
#'
#' @return `a` with potentially different row or column names.
#' 
#' @export
#'
#' @examples
#' # This function is superseded. 
#' # Instead, use `rename_to_pieces_byname()`.
#' # For example:
#' m <- matrix(c(1, 2, 
#'               3, 4, 
#'               5, 6), nrow = 3, byrow = TRUE, 
#'             dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
#' m
#' rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#' # Note, labels are lost, because some labels are missing a suffix.
#' rename_to_piece_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
#' # Original documentation:
#' rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
#' rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
rename_to_pref_suff_byname <- function(a, keep, margin = c(1, 2), notation) {
  rename_to_piece_byname(a, piece = keep, margin = margin, 
                         notation = notation, prepositions = RCLabels::prepositions_list)
}


#' Rename matrix rows and columns by piece of row or column names
#' 
#' It can be convenient to rename rows or columns of matrices 
#' based on retaining only a piece of the row and/or column names.
#' This function provides that capability.
#' 
#' Internally, this function finds pieces of row and column names 
#' via the `RCLabels` package. 
#' `piece` can be anything that `RCLabels::get_piece()` understands.
#' Note that `margin` can be either an integer vector or
#' a character vector. 
#' If `margin` is a character vector, 
#' it is interpreted as a row or column type, and
#' `margin_from_types_byname()` is called internally to 
#' resolve the integer margins of interest.
#' 
#' Note that if row and/or column type are present,
#' the row and/or column type are also renamed according to `piece`.
#'
#' @param a A matrix or list of matrices whose rows or columns will be renamed.
#' @param piece A character string indicating which piece of the row or column names to retain, 
#'              one of "noun", "pps", "pref" or "suff", or a preposition,
#'              indicating which part of the row or column name is to be retained.
#' @param margin As a character, the row type or column type to be renamed.
#'               As an integer, the margin to be renamed.
#'               Default is `c(1, 2)`, meaning that both 
#'               rows (`margin = 1`) and columns (`margin = 2`)
#'               will be renamed.
#' @param inf_notation A boolean that tells whether to infer notation.
#'                     Default is `TRUE`.
#' @param notation The notation used for row and column labels. 
#'                 Default is `list(RCLabels::notations_list)`.
#'                 The default value is wrapped in a list, 
#'                 because `RCLabels::notations_list` is, itself, a list.
#'                 See `RCLabels`.
#' @param choose_most_specific A boolean that indicates whether the most-specific notation
#'                             will be inferred when more than one of `notation` matches 
#'                             a row or column label
#'                             and `allow_multiple = FALSE`.
#'                             When `FALSE`, the first matching notation in `notations`
#'                             is returned when `allow_multiple = FALSE`.
#'                             Default is `FALSE`.
#' @param prepositions Prepositions that can be used in the row and column label.
#'                     Default is `RCLabels::prepositions_list`.
#'
#' @return A version of `a` with renamed rows and columns.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1, 2, 
#'               3, 4, 
#'               5, 6), nrow = 3, byrow = TRUE, 
#'             dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
#' m
#' rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#' m2 <- m %>%
#'   setrowtype("rows") %>% setcoltype("cols")
#' m2
#' rename_to_piece_byname(m2, piece = "pref", margin = "rows",
#'                        notation = RCLabels::arrow_notation)
#' rename_to_piece_byname(m2, piece = "suff", margin = "rows",
#'                        notation = RCLabels::arrow_notation)
rename_to_piece_byname <- function(a,
                                   piece,
                                   margin = list(c(1, 2)),
                                   inf_notation = TRUE,
                                   notation = list(RCLabels::notations_list),
                                   choose_most_specific = FALSE,
                                   prepositions = list(RCLabels::prepositions_list)) {
  piece <- prep_vector_arg(a, piece)
  margin <- prep_vector_arg(a, margin)
  inf_notation <- prep_vector_arg(a, inf_notation)
  notation <- prep_vector_arg(a, notation)
  choose_most_specific <- prep_vector_arg(a, choose_most_specific)
  prepositions <- prep_vector_arg(a, prepositions)
  
  rename_func <- function(a_mat, this_piece, this_margin, this_inf_notation, this_notation, 
                          this_choose_most_specific, these_prepositions) {
    # At this point, a should be a single matrix, 
    # this_* should be individual items ready for use in this function.
    
    # Figure out the margin.
    this_margin <- margin_from_types_byname(a_mat, this_margin)

    if (2 %in% this_margin) {
      # Want to rename columns.
      # Easier to transpose, recursively call ourselves to rename rows, and then transpose again.
      a_mat <- transpose_byname(a_mat) %>% 
        rename_func(this_piece = this_piece, 
                    this_margin = 1,
                    this_inf_notation = this_inf_notation,
                    this_notation = this_notation,
                    this_choose_most_specific,
                    these_prepositions = these_prepositions) %>% 
        transpose_byname()
    }
    
    if (1 %in% this_margin) {
      new_rnames <- rownames(a_mat) %>% 
        RCLabels::get_piece(piece = this_piece, 
                            inf_notation = this_inf_notation,
                            notation = this_notation,
                            choose_most_specific = this_choose_most_specific,
                            prepositions = these_prepositions)
      # Default is to return the old rowtype as the new rowtype
      new_rt <- rowtype(a_mat)
      if (!is.null(new_rt)) {
        # If we had a rowtype, see if we can find a notation for the rowtype.
        inferred_notation <- RCLabels::infer_notation(new_rt, 
                                                      inf_notation = this_inf_notation, 
                                                      notation = this_notation, 
                                                      choose_most_specific = this_choose_most_specific, 
                                                      must_succeed = FALSE)
        if (!is.null(inferred_notation)) {
          # Notation could be inferred.
          # Adjust the rowtype in the same way that we adjusted the row and column labels.
          new_rt <- new_rt %>%
            RCLabels::get_piece(piece = this_piece, 
                                inf_notation = this_inf_notation,
                                notation = this_notation, 
                                choose_most_specific = this_choose_most_specific,
                                prepositions = these_prepositions)
        }
      }
      
      # Set new rownames, without the names on the list (parts of the previous name)
      rownames(a_mat) <- unname(new_rnames)
      # Set new rowtype
      a_mat <- setrowtype(a_mat, unname(new_rt))
    }
    
    return(a_mat)
  }
  unaryapply_byname(rename_func, a = a,
                    .FUNdots = list(this_piece = piece,
                                    this_margin = margin,
                                    this_inf_notation = inf_notation,
                                    this_notation = notation,
                                    this_choose_most_specific = choose_most_specific,
                                    these_prepositions = prepositions), 
                    rowcoltypes = "none")
}


#' Translate row and column types to integer margins
#' 
#' Converts row and column types to integer margins,
#' based on `a` and `types`.
#' If `types` is not a character vector, `types` is returned unmodified.
#' If `types` is a character vector, an integer vector is returned
#' corresponding to the margins on which `types` are found.
#' If `types` are not found in the row or column types of `a`, 
#' `NA_integer_` is returned.
#'
#' @param a A matrix or list of matrices.
#' @param types A character vector or list of character vectors 
#'              representing row or column types whose 
#'              corresponding integer margins in `a` are to be determined.
#'
#' @return A vector of integers or list of vectors of integers 
#'         corresponding to the margins on which `types` exist.
#' 
#' @export
#'
#' @examples
#' # Works for single matrices
#' m <- matrix(1) %>%
#'   setrowtype("Product") %>% setcoltype("Industry")
#' margin_from_types_byname(m, "Product")
#' margin_from_types_byname(m, "Industry")
#' margin_from_types_byname(m, c("Product", "Industry"))
#' margin_from_types_byname(m, c("Industry", "Product"))
#' # Works for lists of matrices
#' margin_from_types_byname(list(m, m), types = "Product")
#' margin_from_types_byname(list(m, m), types = "Industry")
#' margin_from_types_byname(list(m, m), types = c("Product", "Product"))
#' margin_from_types_byname(list(m, m), types = c("Industry", "Industry"))
#' margin_from_types_byname(list(m, m), types = c("Product", "Industry"))
#' margin_from_types_byname(list(m, m), types = list("Product", "Industry"))
#' margin_from_types_byname(list(m, m), types = list(c("Product", "Industry")))
#' margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"), 
#'                                                   c("Product", "Industry")))
#' # Works in a data frame
#' m2 <- matrix(2) %>%
#'   setrowtype("Industry") %>% setcoltype("Product")
#' df <- tibble::tibble(m = list(m, m2), types = list("Product", c("Product", "Industry")))
#' res <- df %>%
#'   dplyr::mutate(
#'     margin = margin_from_types_byname(m, types)
#'  )
#' res$margin
margin_from_types_byname <- function(a, types) {
  
  types <- prep_vector_arg(a, types)
  
  mft_fun <- function(a_mat, these_types) {
    # At this point, a_mat and these_types should be single 
    # items, ready for use.
    if (!is.character(these_types)) {
      return(these_types)
    }
    margin <- c()
    if (rowtype(a_mat) %in% these_types) {
      margin <- margin %>% 
        append(1)
    }
    if (coltype(a_mat) %in% these_types) {
      margin <- margin %>%
        append(2)
    }
    if (length(margin) == 0) {
      return(NA_integer_)
    }
    return(margin)
  }
  unaryapply_byname(mft_fun, a = a, .FUNdots = list(these_types = types), rowcoltypes = "none")
}


#' Sets row type for a matrix or a list of matrices
#'
#' This function is a wrapper for `attr()` so that 
#' setting can be accomplished by the pipe operator (`%>%`).
#' Row types are strings stored in the `rowtype` attribute.
#' 
#' If `is.null(rowtype)`, the rowtype attribute is deleted
#' and subsequent calls to `rowtype` will return `NULL`.
#'
#' @param a The matrix on which row type is to be set.
#' @param rowtype The type of item stored in rows.
#'
#' @return `a` with rowtype attribute set to `rowtype.`
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
#' U %>% setrowtype("Commodities")
#' # This also works for lists
#' setrowtype(list(U,U), rowtype = "Commodities")
#' setrowtype(list(U,U), rowtype = list("Commodities", "Commodities"))
#' DF <- data.frame(U = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' setrowtype(DF$U, "Commodities")
#' DF <- DF %>% mutate(newcol = setrowtype(U, "Commodities"))
#' DF$newcol[[1]]
#' DF$newcol[[2]]
setrowtype <- function(a, rowtype){
  rt_func <- function(a, rowtype){
    attr(a, "rowtype") <- rowtype
    return(a)
  }
  unaryapply_byname(rt_func, a = a, .FUNdots = list(rowtype = rowtype),
                    rowcoltypes = "none")
}


#' Sets column type for a matrix or a list of matrices
#'
#' This function is a wrapper for `attr()` so that 
#' setting can be accomplished by the pipe operator (`%>%`).
#' Column types are strings stored in the `coltype` attribute.
#' 
#' If `is.null(coltype)`, the coltype attribute is deleted
#' and subsequent calls to `coltype` will return `NULL`.
#'
#' @param a The matrix on which column type is to be set.
#' @param coltype The type of item stored in columns.
#'
#' @return `a` with `coltype` attribute set.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
#' U %>% setcoltype("Industries")
#' # This also works for lists
#' setcoltype(list(U,U), coltype = "Industries")
#' setcoltype(list(U,U), coltype = list("Industries", "Industries"))
#' DF <- data.frame(U = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' setcoltype(DF$U, "Industries")
#' DF <- DF %>% mutate(newcol = setcoltype(U, "Industries"))
#' DF$newcol[[1]]
#' DF$newcol[[2]]
setcoltype <- function(a, coltype){
  ct_func <- function(a, coltype){
    attr(a, "coltype") <- coltype
    return(a)
  }
  unaryapply_byname(ct_func, a = a, .FUNdots = list(coltype = coltype), 
                    rowcoltypes = "none")
}


#' Row type
#'
#' Extracts row type of `a`.
#'
#' @param a The object from which you want to extract row types.
#'
#' @return The row type of `a`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' rowtype(U)
#' # This also works for lists
#' rowtype(list(U,U))
rowtype <- function(a){
  unaryapply_byname(attr, a = a, .FUNdots = list(which = "rowtype"), 
                    rowcoltypes = "none")
}


#' Column type
#'
#' Extracts column type of `a`.
#'
#' @param a The object from which you want to extract column types.
#'
#' @return The column type of `a`.
#' 
#' @export
#'
#' @examples
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' coltype(U)
#' # This also works for lists
#' coltype(list(U,U))
coltype <- function(a){
  unaryapply_byname(attr, a = a, .FUNdots = list(which = "coltype"), 
                    rowcoltypes = "none")
}


#' Select (or de-select) rows of a matrix (or list of matrices) by name
#'
#' Arguments indicate which rows are to be retained and which are to be removed.
#' For maximum flexibility, arguments are extended regex patterns
#' that are matched against row names.
#'
#' If `a` is `NULL`, `NULL` is returned.
#' 
#' Patterns are compared against row names using extended regex.
#' If no row names of `a` match the `retain_pattern`, `NULL` is returned.
#' If no row names of `a` match the `remove_pattern`, `m` is returned.
#' Note that the default `retain_pattern` and `remove_pattern` ("$^") 
#' retain nothing and remove nothing.
#'
#' Retaining rows takes precedence over removing rows, always.
#'
#' Some typical patterns are:
#' \itemize{
#'   \item{"^Electricity$|^Oil$": row names that are EXACTLY "Electricity" or EXACTLY "Oil".}
#'   \item{"^Electricity|^Oil": row names that START WITH "Electricity" or START WITH "Oil".}
#'   \item{"Electricity|Oil": row names that CONTAIN "Electricity" or CONTAIN "Oil" anywhere within them.}
#' }
#'
#' Given a list of row names, a pattern can be constructed easily using `RCLabels::make_or_pattern()`.
#' `RCLabels::make_or_pattern()` escapes regex strings using `Hmisc::escapeRegex()`.
#' This function assumes that `retain_pattern` and `remove_pattern` have already been
#' suitably escaped.
#' 
#' Note that if all rows are removed from `a`, `NULL` is returned.
#'
#' @param a A matrix or a list of matrices.
#' @param retain_pattern An extended regex or list of extended regular expressions that specifies which rows of `a` to retain.
#'                       Default pattern ("$^") retains nothing.
#' @param remove_pattern An extended regex or list of extended regular expressions that specifies which rows of `a` to remove,
#'                       Default pattern ("$^") removes nothing.
#'
#' @return A matrix that is a subset of `m` with rows selected by `retain_pattern` and `remove_pattern`.
#' 
#' @export
#'
#' @examples
#' m <- matrix(1:16, ncol = 4, dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_rows_byname(m, 
#'                    retain_pattern = RCLabels::make_or_pattern(c("i1", "i4"),
#'                    pattern_type = "exact"))
#' select_rows_byname(m, 
#'                    remove_pattern = RCLabels::make_or_pattern(c("i1", "i3"), 
#'                    pattern_type = "exact"))
#' # Also works for lists and data frames
#' select_rows_byname(list(m, m), retain_pattern = "^i1$|^i4$")
select_rows_byname <- function(a, retain_pattern = "$^", remove_pattern = "$^"){
  if (is.null(a)) {
    return(NULL)
  }
  # Note default patterns ("$^") retain nothing and remove nothing,
  # because $ means end of line and ^ means beginning of line.
  # The default pattern would match lines where the beginning of the line is the end of the line.
  # That is impossible, so nothing is matched.
  select_func <- function(a_mat, retain_pattern, remove_pattern){
    retain_indices <- grep(pattern = retain_pattern, x = rownames(a_mat))
    remove_indices <- grep(pattern = remove_pattern, x = rownames(a_mat))
    if (length(retain_indices) == 0) {
      # Nothing to be retained, so try removing columns
      if (length(remove_indices) == 0) {
        # Nothing to be retained and nothing to be removed.
        # If the caller wanted to retain something,
        # which is indicated by a non-default retain_pattern,
        # don't retain anything.
        # Do this first, because retain takes precedence.
        if (retain_pattern != "$^") {
          return(NULL)
        }
        # If the caller wanted to remove something,
        # which is indicated by a non-default remove_pattern,
        # don't remove anything. Simply return a.
        if (remove_pattern != "$^") {
          return(a_mat)
        }
        # Neither retain_pattern nor remove_pattern is different from the default.
        # This is almost surely an error.
        stop("neither retain_pattern nor remove_pattern are different from default.")
      }
      # Remove
      # Check to see if we will remove all rows from a
      rows_remaining <- nrow(a_mat) - length(remove_indices)
      if (rows_remaining <= 0) {
        return(NULL)
      }
      return(a_mat[-remove_indices , ] %>%
               # When only 1 row is selected, the natural result will be a numeric vector
               # We want to ensure that the return value is a matrix
               # with correct rowtype and coltype.
               # Thus, we need to take these additional steps.
               matrix(nrow = rows_remaining,
                      dimnames = list(dimnames(a_mat)[[1]][setdiff(1:nrow(a_mat), remove_indices)],
                                      dimnames(a_mat)[[2]])) %>%
               setrowtype(rowtype(a_mat)) %>% setcoltype(coltype(a_mat))
      )
    }
    # Retain
    out <- a_mat[retain_indices , ]
    if (is.Matrix(a_mat)) {
      out <- matsbyname::Matrix(out, nrow = length(retain_indices), ncol = ncol(a_mat))
    } else {
      out <- matrix(out, nrow = length(retain_indices), ncol = ncol(a_mat))
    }
    dimnames(out) <- list(dimnames(a_mat)[[1]][retain_indices],
                          dimnames(a_mat)[[2]])
    out %>% 
      setrowtype(rowtype(a_mat)) %>% setcoltype(coltype(a_mat))
  }
  unaryapply_byname(select_func, a = a, 
                    .FUNdots = list(retain_pattern = retain_pattern, remove_pattern = remove_pattern), 
                    rowcoltypes = "none")
}


#' Select columns of a matrix (or list of matrices) by name
#'
#' Arguments indicate which columns are to be retained and which are to be removed.
#' For maximum flexibility, arguments are extended regex patterns
#' that are matched against column names.
#'
#' If `a` is `NULL`, `NULL` is returned.
#' 
#' Patterns are compared against column names using extended regex.
#' If no column names of `a` match the `retain_pattern`, `NULL` is returned.
#' If no column names of `a` match the `remove_pattern`, `a` is returned.
#'
#' Retaining columns takes precedence over removing columns, always.
#'
#' Some typical patterns are:
#' \itemize{
#'   \item{"^Electricity$|^Oil$": column names that are EXACTLY "Electricity" or "Oil".}
#'   \item{"^Electricity|^Oil": column names that START WITH "Electricity" or "Oil".}
#'   \item{"Electricity|Oil": column names that CONTAIN "Electricity" or "Oil" anywhere within them.}
#' }
#'
#' Given a list of column names, a pattern can be constructed easily using the `make_pattern` function.
#' 
#' `RCLabels::make_or_pattern()` escapes regex strings using `Hmisc::escaprRegex()`.
#' This function assumes that `retain_pattern` and `remove_pattern` have already been
#' suitably escaped.
#' 
#' Note that the default `retain_pattern` and `remove_pattern` ("$^") 
#' retain nothing and remove nothing.
#' 
#' Note that if all columns are removed from `a`, `NULL` is returned.
#' 
#' @param a a matrix or a list of matrices
#' @param retain_pattern an extended regex or list of extended regular expressions that specifies which columns of `m` to retain.
#' Default pattern ("$^") retains nothing.
#' @param remove_pattern an extended regex or list of extended regular expressions that specifies which columns of `m` to remove.
#' Default pattern ("$^") removes nothing.
#'
#' @return a matrix that is a subset of `a` with columns selected by `retain_pattern` and `remove_pattern`.
#' 
#' @export
#'
#' @examples
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_cols_byname(m, 
#'                    retain_pattern = RCLabels::make_or_pattern(c("p1", "p4"), 
#'                    pattern_type = "exact"))
#' select_cols_byname(m, 
#'                    remove_pattern = RCLabels::make_or_pattern(c("p1", "p3"), 
#'                    pattern_type = "exact"))
#' # Also works for lists and data frames
#' select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$")
select_cols_byname <- function(a, retain_pattern = "$^", remove_pattern = "$^"){
  if (is.null(a)) {
    return(NULL)
  }
  out <- a %>% 
    transpose_byname() %>% 
    select_rows_byname(retain_pattern = retain_pattern, remove_pattern = remove_pattern)
  if (is.null(out)) {
    return(NULL)
  }
  out %>% 
    transpose_byname()
}


#' Select or remove rows or columns based on pieces of the names.
#' 
#' `select_rows_byname()` and `select_cols_byname()`
#' select rows and columns using regex patterns
#' This function performs similar actions
#' based on the pieces of row and column labels.
#' 
#' This function uses the `RCLabels` package to match 
#' row and column names by pieces.
#' 
#' To retain rows or columns, specify `retain`. 
#' To remove rows or columns, specify `remove`.
#' 
#' If `a` has row and column types, a string may be passed to `margin`,
#' in which case the margin will be resolved.
#' See examples.
#' 
#' `notation` may be a list of notations that could apply in `a`. 
#' This function will try to infer the notation that applies
#' to row and column names. 
#' 
#' Retaining takes precedence over removing, always.
#' 
#' Options for `piece` are 
#' 
#' * "all" (the default), meaning that the entire label will be matched,
#' * "pref", meaning that the prefix will be matched,
#' * "suff", meaning that the suffix will be matched,
#' * "noun", meaning that the first part will be matched, and
#' * "from" (or another preposition), meaning that the object of that preposition will be matched.
#'
#' If retaining or removing rows or columns results in no rows or columns remaining
#' in the matrix, `NULL` is returned.
#' 
#' @param a A matrix or list of matrices whose rows or columns are to be selected.
#' @param retain The row or column names to be retained.
#'               Default is `NULL`, meaning that removal is requested.
#' @param remove The row or column names to be removed.
#'               Default is `NULL`, meaning that retaining is requested.
#' @param piece The piece of row or column names to be assessed.
#'              Default is "all", indicating that the entire label will be assessed.
#' @param pattern_type The way to match label pieces.
#'                     `pattern_type` is passed to `RCLabels::make_or_pattern()`.
#'                     See `RCLabels::make_or_pattern()` for details.
#'                     Default is "exact", meaning that exact matches are retained or removed.  
#'                     Other options are "leading", "trailing", "anywhere", and "literal".
#' @param prepositions The prepositions that can be used for identifying pieces.
#'                     Default is `RCLabels::prepositions_list`.
#' @param notation The notation for the row and column names. 
#'                 Default is `RCLabels::notations_list`, meaning that all notations known to 
#'                 `RCLabels` will be assessed.
#' @param margin The margin to which row or column removal is requested.
#'               `1` indicates rows; `2` indicates columns.
#'               Default is `c(1, 2)`, meaning that action should be taken on both rows and columns.
#'
#' @return `a` with rows and/or column retained or removed.
#' 
#' @export
#'
#' @examples
#' m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
#'               dimnames = list(c("r1 [to a]", "r2 [to b]"), 
#'                               c("c1 [from c]", "c2 [from d]"))) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' m
#' select_rowcol_piece_byname(m, retain = "r1", piece = "noun", 
#'                            notation = RCLabels::to_notation, 
#'                            margin = 1)
#' select_rowcol_piece_byname(m, retain = "b", piece = "to", 
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = 1)
#' select_rowcol_piece_byname(m, retain = "c1", piece = "noun",
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = 2)
#' select_rowcol_piece_byname(m, retain = "d", piece = "from", 
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = 2)
#' select_rowcol_piece_byname(m, retain = "c", piece = "from", 
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = 2)
#' select_rowcol_piece_byname(m, retain = "b", piece = "to", 
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = "rows")
#' select_rowcol_piece_byname(m, retain = "c", piece = "from", 
#'                            notation = RCLabels::bracket_notation, 
#'                            margin = "cols")
select_rowcol_piece_byname <- function(a, 
                                       retain = NULL, 
                                       remove = NULL, 
                                       piece = "all",
                                       pattern_type = "exact",
                                       prepositions = RCLabels::prepositions_list, 
                                       notation = RCLabels::notations_list, 
                                       margin = c(1, 2)) {
  if (is.null(a)) {
    return(NULL)
  }
  
  select_func <- function(a_mat) {
    # Decode the margin if margin is a string
    margin <- margin_from_types_byname(a_mat, margin)
    # If we want to select columns, transpose and select rows.
    if (2 %in% margin) {
      # Transpose, perform the selection (or de-selection), and re-transpose
      a_mat <- transpose_byname(a_mat) %>% 
        select_rowcol_piece_byname(retain = retain, remove = remove, piece = piece, 
                                   prepositions = prepositions, notation = notation, margin = 1) %>% 
        # Re-transpose
        transpose_byname()
    }
    if (1 %in% margin) {
      # Get the rownames
      rnames <- getrownames_byname(a_mat)
      # Make the pattern.
      if (!is.null(retain)) {
        keep_pattern <- RCLabels::make_or_pattern(retain, pattern_type = pattern_type)
        # Use RCLabels::match_by_pattern() to do the matching.
        which_to_keep <- RCLabels::match_by_pattern(labels = rnames, 
                                                    regex_pattern = keep_pattern, 
                                                    pieces = piece, 
                                                    prepositions = prepositions,
                                                    notation = notation)
      } else {
        # When retain is NULL, we want to remove
        remove_pattern <- RCLabels::make_or_pattern(remove, pattern_type = pattern_type)
        which_to_remove <- RCLabels::match_by_pattern(labels = rnames, 
                                                      regex_pattern = remove_pattern, 
                                                      pieces = piece, 
                                                      prepositions = prepositions,
                                                      notation = notation)
        which_to_keep <- ! which_to_remove
      }
      # Now keep only the rows that we want, retaining all columns.
      a_mat <- a_mat[which_to_keep, , drop = FALSE]
    }
    if (is.null(a_mat)) {
      return(NULL)
    }
    if (matsbyname::nrow_byname(a_mat) == 0 | matsbyname::ncol_byname(a_mat) == 0) {
      a_mat <- NULL
    }
    return(a_mat)
  }
  
  unaryapply_byname(select_func, a = a)
}


#' Clean (delete) rows or columns of matrices that contain exclusively `clean_value`
#' 
#' Cleaning is performed when all entries in a row or column or both, depending on the value of `margin`,
#' are within `+/- tol` of `clean_value`.
#' Internally, values are deemed within +/- of tol when 
#' `abs(x - clean_value) <= tol`.
#' 
#' If there is concern about machine precision, you might want to call this function with 
#' `tol = .Machine$double.eps`.
#'
#' When a row (when `margin = 1`) or a column (when `margin = 2`)
#' contains exclusively `clean_value` (within `tol`), the row or column is deleted from the matrix.
#'
#' @param a The matrix to be cleaned.
#' @param margin The dimension over which cleaning should occur, `1` for rows, `2` for columns,
#'               or `c(1, 2)` for both rows and columns. 
#'               Default is `c(1, 2)`.
#' @param clean_value The undesirable value. 
#'                    Default is `0`.
#' @param tol The tolerance with which any value is deemed equal to `clean_value`.
#'            Default is `0`.
#'
#' @return A "cleaned" matrix, expunged of rows or columns that contain exclusively `clean_value.`
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(-20, 1, -20, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' m
#' m %>% clean_byname(margin = 1, clean_value = -20) # Eliminates -20, -20 row
#' # Nothing cleaned, because no columns contain all 0's (the default clean_value).
#' m %>% clean_byname(margin = 2) 
#' # Also works with lists
#' list(m, m) %>% clean_byname(margin = 1, clean_value = -20)
#' # Also works with data frames
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' DF %>% clean_byname(margin = 1, clean_value = -20)
#' m2 <- matrix(c(-20, -20, 0, -20, -20, 0, -20, -20, -20), nrow = 3,
#'              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")) )
#' m2
#' clean_byname(m2, margin = c(1,2), clean_value = -20)
#' DF2 <- data.frame(m2 = I(list()))
#' DF2[[1, "m2"]] <- m2
#' DF2[[2, "m2"]] <- m2
#' DF2 %>% clean_byname(margin = c(1, 2), clean_value = -20)
clean_byname <- function(a, margin = c(1, 2), clean_value = 0, tol = 0){
  margin <- prep_vector_arg(a, margin)
  clean_value <- prep_vector_arg(a, clean_value)
  tol = prep_vector_arg(a, tol)
  
  clean_func <- function(a, margin, clean_value, tol){
    assertthat::assert_that(1 %in% margin | 2 %in% margin, msg = paste("margin =", margin, "in clean_byname(). Must be 1 or 2."))
    out <- a
    if (1 %in% margin) {
      # Want to clean rows. Code below assumes want to clean columns.
      # Transpose and then transpose again before returning.
      out <- transpose_byname(out) %>% 
        clean_func(margin = 2, clean_value = clean_value, tol = tol) %>% 
        transpose_byname()
    }
    if (2 %in% margin) {
      keepcols <- apply(out, 2, function(x) {
        # !all(x == clean_value)
        !all(abs(x - clean_value) <= tol)
      })
      out <- out[ , keepcols, drop = FALSE]
    } 
    return(out)
  }
  unaryapply_byname(clean_func, a = a, .FUNdots = list(margin = margin, clean_value = clean_value, tol = tol), 
                    rowcoltypes = "all")
}


#' Test whether this is the zero matrix
#' 
#' Note that this function tests whether the elements of `abs(a)` are `<= tol`.
#' The default value for `tol` is `1e-6`.
#' So, you can set `tol = 0` to discover if `a` is EXACTLY the zero matrix.
#'
#' @param a A matrix or list of matrices.
#' @param tol The allowable deviation from 0 for any element.
#'            Interpreted as an absolute value.
#' 
#' @return `TRUE` Iff this is the zero matrix within `tol`.
#' 
#' @export
#'
#' @examples
#' zero <- matrix(0, nrow = 50, ncol = 50)
#' iszero_byname(zero)
#' nonzero <- matrix(1:4, nrow = 2)
#' iszero_byname(nonzero)
#' # Also works for lists
#' iszero_byname(list(zero, nonzero))
#' # And it works for data frames
#' DF <- data.frame(A = I(list()), B = I(list()))
#' DF[[1,"A"]] <- zero
#' DF[[2,"A"]] <- nonzero
#' DF[[1,"B"]] <- nonzero
#' DF[[2,"B"]] <- zero
#' iszero_byname(DF$A)
#' iszero_byname(DF$B)
#' iszero_byname(matrix(1e-10, nrow = 2))
#' iszero_byname(matrix(1e-10, nrow = 2), tol = 1e-11)
iszero_byname <- function(a, tol = 1e-6) {
  zero_func <- function(a_mat, tol){
    all(abs(a_mat) <= abs(tol))
  }
  unaryapply_byname(zero_func, a = a, .FUNdots = list(tol = tol), 
                    rowcoltypes = "none")
}


#' Select zero rows
#' 
#' Matrices with rows containing all zeroes are not invertible (singular).
#' To diagnose this problem, it is useful to find the zero rows
#' of a singular matrix. 
#' This function selects (extracts) only the zero rows of a matrix.
#' 
#' A row is said to be a zero row if all elements are within `tol` of zero.
#'
#' @param a A matrix or a list of matrices.
#' @param tol The allowable deviation from 0 for any element.
#'
#' @return `a` with only zero rows selected.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(0, 0, 1,
#'               0, 0, 0), 
#'             dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
#'             nrow = 2, ncol = 3, byrow = TRUE) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' m
#' selectzerorows_byname(m)
selectzerorows_byname <- function(a, tol = 1e-6) {
  if (is.null(a)) {
    return(NULL)
  }
  zerorow_func <- function(a_mat, tol_val) {
    zero_rows <- sapply(1:nrow(a_mat), FUN = function(i_row) {
      this_row <- a_mat[i_row, ]
      all(abs(this_row) <= tol_val)
    }) %>% 
      which()
    a_mat[zero_rows, , drop = FALSE]
  }
  unaryapply_byname(zerorow_func, a = a, .FUNdots = list(tol_val = tol))
}


#' Select zero columns
#' 
#' Matrices with columns containing all zeroes are not invertible (singular).
#' To diagnose this problem, it is useful to find the zero columns
#' of a singular matrix. 
#' This function selects (extracts) only the zero columns of a matrix.
#'
#' A column is said to be a zero column if all elements are within `tol` of zero.
#' 
#' @param a A matrix or a list of matrices.
#' @param tol The allowable deviation from 0 for any element.
#'
#' @return `a` with only zero columns selected.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1, 0, 1,
#'               1, 0, 1),
#'             dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
#'             nrow = 2, ncol = 3, byrow = TRUE) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' selectzerocols_byname(m)
selectzerocols_byname <- function(a, tol = 1e-6) {
  if (is.null(a)) {
    return(NULL)
  }
  zerocol_func <- function(a_mat, tol_val) {
    zero_cols <- sapply(1:ncol(a_mat), FUN = function(i_col) {
      this_col <- a_mat[ , i_col]
      all(abs(this_col) <= tol_val)
    }) %>% 
      which()
    a_mat[ , zero_cols, drop = FALSE]
  }
  unaryapply_byname(zerocol_func, a = a, .FUNdots = list(tol_val = tol))
}


#' Names of zero rows and columns
#' 
#' When a matrix has rows or columns full of zeroes, 
#' it is singular, and can't be inverted. 
#' This function returns the names of rows or columns that are full with zeroes.
#'
#' @param a A matrix or list of matrices.
#' @param tol The allowable deviation from 0 for any element.
#'
#' @return A vector of names of zero rows or columns.
#'
#' @export
#'
#' @examples
#' m <- matrix(c(1, 0, 1,
#'               1, 0, 0, 
#'               0, 0, 0),
#'             dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")), 
#'             nrow = 3, ncol = 3, byrow = TRUE)
#' m
#' getzerorowcolnames_byname(m)
getzerorowcolnames_byname <- function(a, tol = 1e-6) {
  zero_row_col_names_func <- function(a_mat, tol_val) {
    zero_rows <- selectzerorows_byname(a_mat, tol = tol_val)
    zero_cols <- selectzerocols_byname(a_mat, tol = tol_val)
    zero_row_names <- getrownames_byname(zero_rows)
    zero_col_names <- getcolnames_byname(zero_cols)
    c(zero_row_names, zero_col_names)
  }
  unaryapply_byname(zero_row_col_names_func, a = a, .FUNdots = list(tol = tol), 
                    rowcoltypes = "none")
}


#' Logarithmic mean of two numbers
#' 
#' Calculates the logarithmic mean of two numbers.
#' 
#' This is an internal helper function for \code{logarithmicmean_byname}.
#'
#' @param a the first operand (must be non-negative)
#' @param b the second operand (must be non-negative)
#' @param base the base of the logarithm used in this calculation. 
#'        (Default is \code{exp(1)}.)
#'
#' @return \code{0} if \code{a = 0} or \code{b = 0}; \code{x1} if \code{a == b}; and
#'         \code{(a - b) / log(a/b, base = base)} 
#'         for all other values of \code{a} and \code{b}
#'         
#' @export
#'
#' @examples
#' matsbyname:::logmean(0, 0) # 0
#' matsbyname:::logmean(0, 1) # 0
#' matsbyname:::logmean(1, 0) # 0
#' matsbyname:::logmean(1, 1) # 1
#' matsbyname:::logmean(2, 1)
#' matsbyname:::logmean(1, 2) # commutative
#' matsbyname:::logmean(1, 10) # base = exp(1), the default
#' matsbyname:::logmean(1, 10, base = 10)
logmean <- function(a, b, base = exp(1)){
  # Take care of pathological cases.  
  # See https://en.wikipedia.org/wiki/Logarithmic_mean for details.
  if (a == 0) {
    return(0)
  }
  if (b == 0) {
    return(0)
  }
  if (a == b) {
    return(a)
  }
  (a - b) / log(a/b, base = base)
}


#' Get the number of rows in a "byname" matrix.
#' 
#' The functionn gets the number of rows in a "byname" matrix, or for each "byname" matrix contained
#' in a column of a data frame.
#'
#' @param a A matrix or a column of a data frame populated with "byname" matrices.
#'
#' @return The number of rows of the matrix, or a list containing the number of rows 
#'         in each of the matrices contained in the column of a data frame.
#' @export
#'
#' @examples
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2", "i3")
#' U2 <- matrix(1:3, ncol = length(industrynames), 
#'              nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' productnames <- c("p1", "p2", "p3")
#' industrynames <- c("i1", "i2", "i3", "i4")
#' U3 <- matrix(1:4, ncol = length(industrynames), 
#'              nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' dfUs <- data.frame(
#'   year = numeric(),
#'   matrix_byname = I(list())
#' )
#' dfUs[[1, "matrix_byname"]] <- U
#' dfUs[[2, "matrix_byname"]] <- U2
#' dfUs[[3, "matrix_byname"]] <- U3
#' dfUs[[1, "year"]] <- 2000
#' dfUs[[2, "year"]] <- 2001
#' dfUs[[3, "year"]] <- 2002
#' number_rows <- matsbyname::nrow_byname(dfUs$matrix_byname)
nrow_byname <- function(a) {
  nrow_func <- function(a) {
    nrow(a)
  }
  unaryapply_byname(nrow_func, a = a, rowcoltypes = "none")
}


#' Get the number of columns in a "byname" matrix.
#'
#' The functionn gets the number of columns in a "byname" matrix, or for each "byname" matrix contained
#' in a column of a data frame.
#'
#'
#' @param a A matrix or a column of a data frame populated with "byname" matrices.
#'
#' @return The number of columns of the matrix, or a list containing the number of columns
#'         in each of the matrices contained in the column of a data frame.
#' @export
#'
#' @examples
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2", "i3")
#' U2 <- matrix(1:3, ncol = length(industrynames), 
#'              nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' productnames <- c("p1", "p2", "p3")
#' industrynames <- c("i1", "i2", "i3", "i4")
#' U3 <- matrix(1:4, ncol = length(industrynames), 
#'              nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% 
#'   setrowtype("Products") %>% 
#'   setcoltype("Industries")
#' dfUs <- data.frame(
#'   year = numeric(),
#'   matrix_byname = I(list())
#' )
#'   dfUs <- data.frame(
#' year = numeric(),
#' matrix_byname = I(list())
#' )
#' dfUs[[1, "matrix_byname"]] <- U
#' dfUs[[2, "matrix_byname"]] <- U2
#' dfUs[[3, "matrix_byname"]] <- U3
#' dfUs[[1, "year"]] <- 2000
#' dfUs[[2, "year"]] <- 2001
#' dfUs[[3, "year"]] <- 2002
#' number_cols <- ncol_byname(dfUs$matrix_byname) %>% 
#' print()
ncol_byname <- function(a) {
  ncol_func <- function(a) {
    ncol(a)
  }
  unaryapply_byname(ncol_func, a = a, rowcoltypes = "none")
}


#' Create a "byname" matrix from a vector
#'
#' This function creates a "byname" matrix, or list of matrices, from `.dat`, 
#' depending on the input arguments.
#' This function is similar to `matrix()`, but with "byname" characteristics.
#' 
#' Row and column names are taken from the `dimnames` argument.
#' 
#' Any row or column type information on `.dat` is preserved on output.
#' 
#' The created object(s) can be of type `base::matrix` or `Matrix::Matrix`,
#' the latter enables sparse objects to save both memory and disk.
#'
#' @param .dat The data to be used to create the matrix, in a list format, or as a data frame column
#'             containing a list of the data to be used for each observation.
#' @param nrow The number of rows to be used to create the matrix, in a list format, or as a data frame column
#'             containing a list of the number of rows to be used for each observation.
#' @param ncol The number of columns to be used to create the matrix, in a list format, or as a data frame column
#'             containing a list of the number of columns to be used for each observation.
#' @param byrow The argument stating whether the matrix should be filled by rows or by columns (FALSE by column, TRUE by row),
#'              in a list format, or as a data frame column containing a list of the byrow argument for each observation.
#'              Default is `FALSE.`
#' @param dimnames The dimension names to be used for creating the matrices, in a list format, or as a data frame column
#'                 containing a list of the dimension names to be used for each observation.
#' @param matrix_class One of "matrix" or "Matrix". 
#'                     "matrix" creates a `base::matrix` object with the `matrix()` function.
#'                     "Matrix" creates a `Matrix::Matrix` object using the `matsbyname::Matrix()` function.
#'                     This could be a sparse matrix.
#'                     Default is "matrix".
#'
#' @return A matrix, list of matrices, or column in a data frame, depending on the input arguments.
#' 
#' @export
#'
#' @examples
#' create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
#'                      dimnames = list(c("r1", "r2"), "c1"))
#' create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
#'                      dimnames = list(list("r1", "c1"), list("R1", "C1")))
create_matrix_byname <- function(.dat, nrow, ncol, byrow = FALSE, dimnames, 
                                 matrix_class = c("matrix", "Matrix")) {
  
  matrix_class <- match.arg(matrix_class)
  matrix_func <- function(a, nrow_val, ncol_val, byrow_val, 
                          dimnames_val, rowtype_val = NA, coltype_val = NA) {
    if (matrix_class == "matrix") {
      return(matrix(a, nrow = nrow_val, ncol = ncol_val, byrow = byrow_val, dimnames = dimnames_val))
    } 
    if (matrix_class == "Matrix") {
      return(matsbyname::Matrix(a, nrow = nrow_val, ncol = ncol_val, byrow = byrow_val, dimnames = dimnames_val))
    }
    
  }
  
  unaryapply_byname(FUN = matrix_func, a = .dat,
                    .FUNdots = list(nrow_val = nrow, ncol_val = ncol, 
                                    byrow_val = byrow,
                                    dimnames_val = dimnames),
                    # Transfer any row or column type information in .dat to the output.
                    rowcoltypes = "all")
}


#' Create row vectors from data
#' 
#' This function takes data in the `.dat` and creates row vectors.
#' 
#' The row and column names in the resulting row vector are taken from 
#' `rowname` and the names of `.dat`.
#' If set, `dimnames` overrides `rowname` and the names of `.dat`.
#' 
#' Row types and column types are taken from the row type and column type attributes of `.dat`.
#' 
#' This function is a "byname" function that can accept a single number,
#' a vector, a list, or a data frame in `.dat`.
#' 
#' @param .dat Data to be converted to row vectors.
#' @param rowname The name of the row of the row vector.
#' @param dimnames The dimension names to be used for creating the row vector, in a list format, or as a data frame column
#'                 containing a list of the dimension names to be used for each observation.
#' @param matrix_class One of "matrix" or "Matrix". 
#'                     "matrix" creates a `base::matrix` object with the `matrix()` function.
#'                     "Matrix" creates a `Matrix::Matrix` object using the `matsbyname::Matrix()` function.
#'                     This could be a sparse matrix.
#'                     Default is "matrix".
#'
#' @return A row vector, a list of row vectors, or a data frame column of row vectors, depending on the 
#'         values of `.dat` and `class`.
#'         
#' @export
#'
#' @examples
#' # Works with single numbers
#' create_rowvec_byname(c(c1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), rowname = "r1")
#' # Works with vectors
#' create_rowvec_byname(c(c1 = 1, c2 = 2), rowname = "r1")
#' # Works with a list
#' create_rowvec_byname(list(c(c1 = 1, c2 = 2), c(C1 = 3, C2 = 4, C3 = 5)), 
#'                      rowname = list("r1", "R1"))
#' # Works in a tibble, too.
#' # (Must be a tibble, not a data frame, so that names are preserved.)
#' dat <- list(c(c1 = 1),
#'             c(C1 = 2, C2 = 3), 
#'             c(c1 = 1, c2 = 2, c3 = 3, c4 = 4, c5 = 5, c6 = 6))
#' rnms <- list("r1", "R1", "r1")
#' df1 <- tibble::tibble(dat, rnms)
#' df1
#' df1 <- df1 %>%
#'   dplyr::mutate(
#'     rowvec_col = create_rowvec_byname(dat, rowname = rnms)
#'   )
#' df1$rowvec_col[[1]]
#' df1$rowvec_col[[2]]
#' df1$rowvec_col[[3]]
create_rowvec_byname <- function(.dat, dimnames = NA, rowname = NA, 
                                 matrix_class = c("matrix", "Matrix")) {
  matrix_class <- match.arg(matrix_class)
  rowvec_func <- function(a, dimnames_val, rowname_val) {

    # Figure out the column names.
    # The dimnames argument overrides any names present in a.
    # So we check here if dimnames has been set.
    # If not, we just take names from a, if available.
    if (any(is.na(dimnames_val))) {
      dimnames_val <- list(rowname_val, names(a))
    }
    # Create the row vector using the rowtype and coltype of a.
    create_matrix_byname(a, nrow = 1, ncol = length(a), dimnames = dimnames_val, matrix_class = matrix_class) 
  }

  unaryapply_byname(FUN = rowvec_func, 
                    a = .dat,
                    .FUNdots = list(dimnames_val = dimnames, rowname_val = rowname), rowcoltypes = "all")
}


#' Create column vectors from data
#' 
#' This function takes data in the `.dat` and creates column vectors.
#' 
#' The row and column names in the resulting column vector are taken from 
#' the names of `.dat` and `colname`.
#' If set, `dimnames` overrides the names of `.dat` and `colname`.
#' 
#' This function is a "byname" function that can accept a single number,
#' a vector, a list, or a data frame in `.dat`.
#' 
#' Row types and column types are taken from the row type and column type attributes of `.dat`.
#' 
#' @param .dat Data to be converted to column vectors.
#' @param colname The name of the column of the colvector.
#' @param dimnames The dimension names to be used for creating the column vector, in a list format, or as a data frame column
#'                 containing a list of the dimension names to be used for each observation.
#' @param matrix_class One of "matrix" or "Matrix". 
#'                     "matrix" creates a `base::matrix` object with the `matrix()` function.
#'                     "Matrix" creates a `Matrix::Matrix` object using the `matsbyname::Matrix()` function.
#'                     This could be a sparse matrix.
#'                     Default is "matrix".
#'
#' @return A column vector, a list of column vectors, or a data frame column of column vectors, depending on the 
#'         value of `.dat` and `class`.
#'         
#' @export
#'
#' @examples
#' # Works with single numbers
#' create_colvec_byname(c(r1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), 
#'                      colname = "r1")
#' # Works with vectors
#' create_colvec_byname(c(r1 = 1, r2 = 2), colname = "c1")
#' # Works with a list
#' create_colvec_byname(list(c(r1 = 1, r2 = 2), c(R1 = 3, R2 = 4, R3 = 5)), 
#'                      colname = list("c1", "C1"))
#' # Works in a tibble, too.
#' # (Must be a tibble, not a data frame, so that names are preserved.)
#' dat <- list(c(r1 = 1, r2 = 2),
#'             c(R1 = 2, R2 = 3), 
#'             c(r1 = 1, r2 = 2, r3 = 3, r4 = 4, r5 = 5, r6 = 6))
#' cnms <- list("c1", "C1", "c1")
#' df1 <- tibble::tibble(dat, cnms)
#' df1
#' df1 <- df1 %>%
#'   dplyr::mutate(
#'     colvec_col = create_colvec_byname(dat, colname = cnms)
#'   )
#' df1$colvec_col[[1]]
#' df1$colvec_col[[2]]
#' df1$colvec_col[[3]]
create_colvec_byname <- function(.dat, dimnames = NA, colname = NA, 
                                 matrix_class = c("matrix", "Matrix")) {
  matrix_class <- match.arg(matrix_class)
  colvec_func <- function(a, dimnames_val, colname_val) {

    # Figure out the row names.
    # The dimnames argument overrides any names present in a.
    # So we check here if dimnames has been set.
    # If not, we just take names from a, if available.
    if (any(is.na(dimnames_val))) {
      dimnames_val <- list(names(a), colname_val)
    }
    # Create the row vector using the rowtype and coltype of a.
    create_matrix_byname(a, nrow = length(a), ncol = 1, dimnames = dimnames_val, matrix_class = matrix_class)
  }

  unaryapply_byname(FUN = colvec_func,
                    a = .dat,
                    .FUNdots = list(dimnames_val = dimnames, colname_val = colname),
                    rowcoltypes = "all")
}



