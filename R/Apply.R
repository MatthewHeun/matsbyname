# This file contains functions that apply other functions to 
# matrices or data frames of matrices.

# Note that the directive "@importFrom dplyr mutate" in the unaryapply_byname function
# is meant to eliminate a warning on CRAN, specifically
#   Namespace in Imports field not imported from: ‘dplyr’
#   All declared Imports should be used.

#' Apply a unary function by name
#' 
#' `FUN` is applied to `a` using additional arguments `.FUNdots` to `FUN`.
#' If `a` is a list, the names of `a` are applied to the output.
#' 
#' Note that `.FUNdots` can be a rectangular two-dimensional list of arguments to `FUN`. 
#' If so, `.FUNdots` is interpreted as follows:
#' * The first dimension of `.FUNdots` contains named arguments to `FUN`.
#' * The second dimension of `.FUNdots` contains unique values of the named arguments
#'   to be applied along the list that is `a`.
#'
#' The length of the first dimension of `.FUNdots` is the number of arguments supplied to `FUN`.
#' The length of the second dimension of `.FUNdots` must be equal to the length of `a`.
#' 
#' See `prepare_.FUNdots()` for more details on the `.FUNdots` argument.
#' 
#' Options for the `rowcoltypes` argument are:
#'   * "all": transfer both row and column types of `a` directly to output.
#'   * "transpose": rowtype of `a` becomes coltype of output; coltype of `a` becomes rowtype of output. "transpose" is helpful for `FUN`s that transpose `a` upon output.
#'   * "row": rowtype of `a` becomes both rowtype and coltype of output.
#'   * "col": coltype of `a` becomes both rowtype and coltype of output.
#'   * "none": rowtype and coltype not set by `unaryapply_byname`. Rather, `FUN` will set rowtype and coltype.
#' 
#' Note that `rowcoltypes` should not be a vector or list of strings. 
#' Rather, it should be a single string.
#'
#' @param FUN a unary function to be applied "by name" to `a`.
#' @param a the argument to `FUN`.
#' @param .FUNdots a list of additional named arguments passed to `FUN`.
#' @param rowcoltypes a string that tells how to transfer row and column types of `a` to output. See details.
#'
#' @return the result of applying `FUN` "by name" to `a`.
#' 
#' @export
#' 
#' @importFrom dplyr mutate
#'
#' @examples
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' difference_byname(0, U)
#' unaryapply_byname(`-`, U)
unaryapply_byname <- function(FUN, a, .FUNdots = NULL, 
                              rowcoltypes = c("all", "transpose", "row", "col", "none")){
  
  rowcoltypes <- match.arg(rowcoltypes)
  if (is.null(a)) {
    return(NULL)
  }

  lFUNdots <- prepare_.FUNdots(a, .FUNdots)

  if (is.list(a)) {
    # Now that we have created lFUNdots appropriately, Map to get our result.
    return(Map(unaryapply_byname, list(FUN), a, lFUNdots, rowcoltypes = rowcoltypes) %>%
             # Preserve names of a (if present) in the outgoing list.
             magrittr::set_names(names(a)))
  }
  
  # a is not a list.
  out <- do.call(FUN, c(list(a), lFUNdots))

  if (rowcoltypes == "all") {
    out <- out %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(coltype(a))
  } else if (rowcoltypes == "transpose") {
    out <- out %>%
      setrowtype(coltype(a)) %>%
      setcoltype(rowtype(a))
  } else if (rowcoltypes == "row") {
    out <- out %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(rowtype(a))
  } else if (rowcoltypes == "col") {
    out <- out %>%
      setrowtype(coltype(a)) %>%
      setcoltype(coltype(a))
  } else if (rowcoltypes == "none") {
    # Do nothing. rowtype and coltype should have been set by FUN.
  }
  
  # Check whether row or column types are available
  # before taking action.
  # For speed, get the rowtype directly;
  # using rowtype() results in recursion.
  # rta <- attr(a, which = "rowtype", exact = TRUE)
  # if (!is.null(rta)) {
  #   if (rowcoltypes == "all") {
  #     attr(out, "rowtype") <- rta
  #   } else if (rowcoltypes == "transpose") {
  #     attr(out, "coltype") <- rta
  #   } else if (rowcoltypes == "row") {
  #     attr(out, "rowtype") <- rta
  #     attr(out, "coltype") <- rta
  #   }
  # }
  # For speed, get the coltype directly;
  # using coltype() results in recursion.
  # cta <- attr(a, which = "rowtype", exact = TRUE)
  # if (!is.null(cta)) {
  #   if (rowcoltypes == "all") {
  #     attr(out, "coltype") <- cta
  #   } else if (rowcoltypes == "transpose") {
  #     attr(out, "rowtype") <- cta
  #   } else if (rowcoltypes == "col") {
  #     attr(out, "coltype") <- cta
  #     attr(out, "rowtype") <- cta
  #   }
  # }
  return(out)
}


#' Prepare the `.FUNdots` argument for `*apply_byname` functions.
#' 
#' This is a helper function for the various `*apply_byname` functions.
#' 
#' We have four cases between a and any single item of .FUNdots:
#'   * both a and the item of .FUNdots are lists
#'       - if the item of .FUNdots (a list itself) has length different from 1 or length(a), throw an error
#'       - if the item of .FUNdots (a list itself) has length 1, replicate the single item to be a list of length = length(a)
#'       - if the item of .FUNdots (a list itself) has length = length(a), use the item of .FUNdots as is
#'   * a is a list but the item (argument) of .FUNdots is NOT a list
#'       - if the item of .FUNdots (which is not a list) has length != 1, throw an error, 
#'         because there is ambiguity how the item of .FUNdots should be treated.
#'       - if the item of .FUNdots (which is not a list) has length = 1, replicate that single item to be a list of length = length(a)
#'   * a is NOT a list, but the item of .FUNdots IS a list
#'       - pass the argument along and hope for the best.  This situation is probably an error.  If so, it will become apparent soon.
#'   * neither a nor the item of .FUNdots is a list
#'       - a should have length = 1, but a single matrix reports its length as the number of elements of the matrix.
#'         So, we can't check length in this situation.
#'       - the item of .FUNdots is assumed to have length 1 and passed along
#' 
#' @param a the main argument to an `*apply_byname` function.
#' @param .FUNdots a list of additional arguments to be applied to `FUN` in one of the `*apply_byname` functions.
#'
#' @return a reconfigured version of `.FUNdots`, ready for use by an `*apply_byname` function.
#'
prepare_.FUNdots <- function(a, .FUNdots) {
  
  if (is.null(.FUNdots)) {
    if (is.list(a)) {
      return(RCLabels::make_list(NULL, n = length(a), lenx = 1))
    } else {
      return(NULL)
    }
  }

  assertthat::assert_that(is.list(.FUNdots), msg = ".FUNdots must be a list in prepare_.FUNdots")
  
  for (i in 1:length(.FUNdots)) {
    if (is.list(.FUNdots[[i]])) {
      if (is.list(a)) {
        #'   * both a and the item of .FUNdots are lists
        #'       - if the item of .FUNdots (a list itself) has length different from 1 or length(a), throw an error
        #'       - if the item of .FUNdots (a list itself) has length 1, replicate the single item to be a list of length = length(a)
        #'       - if the item of .FUNdots (a list itself) has length = length(a), use the item of .FUNdots as is
        len <- length(.FUNdots[[i]])
        assertthat::assert_that(len == length(a) | len == 1, 
                                msg = paste0("In prepare_.FUNdots(), when both 'a' and '.FUNdots' are lists, ",
                                             "each top-level argument in .FUNdots ",
                                             "must have length = 1 or length = length(a) (= ",
                                             length(a), "). ",
                                             "Found length = ", 
                                             length(.FUNdots[[i]]), " for argument '", 
                                             names(.FUNdots)[[i]], 
                                             "', which is a list. ", 
                                             "Consider wrapping argument '", 
                                             names(.FUNdots)[[i]], 
                                             "' in a list()."))
        if (len == 1) {
          # Replicate the item of .FUNdots to match length of a.
          .FUNdots[[i]] <- RCLabels::make_list(.FUNdots[[i]][[1]], n = length(a), lenx = 1)
        }
      } else {
        #'   * a is NOT a list, but the item of .FUNdots IS a list
        #'       - pass the argument along and hope for the best.  This situation is probably an error.  
        #'         If so, it will become apparent soon.

        # We do nothing here.
      }
    } else {
      if (is.list(a)) {
        #'   * a is a list but the item (argument) of .FUNdots is NOT a list
        #'     This situation could be ambiguous. 
        #'     Let's say the list of `a` values has length 2, and an argument `margin = c(1, 2)`. 
        #'     Should `margin = 1` be applied to `a[[1]]` and `margin = 2` be applied to `a[[2]]`?
        #'     Or should `margin = c(1, 2)` be applied to both `a[[1]]` and `a[[2]]`?
        #'     This ambiguity should be handled by using the function `prep_vector_arg()`
        #'     within the function that calls `unaryapply_byname()`.
        #'     For an example, see `identize_byname()`.
        #'     When the arguments are coming in from a data frame, there will be no ambiguity,
        #'     but the information will not be coming `.FUNdots[[i]]` as a list.
        #'     Optimizing for the data frame case, 
        #'     this function allows vectors of length equal to the length of the list `a`, 
        #'     interpreting such vectors as applying in sequence to each `a` in turn.
        #'     So the algorithm is as follows:
        #'       - if a non-NULL item of .FUNdots (which is not a list) has
        #'         length other than 1 or length(a), throw an error. 
        #'       - if a non-NULL item of .FUNdots (which is not a list) has length = 1, 
        #'         replicate that single item to be a list of length = length(a).
        #'       - if a non-NULL item of .FUNdots (which is not a list) has length = length(a),
        #'         leave it as-is.
        assertthat::assert_that(is.null(.FUNdots[[i]]) | length(.FUNdots[[i]]) == 1 | length(.FUNdots[[i]]) == length(a), 
                                        msg = paste0("In prepare_.FUNdots(), when 'a' is a list, but an entry in '.FUNdots' is not a list, ",
                                                     "every top-level argument in .FUNdots ",
                                                     "must be NULL or have length = 1 or length = length(a) (= ",
                                                     length(a), "). ",
                                                     "Found length = ", 
                                                     length(.FUNdots[[i]]), " for argument '", 
                                                     names(.FUNdots)[[i]], 
                                                     "', which is not a list. ", 
                                                     "Consider converting argument '",
                                                     names(.FUNdots)[[i]], 
                                                     "' into a list of length 1."))
        if (is.null(.FUNdots[[i]]) | length(.FUNdots[[i]]) == 1) {
          .FUNdots[[i]] <- RCLabels::make_list(.FUNdots[[i]], n = length(a), lenx = 1)
        }
        # Otherwise, do nothing.

      } else {
        #'   * neither a nor the item of .FUNdots is a list
        #'       - a should have length = 1, but a single matrix reports its length as the number of elements of the matrix.
        #'         So, we can't check length in this situation.
        #'       - the item of .FUNdots is assumed to have length 1 and passed along
        
        # We do nothing here.
      }
    }
  }
  if (is.list(a)) {
    # At this point, .FUNdots should be a list that is essentially a column-wise data frame
    # with variables in columns (top level of the list) and observations (different values of the variables) in rows.
    # But we later need an inverted version of this list for the purpose of mapping.
    # I.e., we need top level of the list to be the observations to be mapped 
    # and the second level of the list to be variables for each case.
    # So invert this list using the transpose function.
    return(purrr::transpose(.FUNdots))
  }
  # There is no need to invert the .FUNdots list.
  # In fact, inverting it adds another layer to the list, which we don't want. 
  # So just return it.
  return(.FUNdots)
}


#' Apply a function to an element of a matrix specified by rows and columns
#' 
#' `FUN` is applied to the element of `a` that is specified by `row` and `col`.
#' 
#' `row` and `col` can be any of row or column names or integer indices or a mix of both.
#'
#' @param FUN a unary function to be applied to specified rows and columns of `a`
#' @param a the argument to `FUN`
#' @param row the row name of the element to which `FUN` will be applied
#' @param col the column name of the element to which `FUN` will be applied
#' @param .FUNdots a list of additional arguments to `FUN`. (Default is `NULL`.)
#'
#' @return `a`, after `FUN` has been applied to the element at `row` and `col`
#' 
#' @export
#'
#' @examples
#' divide <- function(x, divisor){
#'   x/divisor
#' }
#' m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' elementapply_byname(divide, a = m, row = 1, col = 1, .FUNdots = list(divisor = 2))
#' elementapply_byname(divide, a = m, row = 1, col = 2, .FUNdots = list(divisor = 10))
#' elementapply_byname(divide, a = m, row = "r2", col = "c2", .FUNdots = list(divisor = 100))
elementapply_byname <- function(FUN, a, row, col, .FUNdots = NULL){
  if (is.null(a)) {
    return(NULL)
  }
  if (is.list(a)) {
    lfun <- replicate(n = length(a), expr = FUN, simplify = FALSE)
    lrow <- RCLabels::make_list(x = row, n = length(a), lenx = 1)
    lcol <- RCLabels::make_list(x = col, n = length(a), lenx = 1)
    lFUNdots <- prepare_.FUNdots(a, .FUNdots)
    return(Map(elementapply_byname, lfun, a, lrow, lcol, lFUNdots) %>% 
             # Preserve names of a (if present) in the outgoing list.
             magrittr::set_names(names(a)))
  }
  out <- a
  out[row, col] <- do.call(FUN, c(list(a[row, col]), .FUNdots))
  if (is.Matrix(out)) {
    # The assignment to out[row, col] strips the rowtype and coltype attributes
    # for Matrix objects.
    out <- out %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  }
  return(out)
}


#' Apply a binary function "by name"
#' 
#' If either `a` or `b` is missing or `NULL`, 
#' `0` is passed to `FUN` in its place.
#' Note that if either `a` and `b` are lists, elements must be named the same.
#' The names of list elements of `a` are applied to the output.
#'
#' @param FUN a binary function to be applied "by name" to `a` and `b`.
#' @param a the first operand for `FUN`.
#' @param b the second operand for `FUN`.
#' @param .FUNdots a list of additional named arguments passed to `FUN.`
#' @param match_type one of "all", "matmult", or "none".
#'        When both `a` and `b` are matrices,
#'        "all" (the default) indicates that
#'        rowtypes of `a` must match rowtypes of `b` and
#'        coltypes of `a` must match coltypes of `b`.
#'        If "matmult",
#'        coltypes of `a` must match rowtypes of `b`.
#'        If "none",
#'        neither coltypes nor rowtypes are checked. 
#' @param set_rowcoltypes tells whether to apply row and column types from `a` and `b`
#'        to the output. 
#'        Set `TRUE` (the default) to apply row and column types to the output.
#'        Set `FALSE`, to *not* apply row and column types to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete `a` and `b` relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        Normally, this should be `TRUE` (the default).
#'        However, if `FUN` takes over this responsibility, set to `FALSE`.
#'
#' @return the result of applying `FUN` "by name" to `a` and `b`.
#' 
#' @export
#'
#' @examples
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' sum_byname(U, Y)
#' binaryapply_byname(`+`, U, Y)
binaryapply_byname <- function(FUN, a, b, .FUNdots = NULL, 
                               match_type = c("all", "matmult", "none"), set_rowcoltypes = TRUE, .organize = TRUE){
  match_type <- match.arg(match_type)
  if (.organize) {
    args <- organize_args(a, b, fill = 0, match_type = match_type)
    a <- args$a
    b <- args$b
  }
  if (is.list(a) & is.list(b)) {
    lfun <- replicate(n = max(length(a), length(b)), expr = FUN, simplify = FALSE)
    lFUNdots <- prepare_.FUNdots(a, .FUNdots)
    return(Map(binaryapply_byname, lfun, a, b, lFUNdots,
                 match_type = match_type, set_rowcoltypes = set_rowcoltypes, .organize = .organize) %>% 
             # If a and b have names, organize_args will have ensured that those names are same.
             # So we can set the names of the outgoing list to the names of a.
             magrittr::set_names(names(a)))
  }
  out <- do.call(FUN, c(list(a), list(b), .FUNdots))
  
  if (set_rowcoltypes) {
    if (match_type == "all") {
      if (!is.null(a)) {
        # Rowtypes and coltypes of a and b are set to a if it exists.
        out <- out %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
      } else if (!is.null(b)) {
        # If a doesn't exist, try setting row and column type to the row and column types of b.
        out <- out %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
      } else {
        # There is no way to set the row and column types. 
        # Throw an error.
        stop("set_rowcoltypes == TRUE, but a and b and NULL. How can we set row and column types from NULL?")
      }
    } else if (match_type == "matmult") {
      # In this case, coltype(a) == rowtype(b) and the result of FUN is
      # a matrix with rowtype == rowtype(a) and coltype == coltype(b).
      out <- out %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(b))
    }
  }
  return(out)
}


#' Apply a function "by name" to any number of operands
#' 
#' Applies `FUN` to all operands in `...`.
#' Other arguments have similar meaning as `binaryapply_byname()`.
#' See details for more information.
#' 
#' If only one `...` argument is supplied, 
#' `FUN` must be capable of handling one argument, and
#' the call is routed to `unaryapply_byname()`.
#' When `set_rowcoltypes` is `TRUE`, 
#' the `rowcoltypes` argument of `unaryapply_byname()` is set to "all", 
#' but when `set_rowcoltypes` is `FALSE`, 
#' the `rowcoltypes` argument of `unaryapply_byname()` is set to "none".
#' If finer control is desired, the caller should use `unaryapply_byname()` directly.
#' If more than one argument is passed in `...`,
#' `FUN` must be a binary function, but its use in by `naryapply_byname()` is "n-ary."
#' Arguments `match_type`, `set_rowcoltypes`, and `.organize`
#' have same meaning as for `binaryapply_byname()`.
#' Thus, all of the operands in `...` must obey the rules of type matching 
#' when `match_type` is `TRUE`.
#' 
#' `naryapply_byname()` and `cumapply_byname()` are similar.
#' Their differences can be described by considering a data frame.
#' `naryapply_byname()` applies `FUN` to several columns (variables) of the data frame.
#' For example, `sum_byname()` applied to several variables gives another column
#' containing the sums across each row of the data frame.
#' `cumapply_byname()` applies `FUN` to successive entries in a single column.
#' For example `sum_byname()` applied to a single column gives the sum of all numbers in that column.
#'
#' @param FUN a binary function to be applied "by name" to all operands in `...`.
#' @param ... the operands for `FUN`.
#' @param .FUNdots a list of additional named arguments passed to `FUN`.
#' @param match_type One of "all", "matmult", or "none".
#'                   When `...` are matrices,
#'                   "all" (the default) indicates that
#'                   rowtypes of all `...` matrices must match and
#'                   coltypes of all `...` matrices must match.
#'                   If "matmult",
#'                   the coltype of the first operand must match the rowtype of the second operand
#'                   for every sequential invocation of `FUN`.
#'                   If "none",
#'                   neither coltypes nor rowtypes are checked by `naryapply_byname()`. 
#' @param set_rowcoltypes Tells whether to apply row and column types from 
#'                        operands in `...` to the output of each sequential invocation of `FUN`. 
#'                        Set `TRUE` (the default) to apply row and column types.
#'                        Set `FALSE`, to *not* apply row and column types to the output.
#' @param .organize A boolean that tells whether or not to automatically 
#'                  complete operands in `...` relative to each other and
#'                  sort the rows and columns of the completed matrices.
#'                  This organizing is done on each sequential invocation of `FUN`.
#'                  Normally, this should be `TRUE` (the default).
#'                  However, if `FUN` takes over this responsibility, set to `FALSE`.
#' @param .summarise A boolean that tells whether this call is considered 
#'                   a summarise operation (like `dplyr::summarise()`).
#'                   Default is `FALSE`.
#'        
#' @return the result of applying `FUN` to all operands in `...`
#' 
#' @export
#'
#' @examples
#' naryapply_byname(FUN = sum_byname, 2, 3)
#' naryapply_byname(FUN = sum_byname, 2, 3, 4, -4, -3, -2)
#' # Routes to unaryapply_byname
#' naryapply_byname(FUN = `^`, list(1,2,3), .FUNdots = list(2))
naryapply_byname <- function(FUN, ..., 
                             .FUNdots = NULL, match_type = c("all", "matmult", "none"), 
                             set_rowcoltypes = TRUE, .organize = TRUE, .summarise = FALSE){
  match_type <- match.arg(match_type)
  dots <- list(...)
  if (.summarise) {
    # Transpose dots
    dots <- purrr::transpose(dots)
  }
  if (length(dots) == 1) {
    # Perform unaryapply
    return(unaryapply_byname(FUN, a = dots[[1]], 
                             .FUNdots = .FUNdots, rowcoltypes = ifelse(set_rowcoltypes, "all", "none")))
  }
  a <- dots[[1]]
  for (i in 2:length(dots)) {
    b <- dots[[i]]
    a <- binaryapply_byname(FUN, a = a, b = b, 
                            .FUNdots = .FUNdots, match_type = match_type, 
                            set_rowcoltypes = set_rowcoltypes, .organize = .organize) 
  }
  return(a)
}


#' Apply a function logically to numbers, matrices, or lists of numbers or matrices
#' 
#' Operands should be logical, although numerical operands are accepted.
#' Numerical operands are interpreted as `0` is `FALSE`, and
#' any other number is `TRUE`.
#' 
#' This function is not exported, 
#' thereby retaining the right to future changes.
#'
#' @param FUN a binary function (that returns logical values) to be applied over operands 
#' @param ... operands; constants, matrices, or lists of matrices
#' @param .FUNdots a list of additional named arguments passed to `FUN`.
#' @param match_type One of "all", "matmult", or "none".
#'                   When `...` are matrices,
#'                   "all" (the default) indicates that
#'                   rowtypes of all `...` matrices must match and
#'                   coltypes of all `...` matrices must match.
#'                   If "matmult",
#'                   the coltype of the first operand must match the rowtype of the second operand
#'                   for every sequential invocation of `FUN`.
#'                   If "none",
#'                   neither coltypes nor rowtypes are checked by `naryapply_byname()`. 
#' @param set_rowcoltypes Tells whether to apply row and column types from 
#'                        operands in `...` to the output of each sequential invocation of `FUN`. 
#'                        Set `TRUE` (the default) to apply row and column types.
#'                        Set `FALSE`, to *not* apply row and column types to the output.
#' @param .organize A boolean that tells whether or not to automatically 
#'                  complete operands in `...` relative to each other and
#'                  sort the rows and columns of the completed matrices.
#'                  This organizing is done on each sequential invocation of `FUN`.
#'                  Normally, this should be `TRUE` (the default).
#'                  However, if `FUN` takes over this responsibility, set to `FALSE`.
#' @param .summarise A boolean that tells whether this call is considered 
#'                   a summarise operation (like `dplyr::summarise()`).
#'                   Default is `FALSE`.
#'        
#' @return the result of `FUN` applied logically to `...`
#'
#' @examples
#' matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, TRUE)
#' matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, FALSE)
naryapplylogical_byname <- function(FUN, ..., 
                                    .FUNdots = NULL, match_type = c("all", "matmult", "none"), 
                                    set_rowcoltypes = TRUE, .organize = TRUE, .summarise = FALSE){
  match_type <- match.arg(match_type)
  dots <- list(...)
  if (.summarise) {
    # Transpose dots
    dots <- purrr::transpose(dots)
  }
  if (length(dots) == 1) {
    return(unaryapply_byname(FUN, a = dots[[1]], .FUNdots = .FUNdots, 
                             rowcoltypes = ifelse(set_rowcoltypes, "all", "none")))
  }
  # Get things started.
  a <- dots[[1]]
  b <- dots[[2]]
  if (.organize) {
    mats <- complete_and_sort(a = a, b = b)
    a <- mats$a
    b <- mats$b
  }
  res <- binaryapply_byname(FUN, a = a, b = b, 
                            .FUNdots = .FUNdots, match_type = match_type, 
                            set_rowcoltypes = set_rowcoltypes, .organize = .organize)
  if (length(dots) > 2) {
    for (i in 2:length(dots)) {
      b <- dots[[i]]
      if (.organize) {
        mats <- complete_and_sort(a = a, b = b)
        a <- mats$a
        b <- mats$b
      }
      res <- binaryapply_byname(FUN, a = a, b = b, 
                                 .FUNdots = .FUNdots, match_type = match_type, 
                                 set_rowcoltypes = set_rowcoltypes, .organize = .organize) %>% 
        and_byname(res)
    }
  }
  return(res)
}


#' Apply a function cumulatively to a list of matrices or numbers
#' 
#' `FUN` must be a binary function that also accepts a single argument.
#' The result is a list with first element `FUN(a[[1]])`.
#' For `i >= 2`, elements are `FUN(a[[i]], out[[i-1]])`,
#' where `out` is the result list.
#' 
#' `naryapply_byname()` and `cumapply_byname()` are similar.
#' Their differences can be described by considering a data frame.
#' `naryapply_byname()` applies `FUN` to several columns (variables) of the data frame.
#' For example, `sum_byname()` applied to several variables gives another column
#' containing the sums across each row of the data frame.
#' `cumapply_byname()` applies `FUN` to successive entries in a single column.
#' For example `sum_byname()` applied to a single column gives the sum of all numbers in that column.
#'
#' @param FUN  the function to be applied
#' @param   a  the list of matrices or numbers to which `FUN` will be applied cumulatively
#'
#' @return a list of same length as `a` 
#'         containing the cumulative application of `FUN` to `a`
#' 
#' @export
#'
#' @examples
#' cumapply_byname(sum, list(1, 2, 3, 4))
#' cumapply_byname(sum_byname, list(1, 2, 3, 4))
#' cumapply_byname(prod, list(1, 2, 3, 4))
#' cumapply_byname(hadamardproduct_byname, list(1, 2, 3, 4))
cumapply_byname <- function(FUN, a){
  # Check for pathological cases.
  if (length(a) == 0) {
    # Nothing to be done here.  Return NULL.
    # Note that length(NULL) == 0, so this tests for m == NULL, too.
    return(NULL)
  }
  if (is_matrix_or_Matrix(a)) {
    # We have a single matrix. Just return it.
    return(FUN(a))
  }
  if (length(a) == 1) {
    # Note that length(NA) == 1, so this test captures cases where m == NA.
    # Nothing to be done.
    return(FUN(a))
  }
  # length(a) > 1
  # Assume we have a list of matrices or numerics
  out <- list()
  out[[1]] <- FUN(a[[1]])
  for (i in 2:length(a)) {
    out[[i]] <- FUN(a[[i]], out[[i - 1]])
  }
  # Preserve names of a in the outgoing list.
  out <- out %>% magrittr::set_names(names(a))
  return(out)
}
