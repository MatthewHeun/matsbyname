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
#' @param FUN a unary function to be applied "by name" to \code{a}.
#' @param a the argument to \code{FUN}.
#' @param .FUNdots a list of additional named arguments passed to \code{FUN}.
#' @param rowcoltypes a string that tells how to transfer row and column types of \code{a} to output. 
#'        Options are:
#'        \itemize{
#'          \item{\code{all}: transfer both row and column types of \code{a} directly to output.}
#'          \item{\code{transpose}: rowtype of \code{a} becomes coltype of output;
#'                                  coltype of \code{a} becomes rowtype of output.
#'                                  "transpose" is helpful for \code{FUN}s that transpose 
#'                                  \code{a} upon output.}
#'          \item{\code{row}: rowtype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{col}: coltype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{none}: rowtype and coltype not set by \code{unaryapply_byname}. 
#'                             Rather, \code{FUN} will set rowtype and coltype.}
#'        }
#'
#' @return the result of applying \code{FUN} "by name" to \code{a}.
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
  
  # This is old code that didn't properly handle cases where 
  # we wanted to apply different .FUNdots to each a.
  # Commented on 16 April 2020, after fixing the bug
  # with un-commented code below. 
  # The commented code below can probably be deleted
  # after such time as I think things are working with all 
  # other code that relies on matsbyname.
  
  # rowcoltypes <- match.arg(rowcoltypes)
  # if (is.null(a)) {
  #   return(NULL)
  # }
  # if (is.list(a)) {
  #   lfun <- replicate(n = length(a), expr = FUN, simplify = FALSE)
  #   lFUNdots <- make_list(x = .FUNdots, n = length(a), lenx = 1)  
  #   return(Map(unaryapply_byname, lfun, a, lFUNdots, rowcoltypes = rowcoltypes) %>% 
  #            # Preserve names of a (if present) in the outgoing list.
  #            magrittr::set_names(names(a)))
  # }
  # out <- do.call(FUN, c(list(a), .FUNdots))
  # 
  # if (rowcoltypes == "all") {
  #   out <- out %>%
  #     setrowtype(rowtype(a)) %>%
  #     setcoltype(coltype(a))
  # } else if (rowcoltypes == "transpose") {
  #   out <- out %>%
  #     setrowtype(coltype(a)) %>%
  #     setcoltype(rowtype(a))
  # } else if (rowcoltypes == "row") {
  #   out <- out %>%
  #     setrowtype(rowtype(a)) %>%
  #     setcoltype(rowtype(a))
  # } else if (rowcoltypes == "col") {
  #   out <- out %>%
  #     setrowtype(coltype(a)) %>%
  #     setcoltype(coltype(a))
  # } else if (rowcoltypes == "none") {
  #   # Do nothing. rowtype and coltype should have been set by FUN.
  # }
  # return(out)
  
  rowcoltypes <- match.arg(rowcoltypes)
  if (is.null(a)) {
    return(NULL)
  }

  lFUNdots <- NULL
  if (is.list(a)) {
    if (is.list(.FUNdots)) {
      # If .FUNdots has 2 levels of lists, the caller has specified arguments for each and every a in a's list.
      if (is.list(.FUNdots[[1]])) {
        # # Do a recursive descent into .FUNdots to look at lengths for each item
        # arg_lengths <- rapply(.FUNdots, length, how = "list")
        
        # The "second level" of .FUNdots is (potentially) a list of values for each argument to FUN. 
        # If the number of values for each argument to FUN matches the length of a, 
        # it is likely that the caller wants each of these items applied to FUN via a mapping.
        # Figure out the structure of the sizes of the arguments supplied in .FUNdots..
        arg_lengths <- list()
        for (i in 1:length(.FUNdots)) {
          arg_lengths[[i]] <- length(.FUNdots[[i]])
        }
        
        if (all(unlist(arg_lengths) == 1)) {
          # Replicate the values to apply along a
          for (i in 1:length(.FUNdots)) {
            .FUNdots[[i]] <- rep.int(.FUNdots[[i]], times = length(a))
            # Recalculate arg lengths so that we can drop into the next if statement.
            arg_lengths[[i]] <- length(.FUNdots[[i]])
          }
        }

        if (all(unlist(arg_lengths) == length(a))) {
          # Likely want to apply .FUNdots to each of the items in a.
          # We need a slightly different structure for .FUNdots.
          # To get this new structure, we'll first make a data frame
          # in which each row is a set of arguments to FUN.
          # Then, we'll pull each row individually
          # for each call to FUN.
          # To start, make an empty data frame with the same number of rows as we have elements in a.
          DF <- data.frame()[1:length(a), ]
          # Fill the data frame by columns with each argument in .FUNdots
          for (i in 1:length(.FUNdots)) {
            DF[[i]] <- I(.FUNdots[[i]])
          }
          # Set data frame columns to be same as the argument names in .FUNdots.
          DF <- DF %>% magrittr::set_names(names(.FUNdots))
          # Eliminate row names
          rownames(DF) <- NULL
          # Build a list of rows, each of which will be a set of arguments for one call to FUN.
          lFUNdots <- list()
          for (i in 1:nrow(DF)) {
            lFUNdots[[i]] <- DF[i, ] %>% 
              # Transfer names from the columns of DF to each set of arguments
              magrittr::set_names(names(DF))
          }
        } else {
          # a and .FUNdots are lists.
          # But the structure of .FUNdots doesn't match expectations
          # (i.e., length of each item in .FUNdots is neither 1 nor length(a))
          # and .FUNdots does not consist of single-value arguments.
          # So we don't quite know what to do here.
          # Throw an error.
          stop("when .FUNdots is a list, each item (argument) must have length 1 or length(a)")
        }
      }
    }
    if (is.null(lFUNdots)) {
      # .FUNdots is not a list OR .FUNdots is a list but isn't a 2-level list.
      # Replicate .FUNdots to equal length(a)
      lFUNdots <- make_list(x = .FUNdots, n = length(a), lenx = 1)  
    }
    # Now that we have created lFUNdots appropriately, Map to get our result.
    return(Map(unaryapply_byname, list(FUN), a, lFUNdots, rowcoltypes = rowcoltypes) %>%
             # Preserve names of a (if present) in the outgoing list.
             magrittr::set_names(names(a)))
  }
  
  # a is not a list.
  out <- do.call(FUN, c(list(a), .FUNdots))

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
  return(out)
}


#' Apply a function to an element of a matrix specified by rows and columns
#' 
#' \code{FUN} is applied to the element of \code{a} that is 
#' 
#' \code{row} and \code{col} can be any of row or column names or integer indices or a mix of both.
#'
#' @param FUN a unary function to be applied to specified rows and columns of \code{a}
#' @param a the argument to \code{FUN}
#' @param row the row name of the element to which \code{FUN} will be applied
#' @param col the column name of the element to which \code{FUN} will be applied
#' @param .FUNdots a list of additional arguments to \code{FUN}. (Default is \code{NULL}.)
#'
#' @return \code{a}, after \code{FUN} has been applied to the element at \code{row} and \code{col}
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
    lrow <- make_list(x = row, n = length(a), lenx = 1)
    lcol <- make_list(x = col, n = length(a), lenx = 1)
    lFUNdots <- make_list(x = .FUNdots, n = length(a), lenx = 1)  
    return(Map(elementapply_byname, lfun, a, lrow, lcol, lFUNdots) %>% 
             # Preserve names of a (if present) in the outgoing list.
             magrittr::set_names(names(a)))
  }
  out <- a
  out[row, col] <- do.call(FUN, c(list(a[row, col]), .FUNdots))
  return(out)
}

#' Apply a binary function "by name"
#' 
#' If either \code{a} or \code{b} is missing or \code{NULL}, 
#' \code{0} is passed to \code{FUN} in its place.
#' Note that if either \code{a} and \code{b} are lists, elements must be named the same.
#' The names of list elements of \code{a} are applied to the output.
#'
#' @param FUN a binary function to be applied "by name" to \code{a} and \code{b}.
#' @param a the first operand for \code{FUN}.
#' @param b the second operand for \code{FUN}.
#' @param .FUNdots a list of additional named arguments passed to \code{FUN}.
#' @param match_type one of "\code{all}", "\code{matmult}", or "\code{none}".
#'        When both \code{a} and \code{b} are matrices,
#'        "\code{all}" (the default) indicates that
#'        rowtypes of \code{a} must match rowtypes of \code{b} and
#'        coltypes of \code{a} must match coltypes of \code{b}.
#'        If "\code{matmult}",
#'        coltypes of \code{a} must match rowtypes of \code{b}.
#'        If "\code{none}",
#'        neither coltypes nor rowtypes are checked. 
#' @param set_rowcoltypes tells whether to apply row and column types from \code{a} and \code{b}
#'        to the output. 
#'        Set \code{TRUE} (the default) to apply row and column types to the output.
#'        Set \code{FALSE}, to \emph{not} apply row and column types to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete \code{a} and \code{b} relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        Normally, this should be \code{TRUE} (the default).
#'        However, if \code{FUN} takes over this responsibility, set to \code{FALSE}.
#'
#' @return the result of applying \code{FUN} "by name" to \code{a} and \code{b}.
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
    lFUNdots <- make_list(x = .FUNdots, n = max(length(a), length(b)), lenx = 1)
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
#' Applies \code{FUN} to all operands in \code{...}.
#' Other arguments have similar meaning as \code{\link{binaryapply_byname}}.
#' See details for more information.
#' 
#' If only one \code{...} argument is supplied, 
#' \code{FUN} must be capable of handling one argument, and
#' the call is routed to \code{\link{unaryapply_byname}}.
#' When \code{set_rolcoltypes} is \code{TRUE}, 
#' the \code{rowcoltypes} argument of \code{\link{unaryapply_byname}} is set to "\code{all}", 
#' but when \code{set_rowcoltypes} is \code{FALSE}, 
#' the \code{rowcoltypes} argument of \code{\link{unaryapply_byname}} is set to "\code{none}".
#' If finer control is desired, the caller should use \code{\link{unaryapply_byname}} directly.
#' If more than one argument is passed in \code{...},
#' \code{FUN} must be a binary function, but its use in by \code{\link{naryapply_byname}} is "n-ary."
#' Arguments \code{match_type}, \code{set_rowcoltypes}, and \code{.organize}
#' have same meaning as for \code{\link[matsbyname]{binaryapply_byname}}.
#' Thus, all of the operands in \code{...} must obey the rules of type matching 
#' when \code{match_type} is \code{TRUE}.
#' 
#' \code{\link{naryapply_byname}} and \code{\link{cumapply_byname}} are similar.
#' Their differences can be described by considering a data frame.
#' \code{\link{naryapply_byname}} applies \code{FUN} to several columns (variables) of the data frame.
#' For example, \code{\link{sum_byname}} applied to several variables gives another column
#' containing the sums across each row of the data frame.
#' \code{\link{cumapply_byname}} applies \code{FUN} to successive entries in a single column.
#' For example \code{\link{sum_byname}} applied to a single column gives the sum of all numbers in that column.
#'
#' @param FUN a binary function to be applied "by name" to all operands in \code{...}.
#' @param ... the operands for \code{FUN}.
#' @param .FUNdots a list of additional named arguments passed to \code{FUN}.
#' @param match_type one of "\code{all}", "\code{matmult}", or "\code{none}".
#'        When \code{...} are matrices,
#'        "\code{all}" (the default) indicates that
#'        rowtypes of all \code{...} matrices must match and
#'        coltypes of all \code{...} matrices must match.
#'        If "\code{matmult}",
#'        the coltype of the first operand must match the rowtype of the second operand
#'        for every sequential invocation of \code{FUN}.
#'        If "\code{none}",
#'        neither coltypes nor rowtypes are checked by \code{\link{naryapply_byname}}. 
#' @param set_rowcoltypes tells whether to apply row and column types from 
#'        operands in \code{...} to the output of each sequential invocation of \code{FUN}. 
#'        Set \code{TRUE} (the default) to apply row and column types.
#'        Set \code{FALSE}, to \emph{not} apply row and column types to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete operands in \code{...} relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        This organizing is done on each sequential invocation of \code{FUN}.
#'        Normally, this should be \code{TRUE} (the default).
#'        However, if \code{FUN} takes over this responsibility, set to \code{FALSE}.
#'        
#' @return the result of applying \code{FUN} to all operands in \code{...}
#' 
#' @export
#'
#' @examples
#' naryapply_byname(FUN = sum_byname, 2, 3)
#' naryapply_byname(FUN = sum_byname, 2, 3, 4, -4, -3, -2)
#' # Routes to unaryapply_byname
#' naryapply_byname(FUN = `^`, list(1,2,3), .FUNdots = 2)
naryapply_byname <- function(FUN, ..., 
                             .FUNdots = NULL, match_type = c("all", "matmult", "none"), 
                             set_rowcoltypes = TRUE, .organize = TRUE){
  match_type <- match.arg(match_type)
  dots <- list(...)
  if (length(dots) == 1) {
    # Perform a unaryapply
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
#' Numerical operands are interpreted as \code{0} is \code{FALSE}, and
#' any other number is \code{TRUE}.
#' 
#' This function is not exported, 
#' thereby retaining the right to future changes.
#'
#' @param FUN a binary function (that returns logical values) to be applied over operands 
#' @param ... operands; constants, matrices, or lists of matrices
#' @param .FUNdots a list of additional named arguments passed to \code{FUN}.
#' @param match_type one of "\code{all}", "\code{matmult}", or "\code{none}".
#'        When \code{...} are matrices,
#'        "\code{all}" (the default) indicates that
#'        rowtypes of all \code{...} matrices must match and
#'        coltypes of all \code{...} matrices must match.
#'        If "\code{matmult}",
#'        the coltype of the first operand must match the rowtype of the second operand
#'        for every sequential invocation of \code{FUN}.
#'        If "\code{none}",
#'        neither coltypes nor rowtypes are checked by \code{\link{naryapply_byname}}. 
#' @param set_rowcoltypes tells whether to apply row and column types from 
#'        operands in \code{...} to the output of each sequential invocation of \code{FUN}. 
#'        Set \code{TRUE} (the default) to apply row and column types.
#'        Set \code{FALSE}, to \emph{not} apply row and column types to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete operands in \code{...} relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        This organizing is done on each sequential invocation of \code{FUN}.
#'        Normally, this should be \code{TRUE} (the default).
#'        However, if \code{FUN} takes over this responsibility, set to \code{FALSE}.
#'        
#' @return the result of \code{FUN} applied logically to \code{...}
#'
#' @examples
#' matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, TRUE)
#' matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, FALSE)
naryapplylogical_byname <- function(FUN, ..., 
                                    .FUNdots = NULL, match_type = c("all", "matmult", "none"), 
                                    set_rowcoltypes = TRUE, .organize = TRUE){
  match_type <- match.arg(match_type)
  dots <- list(...)
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
#' \code{FUN} must be a binary function that also accepts a single argument.
#' The result is a list with first element \code{FUN(a[[1]])}.
#' For \code{i >= 2}, elements are \code{FUN(a[[i]], out[[i-1]])},
#' where \code{out} is the result list.
#' 
#' \code{\link{naryapply_byname}} and \code{\link{cumapply_byname}} are similar.
#' Their differences can be described by considering a data frame.
#' \code{\link{naryapply_byname}} applies \code{FUN} to several columns (variables) of the data frame.
#' For example, \code{\link{sum_byname}} applied to several variables gives another column
#' containing the sums across each row of the data frame.
#' \code{\link{cumapply_byname}} applies \code{FUN} to successive entries in a single column.
#' For example \code{\link{sum_byname}} applied to a single column gives the sum of all numbers in that column.
#'
#' @param FUN  the function to be applied
#' @param   a  the list of matrices or numbers to which \code{FUN} will be applied cumulatively
#'
#' @return a list of same length as \code{a} 
#'         containing the cumulative application of \code{FUN} to \code{a}
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
  if (is.matrix(a)) {
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
