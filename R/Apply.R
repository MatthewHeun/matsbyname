# This file contains functions that apply other functions to 
# matrices or data frames of matrices

#' Apply a unary function "by name"
#' 
#' Note that if \code{a} is a list, the names of \code{a} are applied to the output.
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
#' @examples
#' library(magrittr)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' difference_byname(0, U)
#' unaryapply_byname(`-`, U)
unaryapply_byname <- function(FUN, a, .FUNdots = NULL, 
                              rowcoltypes = c("all", "transpose", "row", "col", "none")){
  rowcoltypes <- match.arg(rowcoltypes)
  if (is.list(a)) {
    lfun <- replicate(n = length(a), expr = FUN, simplify = FALSE)
    lFUNdots <- make_list(x = .FUNdots, n = length(a), lenx = 1)  
    return(Map(unaryapply_byname, lfun, a, lFUNdots, rowcoltypes = rowcoltypes) %>% 
             # Preserve names of a (if present) in the outgoing list.
             set_names(names(a)))
  }
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
  } else {
    stop(paste("Unknown rowcoltypes argument in unaryapply_byname:", rowcoltypes))
  }
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
#' library(magrittr)
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
             set_names(names(a)))
  }
  out <- do.call(FUN, c(list(a), list(b), .FUNdots))
  
  if (set_rowcoltypes) {
    # Either match_type is 
    #   "all" 
    #     in which case rowtype(a) == rowtype(b) and coltype(a) == coltype(b), 
    #     and setting rowtype to rowtype(a) and coltype to coltype(b) works fine, or
    #   "matmult"
    #     in which case coltype(a) == rowtype(b) and the result of FUN is
    #     a matrix with rowtype == rowtype(a) and coltype == coltype(b).
    # Given those options, we set rowtype to rowtype(a) and coltype to coltype(b).
    return(out %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(b)))
  }
  return(out)
}


#' Apply a function "by name" to any number of operands
#' 
#' \code{FUN} must be a binary function.
#' Arguments \code{match_type}, \code{set_rowcoltypes}, and \code{.organize}
#' have same meaning as for \code{\link[matsbyname]{binaryapply_byname}}.
#' Thus, all of the operands in \code{...} must obey the rules of type matching 
#' given by \code{match_type}.
#' 
#' \code{\link{apply_byname}} and \code{\link{cumapply_byname}} are similar.
#' Their differences can be described by considering a data frame.
#' \code{\link{apply_byname}} applies \code{FUN} to several columns (variables) of the data frame.
#' For example, \code{\link{sum_byname}} applied to several variables gives another column
#' containing the sums across each row of the data frame.
#' \code{\link{cumapply_byname}} applies \code{FUN} to successive entries in a single column.
#' For example \code{\link{sum_byname}} applied to a single column gives the sum of all numbers in that column.
#'
#' @param FUN a binary function to be applied "by name" sequentially to \code{...}.
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
#'        neither coltypes nor rowtypes are checked by \code{\link{apply_byname}}. 
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
#' @return the result of applying \code{FUN} sequantially to each operand in \code{...}
#' 
#' @export
#'
#' @examples
apply_byname <- function(FUN, ..., .FUNdots = NULL, 
                         match_type = c("all", "matmult", "none"), set_rowcoltypes = TRUE, .organize = TRUE){
  if (length(list(...)) < 2) {
    stop("Must have at least 2 arguments in ... for apply_byname.")
  }
  dots <- list(...)
  a <- dots[[1]]
  for (i in 2:length(dots)) {
    b <- dots[[i]]
    a <- binaryapply_byname(FUN, a = a, b = b, .FUNdots = .FUNdots, match_type = match_type, set_rowcoltypes = set_rowcoltypes, .organize = .organize) 
  }
  return(a)
}

#' Apply a function cumulatively to a list of matrices or numbers
#' 
#' \code{FUN} must be a binary function that also accepts a single argument.
#' The result is a list with first element \code{FUN(a[[1]])}.
#' For \code{i >= 2}, elements are \code{FUN(a[[i]], out[[i-1]])},
#' where \code{out} is the result list.
#' 
#' #' \code{\link{apply_byname}} and \code{\link{cumapply_byname}} are similar.
#' Their differences can be described by considering a data frame.
#' \code{\link{apply_byname}} applies \code{FUN} to several columns (variables) of the data frame.
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
#' cumapply_byname(elementproduct_byname, list(1, 2, 3, 4))
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
  out <- out %>% set_names(names(a))
  return(out)
}
