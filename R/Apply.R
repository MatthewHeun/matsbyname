# This file contains functions that apply other functions to 
# matrices or data frames of matrices

#' Apply a unary function byname
#'
#' @param FUN a unary function to be applied "byname" to \code{a}.
#' @param a the argument to \code{FUN}.
#' @param ... other arguments to be passed to \code{FUN}.
#' @param rowcoltypes a string that tells how to transfer row and column types of \code{a} to output. 
#'        Options are:
#'        \itemize{
#'          \item{\code{all}: transfer both row and column types of \code{a} directly to output.}
#'          \item{\code{transpose}: rowtype of \code{a} becomes coltype of output;
#'                                  coltype of \code{a} becomes rowtype of output.}
#'          \item{\code{row}: rowtype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{col}: coltype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{none}: rowtype and coltype not set by this function. 
#'                             Rather, \code{FUN} will rowtype and coltype.}
#'        }
#'
#' @return the result of applying \code{FUN} "byname" to \code{a}.
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
unaryapply_byname <- function(FUN, a, ..., rowcoltypes = c("all", "transpose", "row", "col", "none")){
  rowcoltypes <- match.arg(rowcoltypes)
  if (is.list(a)) {
    return(Map(unaryapply_byname, make_list(FUN, n = length(a)), a, ..., rowcoltypes = rowcoltypes))
  }
  if (length(list(...)) == 0) {
    out <- FUN(a)
  } else {
    out <- FUN(a, ...)
  }
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

#' Apply a binary function byname
#' 
#' If either \code{a} or \code{b} is missing or \code{NULL}, 
#' \code{0} is passed to \code{FUN} in its place.
#'
#' @param FUN a binary function to be applied "byname" to \code{a} and \code{b}.
#' @param a the first argument to \code{FUN}.
#' @param b the second argument to \code{FUN}.
#' @param ... additional named arguments passed to \code{FUN}.
#' @param match_type one of "\code{all}", "\code{matmult}", or "\code{none}".
#'        When both \code{a} and \code{b} are matrices,
#'        "\code{all}" (the default) indicates that
#'        rowtypes of \code{a} must match rowtypes of \code{b} and
#'        coltypes of \code{a} must match coltypes of \code{b}.
#'        If "\code{matmult}",
#'        coltypes of \code{a} must match rowtypes of \code{b}.
#'        If "\code{none}",
#'        neither coltypes nor rowtypes are checked. 
#' @param rowcoltypes tells whether to apply row and column types from \code{a} and \code{b}
#'        to the output. 
#'        The default (\code{TRUE}) means that row and column types are applied to the output.
#'        If \code{FALSE}, row and column types are \emph{not} applied to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete \code{a} and \code{b} relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        Normally, this should be \code{TRUE} (the default).
#'
#' @return the result of applying \code{FUN} "byname" to \code{a} and \code{b}.
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
binaryapply_byname <- function(FUN, a, b, ..., 
                               match_type = c("all", "matmult", "none"), rowcoltypes = TRUE, .organize = TRUE){
  match_type <- match.arg(match_type)
  if (.organize) {
    args <- organize_args(a, b, fill = 0, match_type = match_type)
    a <- args$a
    b <- args$b
  }
  if (is.list(a) & is.list(b)) {
    return(Map(binaryapply_byname, make_list(FUN, n = max(length(a), length(b))), 
               a, b, ..., match_type = match_type, rowcoltypes = rowcoltypes, .organize = .organize))
  }
  if (length(list(...)) == 0) {
    out <- FUN(a, b)
  } else {
    out <- FUN(a, b, ...)
  }
  # if (match_type %in% c("all", "matmult")) {
  if (rowcoltypes) {
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
