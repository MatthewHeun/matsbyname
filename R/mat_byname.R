# 
# This file contains information about the S3 class matbyname
# 


#' Create a \code{mat_byname} object
#' 
#' \code{m} should be a matrix.
#' 
#' If \code{is.null(m)}, \code{NULL} is returned.
#' If \code{all(is.na(m))}, \code{NA} is returned.
#' If \code{m} is not a \code{matrix}, an error is given.
#'
#' @param m a \code{matrix} object
#' @param data a vector of values for \code{m}
#' @param nrow the number of rows in \code{m}
#' @param ncol the number of columns in \code{m}
#' @param byrow tells whether to fill \code{m} by rows (\code{TRUE}) or columns (\code{FALSE})
#' @param dimnames row and column names for \code{m}
#' @param rowtype the type for rows
#' @param coltype the type for columns
#'
#' @return a \code{mat_byname} object
#' 
#' @export
#'
#' @examples
#' mat_byname(matrix(c(1:2)))
# mat_byname <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, 
#                        m = matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow, dimnames = dimnames),
#                        rowtype = NULL, coltype = NULL) {
#   m %>% setrowtype(rowtype) %>% setcoltype(coltype) %>% as.mat_byname()
# }


#' Test if an argument is a \code{mat_byname}
#'
#' @param x the object to be tested
#'
#' @return \code{TRUE} if \code{x} is a \code{mat_byname}, \code{FALSE} otherwise
#' 
#' @export
#'
#' @examples
#' is.mat_byname(matrix(1:2))
#' is.mat_byname(mat_byname(1:2))
# is.mat_byname <- function(x) {
#   if ("mat_byname" %in% class(x)) {
#     return(TRUE)
#   }
#   return(FALSE)
# }

#' Convert a \code{matrix}-like object to a \code{mat_byname}
#'
#' @param x the matrix-like object to be converted to a \code{mat_byname} object
#'
#' @return the \code{mat_byname} object
#' 
#' @export
#'
#' @examples
#' as.mat_byname(matrix(1:2))
# as.mat_byname <- function(x){
#   if (is.null(x)) {
#     stop("'x' must not be NULL in as.mat_byname.")
#   }
#   # if (!"matrix" %in% class(x)) {
#   #   stop("'x' must be of a matrix type in as.mat_byname.")
#   # }
#   class(x) <- c("mat_byname", class(x))
#   return(x)
# }


# `+.mat_byname` <- function(a, b){
#   # See http://adv-r.had.co.nz/S3.html for details.
#   # See, especially, the "Best practices section at the end.
#   sum_byname(a, b)
# }
