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
#'
#' @return a \code{mat_byname} object
#' 
#' @export
#'
#' @examples
#' mat_byname(matrix(c(1:2)))
mat_byname <- function(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, 
                       m = matrix(data = data, nrow = nrow, ncol = ncol, byrow = byrow, dimnames = dimnames),
                       rowtype, coltype) {
  if (is.null(m)) {
    return(NULL)
  }
  if (all(is.na(m))) {
    return(NA)
  }
  if (!"matrix" %in% class(m)) {
    stop("A matrix is required in mat_byname.")
  }
  class(m) <- append(class(m), "mat_byname")
  return(m %>% setrowtype(rowtype) %>% setcoltype(coltype))
}