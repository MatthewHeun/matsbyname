

is.matrix_or_Matrix <- function(a) {
  if (is.list(a)) {
    return(sapply(a, FUN = function(this_a){is.matrix(this_a) | inherits(this_a, "Matrix")}))
  }
  return(is.matrix(a) | inherits(a, "Matrix"))
}