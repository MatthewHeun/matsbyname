

#' Complete matrices relative to one another and sort into same row, column order
#'
#' Completes each matrix relative to each other, thereby assuring that
#' both matrices have same row and column names.
#' Missing rows and columns (relative to the other matrix)
#' are filled with `fill`.
#' Thereafter, rows and columns of the matrices are sorted
#' such that they are in the same order (by name).
#' To complete rows of `a` relative to columns of `b`
#' (and vice versa),
#' [transpose_byname()] the `b` argument.
#'
#' `margin` has nearly the same semantic meaning as in [apply()].
#' For rows only, give `1`;
#' for columns only, give `2`;
#' for both rows and columns, give `c(1,2)`, the default value.
#'
#' If only `a` is specified, rows of `a` are completed and sorted
#' relative to columns of `a`.
#' If neither `a` nor `b` have dimnames,
#' `a` and `b` are returned unmodified.
#' If only one of `a` or `b` has dimnames, an error is thrown.
#'
#' @param a The first matrix
#' @param b The second (optional) matrix.
#' @param fill rows and columns added to `a` and `b` will contain the value `fill` (a double).
#' @param margin Specifies the dimension(s) of `a` and `b` over which
#'        completing and sorting will occur
#' @param roworder Specifies a custom ordering for rows of returned matrices.
#'        Unspecified rows are dropped.
#' @param colorder Specifies a custom ordering for columns of returned matrices.
#'        Unspecified columns are dropped.
#'
#' @return A named list containing completed and sorted versions of `a` and `b`.
#'
#' @export
#'
#' @examples
#' m1 <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
#' m2 <- matrix(c(7:12), ncol=3, dimnames = list(c("r3", "r4"), c("c2", "c3", "c4")))
#' complete_and_sort(m1)
#' complete_and_sort(m1, m2)
#' complete_and_sort(m1, m2, roworder = c("r3", "r2", "r1"))
#' complete_and_sort(m1, m2, colorder = c("c4", "c3")) # Drops un-specified columns
#' complete_and_sort(m1, m2, margin = 1)
#' complete_and_sort(m1, m2, margin = 2)
#' complete_and_sort(m1, t(m2))
#' complete_and_sort(m1, t(m2), margin = 1)
#' complete_and_sort(m1, t(m2), margin = 2)
#' v <- matrix(1:6, ncol=2, dimnames=list(c("r3", "r1", "r2"), c("c2", "c1")))
#' complete_and_sort(v, v)
#' # Also works with lists
#' complete_and_sort(list(m1,m1), list(m2,m2))
complete_and_sort <- function(a, b, fill = 0, margin = c(1,2), roworder = NA, colorder = NA){
  margin <- prep_vector_arg(a, margin)
  if (missing(b)) {
    a <- complete_rows_cols(a, fill = fill, margin = margin)
    a <- sort_rows_cols(a, roworder = roworder, colorder = colorder)
    return(a)
  }
  a <- complete_rows_cols(a, b, fill = fill, margin = margin) %>%
    sort_rows_cols(margin = margin, roworder = roworder, colorder = colorder)
  b <- complete_rows_cols(b, a, fill = fill, margin = margin) %>%
    sort_rows_cols(margin = margin, roworder = roworder, colorder = colorder)
  return(list(a = a, b = b))
}
