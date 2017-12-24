library("dplyr")
library("magrittr")
library("parallel")

#' Complete rows and columns in one matrix relative to another
#' 
#' "Complete"-ing rows and columns means that \code{x} contains a union of rows and columns
#' between \code{x} and \code{m},
#' with missing data represented by the value for \code{fill} (0, by default).
#' Note that \code{complete_rows_cols(mat1, mat2)} and \code{complete_rows_cols(mat2, mat1)} are 
#' not guaranteed to have the same order for rows and columns.
#' (Nor are the values in the matrix guaranteed to have the same positions, of course.)
#' If either \code{x} or \code{matrix} are missing names on a margin (row or column),
#' an error is given.
#' If both \code{matrix} and \code{names} are missing,
#' \code{x} will be completed relative to itself. 
#' I.e., \code{x} will be made square, and will contain the union of row and column names from \code{x} itself.
#' If \code{matrix} is \code{NULL} (the default), 
#' \code{x} will be completed relative to itself.
#'
#' @param x a matrix or data frame to be completed
#' @param matrix instead of supplying \code{names} directly, a \code{matrix} can be supplied
#' from which \code{dimnames} will be extracted
#' @param names the names of rows and columns to be completed in \code{x}, with 
#' the same structure as the value of \code{dimnames(matrix)}
#' @param fill rows and columns added to \code{x} will contain \code{fill}
#' @param margin specifies the subscript(s) in \code{x} over which completion will occur
#' \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}
#' For rows only, give \code{1}; 
#' for columns only, give \code{2};
#' for both rows and columns, give \code{c(1,2)}, the default value.
#' @export
#' @return A modified version of \code{x} possibly containing additional rows and columns 
#' whose names are obtained from \code{matrix}
#' @examples
#' m1 <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
#' m2 <- matrix(c(7:12), ncol=3, dimnames = list(c("r2", "r3"), c("c2", "c3", "c4")))
#' complete_rows_cols(m1, m2) # Adds empty column c4
#' complete_rows_cols(m1, t(m2)) # Creates r2, r3 columns; c2, c3, c4 rows
#' complete_rows_cols(m1, m2, margin = 1) # No changes because r2 and r3 already present in m1
#' complete_rows_cols(m1, m2, margin = 2) # Adds empty columns c3 and c4
#' complete_rows_cols(m1, t(m2), margin = 1) # Adds empty rows c2, c3, c4
#' complete_rows_cols(m1, m2, fill = 100) # Adds columns c3 and c4 with 100's
#' complete_rows_cols(data.frame(m1), data.frame(m2)) # Also works with data frames
#' complete_rows_cols(m1, m1) # Nothing added, because everything already present
#' complete_rows_cols(m1, t(m1)) # Adds empty c1, c2 rows; Adds empty r1, r2, r3 columns
#' # Same as previous. With missing matrix, complete relative to transpose of x.
#' complete_rows_cols(m1) 
#' # Adds rows r10, r11; cols c10, c11
#' complete_rows_cols(m1, names = list(c("r10", "r11"), c("c10", "c11"))) 
#' # Also works with lists
#' complete_rows_cols(x = list(m1,m1))
#' complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2))
#' # No changes because r2, r3 already present in m1
#' complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2), margin = 1) 
#' complete_rows_cols(x = list(m1,m1), matrix = list(m2,m2), margin = 2)
#' complete_rows_cols(x = list(m1,m1), 
#'                    names = make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2, lenx = 1))
complete_rows_cols <- function(x, matrix = NA, names = dimnames(matrix), fill = 0, margin = c(1,2)){
  if (is.null(matrix)) {
    matrix <- NA
  }
  if (any(is.null(names))) {
    names <- NA
  }
  if (is.null(fill)) {
    fill <- NA
  }
  if (is.list(x) & !is.data.frame(x)) {
    if (any(is.na(matrix))) {
      matrix <- make_list(NA, length(x))
    }
    if (any(is.na(names))) {
      names <- lapply(matrix, dimnames)
    }
    if (is.na(fill)) {
      fill <- make_list(NA, length(x))
    }
    margin <- make_list(margin, length(x))
    return(mcMap(complete_rows_cols, x = x, matrix = matrix, names = names, fill = fill, margin = margin))
  }
  for (mar in margin) {
    # Check that row or column names are available for the margin to be completed.
    # If not, this is almost certainly an unintended error by the caller.
    if (is.null(dimnames(x)[[mar]])) {
      stop(paste("NULL dimnames for margin =", mar, "on x"))
    }
  }
  if (any(is.na(names))) {
    # No other names were provided, nor was there a matrix from which names could be extracted.
    # Instead, complete x relative to itself. The easy way is to complete x relative to its transpose.
    return(complete_rows_cols(x, t(x), margin = margin))
  }
  if (1 %in% margin) {
    zerorownames <- setdiff(names[[1]], rownames(x))
    zerorows <- matrix(fill, ncol = ncol(x), nrow = length(zerorownames), 
                       dimnames = list(zerorownames, colnames(x)))
    x <- rbind(x, zerorows)
  }
  if (2 %in% margin) {
    zerocolnames <- setdiff(names[[2]], colnames(x))
    zerocols <- matrix(fill, ncol = length(zerocolnames), nrow = nrow(x), 
                       dimnames = list(rownames(x), zerocolnames))
    x <- cbind(x, zerocols)
  }
  return(x)
}

#' Sorts rows and columns of a matrix
#' 
#' Checks that row names are unique and that column names are also unique.
#' Then, sorts the rows and columns in a way that ensures
#' any other matrix with the same row and column names will have 
#' the same order.
#'
#' @param x a matrix or data frame whose rows and columns are to be sorted
#' @param margin specifies the subscript(s) in \code{x} over which sorting will occur. 
#' \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}.
#' For rows only, give \code{1}; 
#' for columns only, give \code{2};
#' for both rows and columns, give \code{c(1,2)}, the default value.
#' @param roworder specifies the order for rows with default sort(rownames(x)). 
#' If \code{NULL}, default is used. Unspecified rows are dropped.
#' @param colorder specifies the order for rows with default sort(colnames(x))
#' If \code{NULL}, default is used. Unspecified columns are dropped.
#' @return A modified version of \code{x} with sorted rows and columns
#' @export
#' @examples
#' m <- matrix(c(1:6), nrow=3, dimnames = list(c("r3", "r5", "r1"), c("c4", "c2")))
#' sort_rows_cols(m)
#' sort_rows_cols(t(m))
#' sort_rows_cols(m, margin=1) # Sorts rows
#' sort_rows_cols(m, margin=2) # Sorts columns
#' v <- matrix(c(1:5), ncol=1, dimnames=list(rev(paste0("r", 1:5)), "c1")) # Column vector
#' sort_rows_cols(v)
#' sort_rows_cols(v, margin = 1) # Sorts rows
#' sort_rows_cols(v, margin = 2) # No effect: only one column
#' r <- matrix(c(1:4), nrow=1, dimnames=list("r1", rev(paste0("c", 1:4)))) # Row vector
#' sort_rows_cols(r) # Sorts columns
#' n <- matrix(c(1,2), nrow = 1, dimnames = list(NULL, c("c2", "c1"))) # No row name
#' sort_rows_cols(n) # Sorts columns, because only one row.
#' # Also works with lists
#' sort_rows_cols(list(m,m)) # Sorts rows and columns for both m's.
#' # Sort rows only for first one, sort rows and columns for second one.  
#' # Row order is applied to all m's.  Column order is natural.
#' sort_rows_cols(x = list(m,m), margin = list(1, c(1,2)), roworder = c("r5", "r3", "r1")) 
#' # Different roworders given for each m.
#' sort_rows_cols(x = list(m,m), 
#'                margin = list(1, c(1,2)), 
#'                roworder = list(c("r1", "r3", "r5"), c("r5", "r3", "r1"))) 
sort_rows_cols <- function(x, margin=c(1,2), roworder = NA, colorder = NA){
  if (is.list(x) & !is.data.frame(x)) {
    margin <- make_list(margin, n = length(x))
    roworder <- make_list(roworder, n = length(x))
    colorder <- make_list(colorder, n = length(x))
    return(mcMap(sort_rows_cols, x = x, margin = margin, roworder = roworder, colorder = colorder))
  }
  if (any(is.na(roworder))) {
    if (!is.null(rownames(x))) {
      roworder <- sort(rownames(x))
    } else {
      # Can't sort on rows, because they are not named.
      if (2 %in% margin) {
        # Remove 1 from margin, if it is there.
        margin <- 2
      } else {
        # Nothing to be done
        return(x)
      }
    }
  }
  if (any(is.na(colorder))) {
    if (! is.null(colnames(x))) {
      colorder <- sort(colnames(x))
    } else {
      # Can't sort on columns, because they are not named.
      if (1 %in% margin){
        # Remove 2 from margin, if it is there.
        margin <- 1
      } else {
        # Nothing to be done
        return(x)
      }
    }
  }
  if (1 %in% margin & nrow(x) > 1){
    # Sort rows
    if (length(unique(rownames(x))) != length(rownames(x))){
      stop("Row names not unique.")
    }
    x <- x[roworder, , drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
  }
  if (2 %in% margin & ncol(x) > 1){
    if (length(unique(colnames(x))) != length(colnames(x))){
      stop("Column names not unique.")
    }
    x <- x[ , colorder, drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
  }
  return(x)
}

#' Complete matrices relative to one another and sort into same row, column order
#'
#' Completes each matrix relative to each other, thereby assuring that 
#' both matrices have same row and column names. 
#' Missing rows and columns (relative to the other matrix)
#' are filled with zeroes.
#' Thereafter, sorts the rows and columns of the matrices
#' such that they are in the same order (by name).
#' To complete rows of \code{m1} relative to columns of \code{m2},
#' set the \code{m2} argument to \code{transpose_byname(m2)}.
#' 
#' \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}.
#' For rows only, give \code{1}; 
#' for columns only, give \code{2};
#' for both rows and columns, give \code{c(1,2)}, the default value.
#' @param m1 The first matrix
#' @param m2 The second matrix
#' @param margin Specifies the dimension(s) of \code{m1} and \code{m2} over which completing and sorting will occur 
#' @param roworder Specifies a custom ordering for rows of returned matrices. 
#' Unspecified rows are dropped.
#' @param colorder Specifies a custom ordering for columns of returned matrices.
#' Unspecified columns are dropped.
#'
#' @return A named list containing completed and sorted versions of \code{m1} and \code{m2}.
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
complete_and_sort <- function(m1, m2, margin=c(1,2), roworder = NA, colorder = NA){
  if (missing(m2)){
    m1 <- complete_rows_cols(m1, margin = margin)
    m1 <- sort_rows_cols(m1, roworder = roworder, colorder = colorder)
    return(m1)
  }
  m1 <- m1 %>% 
    complete_rows_cols(m2, margin = margin) %>% 
    sort_rows_cols(margin = margin, roworder = roworder, colorder = colorder)
  m2 <- m2 %>% 
    complete_rows_cols(m1, margin = margin) %>% 
    sort_rows_cols(margin = margin, roworder = roworder, colorder = colorder)
  return(list(m1=m1, m2=m2))
}

#' Makes a list of items in x, regardless of x's type
#' 
#' Repeats \code{x} as necessary to make \code{n} of them.
#' Does not try to simplify \code{x}.
#'
#' @param x the object to be duplicated
#' @param n the number of times to be duplicated
#' @param lenx the length of item \code{x}. Normally \code{lenx} is taken from \code{length(x)},
#' but if \code{x} is itself a \code{list}, you may wish for the \code{list} to be duplicated several
#' times. In that case, set \code{lenx = 1}.
#'
#' @return a list of \code{x} duplicated \code{n} times
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
#' make_list(m, n = 1)
#' make_list(m, n = 2)
#' make_list(m, n = 5)
#' make_list(list(c(1,2), c(1,2)), n = 4)
#' m <- matrix(1:4, nrow = 2)
#' l <- list(m, m+100)
#' make_list(l, n = 4)
#' make_list(l, n = 1) # Warning because l is trimmed.
#' make_list(l, n = 5) # Warning because length(l) (i.e., 2) not evenly divisible by 5
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2) # Confused by x being a list
#' make_list(list(c("r10", "r11"), c("c10", "c11")), n = 2, lenx = 1) # Fix by setting lenx = 1
make_list <- function(x, n, lenx = ifelse(is.list(x), length(x), 1)){
  out <- vector(mode="list", length=n)
  reptimes <- as.integer(n / lenx)
  if (n %% lenx != 0 & lenx != 1){
    warning("n not evenly divisible by length(x)")
  }
  if (lenx == 1){
    return(
      lapply(X = 1:n, FUN = function(i){
        out[[i]] <- x
      })
    )
  }
  if (n < lenx){
    # Fewer items than length of x is desired
    return(x[[1:n]])
  }
  for (cycle in 1:reptimes){
    for (xindex in 1:lenx){
      outindex <- (cycle-1)*lenx + (xindex)
      out[[outindex]] <- x[[xindex]]
    }
  }
  if (n %% length(x) == 0){
    # Had an even number of cycles
    return(out)
  }
  for (outindex in (reptimes*lenx + 1):n){
    xindex <- outindex - reptimes*lenx
    out[[outindex]] <- x[[xindex]]
  }
  return(out)
}