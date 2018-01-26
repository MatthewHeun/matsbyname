library("dplyr")
library("magrittr")
library("parallel")

#' Complete rows and columns in one matrix relative to another
#' 
#' "Complete"-ing rows and columns means that \code{x} contains a union of rows and columns
#' between \code{x} and \code{m},
#' with missing data represented by the value for \code{fill} (0, by default).
#' 
#' Note that \code{complete_rows_cols(mat1, mat2)} and \code{complete_rows_cols(mat2, mat1)} are 
#' not guaranteed to have the same order for rows and columns.
#' (Nor are the values in the matrix guaranteed to have the same positions.)
#' If \code{names} is NULL, \code{x} is returned unmodified.
#' If either \code{x} or \code{matrix} are missing names on a margin (row or column),
#' an error is given.
#' If both \code{is.na(matrix)} and \code{any(is.null(names))},
#' \code{x} will be completed relative to itself. 
#' I.e., \code{x} will be made square, and will contain the union of row and column names from \code{x} itself.
#'
#' @param x a matrix or data frame to be completed
#' @param matrix instead of supplying \code{names} directly, a \code{matrix} can be supplied
#'        from which \code{dimnames} will be extracted
#' @param names the names of rows and columns to be completed in \code{x}. 
#'        \code{names} should be a list of two string vectors;
#'        the containing row names and the second containing column names.
#'        If \code{names} is NULL, \code{dimnames(matrix)} will be used instead.
#' @param fill rows and columns added to \code{x} will contain \code{fill}
#' @param margin specifies the subscript(s) in \code{x} over which completion will occur
#'        \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}
#'        For rows only, give \code{1}; 
#'        for columns only, give \code{2};
#'        for both rows and columns, give \code{c(1,2)}, the default value.
#'        
#' @export
#' 
#' @return A modified version of \code{x} possibly containing additional rows and columns 
#' whose names are obtained from \code{matrix}
#' 
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
complete_rows_cols <- function(x = NULL, matrix = NULL, names = NULL, fill = 0, margin = c(1,2)){
  if (is.null(x) & is.null(names) & is.null(matrix)) {
    stop("All of x, names, and matrix are NULL in complete_rows_cols.")
  }
  # if (is.null(matrix)) {
  #   matrix <- NA
  # }
  # if (any(is.null(names))) {
  #   names <- NA
  # }
  # if (is.null(fill)) {
  #   fill <- NA
  # }
  if (is.list(x) & !is.data.frame(x) & !is.matrix(x)) {
    # Assume we have a list of matrices, not a single matrix, for x.
    # Double-check that we have what we need in the matrix argument.
    # if (any(is.na(matrix))) {
    if (is.na(matrix)) {
      # matrix is a single NA value.  But we need a list of
      # NA values for the Map function.
      # Make a list of same length as the list of x.
      # Each value is NA.
      matrix <- make_list(NA, length(x))
    } else if (is.matrix(matrix)) {
      # We have a single matrix for matrix.
      # Duplicate it to be a list with same length as x.
      matrix <- make_list(matrix, length(x))
    }
    # if (any(is.na(names))) {
    if (is.na(names)) {
      # names is NA.  
      # But we need a list of names that is same length as the list in the x argument
      # for the Map function.
      # Make names into a list of NAs with length the same as the length of x.
      names <- make_list(NA, length(x))
    }
    if (is.na(fill)) {
      # fill is NA.
      # But we need a list of fill that is same length as the list in the x argument
      # for the Map function.
      # Make fill into a list of NAs with length the same as the length of x.
      fill <- make_list(NA, length(x))
    }
    # Ensure that margin is a list with same length as argument x.
    margin <- make_list(margin, length(x))
    # Now we can Map everything!
    return(mcMap(complete_rows_cols, x = x, matrix = matrix, names = names, fill = fill, margin = margin))
  }
  
  # When we get here, we should not have lists for any of the matrices.
  # If present, we should have single matrices for x and matrix. 
  # If present, we should have a list of two items for names.
  
  if (is.null(names)) {
    # When names is NULL, try to gather row and column names from matrix.
    names <- dimnames(matrix)
  }

  if (is.null(names)) {
    # names is null, even after trying to gether row and column names from matrix.  
    # Just return x.
    return(x)
  }
    
  rt <- rowtype(matrix)
  ct <- coltype(matrix)
  if (is.null(x) & length(names) == 2) {
    # x is NULL, names is a nxn list.  Create a fill-matrix with names.
    return(matrix(fill, nrow = length(names[[1]]), ncol = length(names[[2]]), dimnames = names) %>% 
             setrowtype(rt) %>% setcoltype(ct))
  }

  # If we get here, we could have x without dimnames and non-NULL names.
  # This is a degenerate case.
  # We don't know what row and column names are already present in x.
  # So we can't know how to complete x with the items in names.
  # Give an error.
  if (is.null(dimnames(x)) & !is.null(names)) {
    stop("Can't complete x that is missing dimnames with non-NULL names.  How can we know which names are already present in x?")
  }
  
  
  # At this point, we should have 
  #   * a single matrix x,
  #   * names with which to complete the dimnames of x,
  #   * a single fill value, and 
  #   * an indication of which margin(s) over which to complete the names.
  
  for (mar in margin) {
    # Check that row or column names are available for the margin to be completed.
    # If not, this is almost certainly an unintended error by the caller.
    if (is.null(dimnames(x)[[mar]])) {
      stop(paste("NULL dimnames for margin =", mar, "on x"))
    }
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
#' Thereafter, rows and columns of the matrices are sorted
#' such that they are in the same order (by name).
#' To complete rows of \code{m1} relative to columns of \code{m2},
#' set the \code{m2} argument to \code{transpose_byname(m2)}.
#' 
#' \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}.
#' For rows only, give \code{1}; 
#' for columns only, give \code{2};
#' for both rows and columns, give \code{c(1,2)}, the default value.
#' 
#' If only \code{m1} is specified, rows of \code{m1} are completed and sorted 
#' relative to columns of \code{m1}.
#' If neither \code{m1} nor \code{m2} have dimnames, 
#' \code{m1} and \code{m2} are returned unmodified.
#' If only one of \code{m1} or \code{m2} has dimnames, an error is thrown.
#' 
#' @param m1 The first matrix
#' @param m2 The second (optional) matrix.
#' @param margin Specifies the dimension(s) of \code{m1} and \code{m2} over which 
#'        completing and sorting will occur 
#' @param roworder Specifies a custom ordering for rows of returned matrices. 
#'        Unspecified rows are dropped.
#' @param colorder Specifies a custom ordering for columns of returned matrices.
#'        Unspecified columns are dropped.
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
  if (missing(m2)) {
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
  return(list(m1 = m1, m2 = m2))
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