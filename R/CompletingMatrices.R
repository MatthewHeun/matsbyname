#' Complete rows and columns in one matrix relative to another
#' 
#' "Completing" rows and columns means that \code{a} contains a union of rows and columns
#' between \code{a} and \code{m},
#' with missing data represented by the value for \code{fill} (0, by default).
#' 
#' Note that \code{complete_rows_cols(mat1, mat2)} and \code{complete_rows_cols(mat2, mat1)} are 
#' not guaranteed to have the same order for rows and columns.
#' (Nor are the values in the matrix guaranteed to have the same positions.)
#' If \code{dimnames(mat)} is NULL, \code{a} is returned unmodified.
#' If either \code{a} or \code{matrix} are missing names on a margin (row or column),
#' an error is given.
#' Matrices can be completed relative to themselves,
#' meaning that \code{a} will be made square,  
#' containing the union of row and column names from \code{a} itself.
#' All added rows and columns will be created from one of the \code{fill*} arguments.
#' When conflicts arise, precedence among the \code{fill*} arguments is 
#' \code{fillrow} then \code{fillcol} then \code{fill}.
#' Self-completion occurs if \code{a} is non-NULL and 
#' both \code{is.null(matrix)} and \code{is.null(names)}.
#' Under these conditions, no warning is given.
#' If \code{is.null(names)} and dimnames of \code{matrix} cannot be determined
#' (because, for example, \code{matrix} doesn't have any dimnames),
#' \code{a} is completed relative to itself and a warning is given.
#'
#' @param a a matrix or list of matrices to be completed. 
#' @param mat a \code{matrix} from which \code{dimnames} will be extracted
#'        for the purposes of completing \code{a} with respect to \code{mat}.
#' @param fill rows and columns added to \code{a} will contain the value \code{fill}. 
#'        (Default is 0.) 
#' @param fillrow a row vector of type \code{matrix} with same column names as \code{a}. 
#'        Any rows added to \code{a} will be \code{fillrow}.  
#'        If non-\code{NULL}, \code{fillrow} takes precedence over both \code{fillcol} and \code{fill}
#'        in the case of conflicts.
#' @param fillcol a column vector of type \code{matrix} with same row names as \code{a}. 
#'        Any columns added to \code{a} will be \code{fillcol}.  
#'        If non-\code{NULL}, \code{fillcol} takes precedence over \code{fill}
#'        in the case of conflicts.
#' @param margin specifies the subscript(s) in \code{a} over which completion will occur
#'        \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}
#'        For rows only, give \code{1}; 
#'        for columns only, give \code{2};
#'        for both rows and columns, give \code{c(1,2)}, the default value.
#'        
#' @export
#' 
#' @return A modified version of \code{a} possibly containing additional rows and columns 
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
#' complete_rows_cols(m1, m1) # Nothing added, because everything already present
#' complete_rows_cols(m1, t(m1)) # Adds empty c1, c2 rows; Adds empty r1, r2, r3 columns
#' # Same as previous. With missing matrix, complete relative to transpose of m1.
#' complete_rows_cols(m1) 
#' # Adds rows r10, r11; cols c10, c11
#' complete_rows_cols(m1, matrix(0, nrow = 2, ncol = 2, 
#'                               dimnames = list(c("r10", "r11"), c("c10", "c11")))) 
#' # Also works with lists
#' complete_rows_cols(a = list(m1,m1))
#' complete_rows_cols(a = list(m1,m1), mat = list(m2,m2))
#' # No changes because r2, r3 already present in m1
#' complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 1) 
#' complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 2)
#' complete_rows_cols(a = list(m1,m1), 
#'                    mat = make_list(matrix(0, nrow = 2, ncol = 2, 
#'                                           dimnames = list(c("r10", "r11"), c("c10", "c11"))), 
#'                                    n = 2, lenx = 1))
#' # fillrow or fillcol can be specified
#' a <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, 
#'             dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' b <- matrix(c(1:6), byrow = TRUE, nrow = 3, ncol = 2, 
#'             dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
#' fillrow <- matrix(c(31, 32), byrow = TRUE, nrow = 1, ncol = 2, 
#'                   dimnames = list("r42", c("c1", "c2")))
#' complete_rows_cols(a = a, mat = b, fillrow = fillrow)
complete_rows_cols <- function(a = NULL, mat = NULL, fill = 0, 
                               fillrow = NULL, fillcol = NULL, 
                               margin = c(1,2)){
  
  if (is.null(a) & is.null(mat)) {
    stop("Both a and mat are NULL in complete_rows_cols.")
  }
  if (is.data.frame(a)) {
    stop("a cannot be a data frame in complete_rows_cols.")
  }
  if (is.list(a) & !is.data.frame(a) & !is.matrix(a)) {
    # Assume we have a list of matrices for a, not a single matrix.
    # Double-check that we have what we need in the mat argument.
    if (is.null(mat)) {
      # mat is a single NULL value.  But we need a list of
      # NULL values for the Map function.
      # Make a list of same length as the list of a.
      # Each value is NA.
      mat <- make_list(NULL, length(a))
    } else if (is.matrix(mat)) {
      # We have a single matrix for matrix.
      # Duplicate it to be a list with same length as a.
      mat <- make_list(mat, length(a))
    }
  } else if (is.null(a) & is.list(mat) & !is.data.frame(mat) & !is.matrix(mat)) {
    # a is NULL, and assume we have a list of matrices in the mat argument.
    # Under these conditions, we return matrices with same row and column names as each mat, but
    # filled with the "fill" value.
    # For that to work, we need to ensure that each of the other arguments are lists.
    a = make_list(NULL, length(mat))
    margin <- make_list(margin, length(mat), lenx = 1)
  }

  # Double-check that we have what we need for the margin argument.
  margin <- prep_vector_arg(a, margin)
  
  complete_func <- function(a = NULL, mat = NULL, fill, fillrow = NULL, fillcol = NULL, margin){
    # When we get here, we should not have lists for any of the arguments.
    # We should have single matrices for a and/or matrix. 
    
    dimnamesmat <- dimnames(mat)
    
    # Check that fillrow, if present, is appropriate
    if (!is.null(fillrow)) {
      if (!is.matrix(fillrow)) {
        stop("fillrow must be a matrix in complete_rows_cols.")
      }
      if (!nrow(fillrow) == 1) {
        stop("fillrow must be a matrix with one row in complete_rows_cols.")
      }
    }
    
    # Check that fillcol, if present, is appropriate
    if (!is.null(fillcol)) {
      if (!is.matrix(fillcol)) {
        stop("fillcol must be a matrix in complete_rows_cols.")
      }
      if (!ncol(fillcol) == 1) {
        stop("fillcol must be a matrix with one column in complete_rows_cols.")
      }
    }
    
    if (is.null(dimnamesmat)) {
      # dimnamesmat is null, even after trying to gather row and column names from mat.  
      # If a is a matrix with names, complete it relative to itself.
      if (is.matrix(a) & !is.null(dimnames(a))) {
        if (!is.null(mat)) {
          # If we get here but matrix is not NULL, the user was probably trying
          # to complete a relative to matrix.
          # But matrix doens't have any dimnames.
          # Warn that we're going to complete a relative to itself.
          warning("NULL names in complete_rows_cols, despite 'mat' being specified. Completing a relative to itself.")
        }
        return(complete_rows_cols(a, mat = t(a)))
      }
      # a is a matrix without dimnames.  Just return a.
      return(a)
    }
    
    rt <- rowtype(a)
    ct <- coltype(a)
    if ((is.null(a) | (is.matrix(a) & all(dim(a) == 0))) & length(dimnamesmat) == 2) {
      # a is NULL or a 0x0 matrix, but dimnamesmat is a nxn list.
      # We can work with this.
      # If we have fillcol, make a matrix consisting of repeated fillcols 
      # and with row and column names of dimnamesmat.
      if (!is.null(fillcol) & is.null(fillrow)) {
        # Verify that we have the right size.
        if (!isTRUE(all.equal(rownames(fillcol), dimnamesmat[[1]]))) {
          stop("rownames of fillcol must match rownames of mat in complete_rows_cols.")
        }
        return(matrix(rep.int(fillcol, times = ncol(mat)), nrow = nrow(mat), ncol = ncol(mat),
                      dimnames = dimnamesmat) %>% setrowtype(rt) %>% setcoltype(ct))
      }
      if (!is.null(fillrow)) {
        # Verify that we have the right size.
        if (!isTRUE(all.equal(colnames(fillrow), dimnamesmat[[2]]))) {
          stop("colnames of fillrow must match colnames of mat in complete_rows_cols.")
        }
        return(matrix(rep.int(fillrow, times = nrow(mat)), byrow = TRUE, nrow = nrow(mat), ncol = ncol(mat),
                      dimnames = dimnamesmat) %>% setrowtype(rt) %>% setcoltype(ct))
      }
      # Neither fillrow nor fillcol was specified.  
      # Make a matrix from fill and return it.
      return(matrix(fill, nrow = length(dimnamesmat[[1]]), ncol = length(dimnamesmat[[2]]), dimnames = dimnamesmat) %>% 
               setrowtype(rt) %>% setcoltype(ct))
    }
    
    # If we get here, we could have matrix "a" without dimnames and non-NULL dimnames on mat
    # This is a degenerate case.
    # We don't know what row and column names are already present in a.
    # So we can't know how to complete a with the dimnames from mat.
    # Give an error.
    if (is.null(dimnames(a)) & !is.null(dimnamesmat)) {
      stop("Can't complete a that is missing dimnames with non-NULL dimnames on mat.  How can we know which names are already present in a?")
    }
    
    # At this point, we should have 
    #   * a single matrix a,
    #   * names with which to complete the dimnames of a (dimnamesmat),
    #   * a single fill value (in the fill argument) or fillrow or fillcol, and 
    #   * an indication of which margin(s) over which to complete the names (in the margin argument).
    
    for (mar in margin) {
      # Check that row or column names are available for the margin to be completed.
      # If not, this is almost certainly an unintended error by the caller.
      if (is.null(dimnames(a)[[mar]])) {
        stop(paste("NULL dimnames for margin =", mar, "on a"))
      }
    }
    
    # We do margin 2 (columns) first, because margin 1 (rows) should win 
    # if there is a conflict between fillrow and fillcol.
    if (2 %in% margin) {
      fillcolnames <- setdiff(dimnamesmat[[2]], colnames(a))
      if (is.null(fillcol)) {
        # Make fill cols from the fill value.
        fillcols <- matrix(fill, ncol = length(fillcolnames), nrow = nrow(a), 
                           dimnames = list(rownames(a), fillcolnames))
      } else {
        # # fillcol is present. Ensure that row names on fillcol are identical to rownames in a
        # if (!isTRUE(all.equal(rownames(fillcol), rownames(a)))) {
        #   stop("row names of fillcol must match row names of a in complete_rows_cols.")
        # }
        # # Make the fillcols matrix from the fillcol row vector
        # fillcols <- matrix(rep.int(fillcol, length(fillcolnames)), 
        #                    ncol = length(fillcolnames), nrow = nrow(a), 
        #                    dimnames = list(rownames(a), fillcolnames))

        # fillcol is present. Perform some tests.
        # Stop if fillcol or a has any duplicated column names
        if (any(duplicated(rownames(fillcol)))) {
          stop("Duplicated row names found in matrix fillcol in complete_rows_cols.")
        }
        if (any(duplicated(rownames(a)))) {
          stop("Duplicated row names found in matrix a in complete_rows_cols.")
        }
        # Ensure that all of the row names in matrix a are present in fillcol
        if (!all(rownames(a) %in% rownames(fillcol))) {
          stop("Some rows of matrix a are not present in matrix fillcol in complete_rows_cols.")
        }
        # Ensure that the order of rows in fillcol is the same as the order of rows in a.
        fillcol <- fillcol[rownames(a), , drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
        # Make the fillcols matrix from the fillrow row vector
        fillcols <- matrix(rep.int(fillcol, length(fillcolnames)), 
                           ncol = length(fillcolnames), nrow = nrow(a), 
                           dimnames = list(rownames(a), fillcolnames))
      }
      a <- cbind(a, fillcols) %>% 
        setrowtype(rt) %>% setcoltype(ct)
    }

    if (1 %in% margin) {
      fillrownames <- setdiff(dimnamesmat[[1]], rownames(a))
      if (is.null(fillrow)) {
        # Make fill rows from the fill value.
        fillrows <- matrix(fill, nrow = length(fillrownames), ncol = ncol(a), 
                          dimnames = list(fillrownames, colnames(a)))
      } else {
        # fillrow is present. Perform some tests.
        # Stop if fillrow or a has any duplicated column names
        if (any(duplicated(colnames(fillrow)))) {
          stop("Duplicated column names found in matrix fillrow in complete_rows_cols.")
        }
        if (any(duplicated(colnames(a)))) {
          stop("Duplicated column names found in matrix a in complete_rows_cols.")
        }
        # Ensure that all of the column names in matrix a are present in fillrow
        if (!all(colnames(a) %in% colnames(fillrow))) {
          stop("Some columns of matrix a are not present in matrix fillrow in complete_rows_cols.")
        }
        # Ensure that the order of columns in fillrow is the same as the order of columns in a.
        fillrow <- fillrow[ , colnames(a), drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
        # Make the fillrows matrix from the fillrow row vector
        fillrows <- matrix(rep.int(fillrow, length(fillrownames)), byrow = TRUE, 
                           nrow = length(fillrownames), ncol = ncol(a), 
                           dimnames = list(fillrownames, colnames(a)))
      }
      a <- rbind(a, fillrows) %>% 
        setrowtype(rt) %>% setcoltype(ct)
    }
    return(a)
  }
  
  binaryapply_byname(complete_func, a = a, b = mat, .FUNdots = list(fill = fill, 
                                                                    fillrow = fillrow, fillcol = fillcol, 
                                                                    margin = margin), 
                     match_type = "all", set_rowcoltypes = TRUE, .organize = FALSE)
}

#' Sorts rows and columns of a matrix
#' 
#' Checks that row names are unique and that column names are unique.
#' Then, sorts the rows and columns in a way that ensures
#' any other matrix with the same row and column names will have 
#' the same order.
#' 
#' Default sort order is given by \code{base::sort()} with \code{decreasing = FALSE}.
#'
#' @param a a matrix or data frame whose rows and columns are to be sorted
#' @param margin specifies the subscript(s) in \code{a} over which sorting will occur. 
#'        \code{margin} has nearly the same semantic meaning as in \code{\link[base]{apply}}.
#'        For rows only, give \code{1}; 
#'        for columns only, give \code{2};
#'        for both rows and columns, give \code{c(1,2)}, the default value.
#' @param roworder specifies the order for rows with default \code{sort(rownames(a))}. 
#'        If \code{NA} (the default), default sort order is used. 
#'        Unspecified rows are removed from the output, thus providing a way to delete rows from \code{a}.
#'        Extraneous row names (row names in \code{roworder} that do not appear in \code{a}) are ignored.
#' @param colorder specifies the order for rows with default \code{sort(colnames(a))}.
#'        If \code{NA} (the default), default sort order is used. 
#'        Unspecified columns are removed from the output, thus providing a way to delete columns from \code{a}.
#'        Extraneous column names (column names in \code{colorder} that do not appear in \code{a}) are ignored.
#' 
#' @return A modified version of \code{a} with sorted rows and columns
#' 
#' @export
#' 
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
#' sort_rows_cols(a = list(m,m), margin = 1, roworder = list(c("r5", "r3", "r1")))
#' # Columns are sorted as default, because no colorder is given.
#' # roworder is ignored. 
#' sort_rows_cols(a = list(m,m), margin = 2, roworder = list(c("r5", "r3", "r1")))
#' # Both columns and rows sorted, rows by the list, columns in natural order.
#' sort_rows_cols(a = list(m,m), margin = c(1,2), roworder = list(c("r5", "r3", "r1")))
sort_rows_cols <- function(a, margin = c(1,2), roworder = NA, colorder = NA){
  margin <- prep_vector_arg(a, margin)
  
  sort_func <- function(a, margin, roworder, colorder){
    # Gather rowtype and coltype so we can apply those later.
    rt <- rowtype(a)
    ct <- coltype(a)
    if (any(is.na(roworder))) {
      if (!is.null(rownames(a))) {
        roworder <- sort(rownames(a))
      } else {
        # Can't sort on rows, because they are not named.
        if (2 %in% margin) {
          # Remove 1 from margin, if it is there.
          margin <- 2
        } else {
          # Nothing to be done
          return(a)
        }
      }
    }
    if (any(is.na(colorder))) {
      if (!is.null(colnames(a))) {
        colorder <- sort(colnames(a))
      } else {
        # Can't sort on columns, because they are not named.
        if (1 %in% margin) {
          # Remove 2 from margin, if it is there.
          margin <- 1
        } else {
          # Nothing to be done
          return(a)
        }
      }
    }
    if (1 %in% margin & nrow(a) > 1) {
      # Sort rows
      if (length(unique(rownames(a))) != length(rownames(a))) {
        dupes <- rownames(a)[duplicated(rownames(a))] %>% unique()
        stop(paste0("Row names not unique. Duplicated row names are: ", paste0(dupes, collapse = ", ")))
      }
      # Trim items from roworder that do not appear as names in rownames(a)
      roworder <- roworder[roworder %in% rownames(a)]
      a <- a[roworder, , drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
    }
    if (2 %in% margin & ncol(a) > 1) {
      if (length(unique(colnames(a))) != length(colnames(a))) {
        dupes <- colnames(a)[duplicated(colnames(a))] %>% unique()
        stop(paste0("Column names not unique. Duplicated column names are: ", paste0(unique(dupes), collapse = ", ")))
      }
      # Trim items from colorder that do not appear as names in colnames(a)
      colorder <- colorder[colorder %in% colnames(a)]
      a <- a[ , colorder, drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
    }
    return(a %>% setrowtype(rt) %>% setcoltype(ct))
  }
  unaryapply_byname(sort_func, a = a, 
                    .FUNdots = list(margin = margin, roworder = roworder, colorder = colorder))
}

#' Complete matrices relative to one another and sort into same row, column order
#'
#' Completes each matrix relative to each other, thereby assuring that
#' both matrices have same row and column names.
#' Missing rows and columns (relative to the other matrix)
#' are filled with \code{fill}.
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
#' @param a The first matrix
#' @param b The second (optional) matrix.
#' @param fill rows and columns added to \code{a} and \code{b} will contain the value \code{fill}. (a double)
#' @param margin Specifies the dimension(s) of \code{a} and \code{b} over which
#'        completing and sorting will occur
#' @param roworder Specifies a custom ordering for rows of returned matrices.
#'        Unspecified rows are dropped.
#' @param colorder Specifies a custom ordering for columns of returned matrices.
#'        Unspecified columns are dropped.
#'
#' @return A named list containing completed and sorted versions of \code{a} and \code{b}.
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

#' Makes a list of items in x, regardless of x's type
#'
#' Repeats \code{x} as necessary to make \code{n} of them.
#' Does not try to simplify \code{x}.
#'
#' @param x the object to be duplicated
#' @param n the number of times to be duplicated
#' @param lenx the length of item \code{x}. Normally \code{lenx} is taken to be \code{length(x)},
#' but if \code{x} is itself a \code{list}, you may wish for the \code{list} to be duplicated several
#' times. In that case, set \code{lenx = 1}.
#'
#' @return a list of \code{x} duplicated \code{n} times
#'
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
make_list <- function(x, n, lenx = ifelse(is.vector(x), length(x), 1)){
  out <- vector(mode = "list", length = n)
  reptimes <- as.integer(n / lenx)
  if (n %% lenx != 0 & lenx != 1) {
    warning("n not evenly divisible by length(x)")
  }
  if (lenx == 1) {
    return(
      lapply(X = 1:n, FUN = function(i){
        out[[i]] <- x
      })
    )
  }
  if (n < lenx) {
    # Fewer items than length of x is desired
    return(x[[1:n]])
  }
  for (cycle in 1:reptimes) {
    for (xindex in 1:lenx) {
      outindex <- (cycle - 1)*lenx + (xindex)
      out[[outindex]] <- x[[xindex]]
    }
  }
  if (n %% length(x) == 0) {
    # Had an even number of cycles
    return(out)
  }
  for (outindex in (reptimes*lenx + 1):n) {
    xindex <- outindex - reptimes*lenx
    out[[outindex]] <- x[[xindex]]
  }
  return(out)
}