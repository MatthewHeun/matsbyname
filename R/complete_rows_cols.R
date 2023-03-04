#' Complete rows and columns in one matrix relative to another
#' 
#' "Completing" rows and columns means that `a` contains a union of rows and columns
#' between `a` and `mat`,
#' with missing data represented by the value for `fill` (`0`, by default), 
#' `fillrow`, or `fillcol`.
#' 
#' Note that `complete_rows_cols(mat1, mat2)` and `complete_rows_cols(mat2, mat1)` are 
#' not guaranteed to have the same order for rows and columns.
#' (Nor are the values in the matrix guaranteed to have the same positions.)
#' 
#' If `dimnames(mat)` is `NULL`, `a` is returned unmodified.
#' 
#' If either `a` or `mat` are missing names on a margin (row or column),
#' an error is given.
#' 
#' When `a` is non-`NULL`, 
#' `a` is named, and `mat` is `NULL` (the default),
#' `a` is completed relative to itself,
#' meaning that `a` will be made square,  
#' containing the union of row and column names from `a`.
#' Under these conditions, no warning is given.
#' 
#' If `mat` is non-`NULL` and dimnames of `mat` cannot be determined
#' (because, for example, `mat` doesn't have dimnames),
#' `a` is completed relative to itself and a warning is given.
#' 
#' All added rows and columns will be created from one of the `fill*` arguments.
#' When conflicts arise, precedence among the `fill*` arguments is 
#' `fillrow` then `fillcol` then `fill`.
#'
#' @param a A `matrix` or list of `matrix` objects to be completed. 
#'          `a` can be `Matrix` objects, too.
#' @param mat A `matrix` or `Matrix` from which dimnames will be extracted
#'            for the purposes of completing `a` with respect to `mat`.
#' @param fill Rows and columns added to `a` will contain the value `fill`. 
#'             (Default is `0`.) 
#' @param fillrow A row vector of type `matrix` with same column names as `a`. 
#'                Any rows added to `a` will be `fillrow`.  
#'                If non-`NULL`, `fillrow` takes precedence over both `fillcol` and `fill`
#'                in the case of conflicts.
#' @param fillcol A column vector of type matrix with same row names as `a`. 
#'                Any columns added to `a` will be `fillcol`.  
#'                If non-`NULL`, `fillcol` takes precedence over `fill`
#'                in the case of conflicts.
#' @param margin Specifies the subscript(s) in `a` over which completion will occur
#'               `margin` has nearly the same semantic meaning as in `base::apply()`
#'               For rows only, give `1`; 
#'               for columns only, give `2`;
#'               for both rows and columns, give `c(1,2)`, the default value.
#'        
#' @export
#' 
#' @return A modified version of `a` possibly containing additional rows and columns 
#'         whose names are obtained from `mat` and whose values are obtained from
#'         `fillrow`, `fillcol` or `fill` (in that order of preference).
#' 
#' @examples
#' m1 <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
#' m1
#' m2 <- matrix(c(7:12), ncol=3, dimnames = list(c("r2", "r3"), c("c2", "c3", "c4")))
#' m2
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
#'                    mat = RCLabels::make_list(matrix(0,
#'                                                     nrow = 2, 
#'                                                     ncol = 2, 
#'                                                     dimnames = list(c("r10", "r11"), 
#'                                                                     c("c10", "c11"))), 
#'                                              n = 2, lenx = 1))
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
    stop("Both a and mat are NULL in complete_rows_cols().")
  }
  if (is.data.frame(a)) {
    stop("a cannot be a data frame in complete_rows_cols().")
  }
  if (is.list(a) & !is.data.frame(a) & !is_matrix_or_Matrix(a)) {
    # Assume we have a list of matrices for a, not a single matrix.
    # Double-check that we have what we need in the mat argument.
    if (is.null(mat)) {
      # mat is a single NULL value.  But we need a list of
      # NULL values for the Map function.
      # Make a list of same length as the list of a.
      # Each value is NA.
      mat <- RCLabels::make_list(NULL, length(a))
    } else if (is_matrix_or_Matrix(mat)) {
      # We have a single matrix for matrix.
      # Duplicate it to be a list with same length as a.
      mat <- RCLabels::make_list(mat, length(a))
    }
  } else if (is.null(a) & is.list(mat) & !is.data.frame(mat) & !is_matrix_or_Matrix(mat)) {
    # a is NULL, and assume we have a list of matrices in the mat argument.
    # Under these conditions, we return matrices with same row and column names as each mat, but
    # filled with the "fill" value.
    # For that to work, we need to ensure that each of the other arguments are lists.
    a = RCLabels::make_list(NULL, length(mat))
    margin <- RCLabels::make_list(margin, length(mat), lenx = 1)
  }
  
  # Double-check that we have what we need for the margin argument.
  margin <- prep_vector_arg(a, margin)
  
  complete_func <- function(a = NULL, mat = NULL, fill, fillrow = NULL, fillcol = NULL, margin){
    # When we get here, we should not have lists for any of the arguments.
    # We should have single matrices for a and/or matrix. 
    
    dimnamesa <- dimnames(a)
    # Sometimes, a Matrix will have a list for dimnames,
    # but both components are NULL.
    # Take care of that situation by NULLing dimnamesmat.
    if (all(sapply(dimnamesa, is.null))) {
      dimnamesa <- NULL
    }
    
    dimnamesmat <- dimnames(mat)
    # Sometimes, a Matrix will have a list for dimnames,
    # but both components are NULL.
    # Take care of that situation by NULLing dimnamesmat.
    if (all(sapply(dimnamesmat, is.null))) {
      dimnamesmat <- NULL
    }
    
    # Check that fillrow, if present, is appropriate
    if (!is.null(fillrow)) {
      if (!is_matrix_or_Matrix(fillrow)) {
        stop("fillrow must be a matrix or a Matrix in complete_rows_cols.")
      }
      if (!nrow(fillrow) == 1) {
        stop("fillrow must be a matrix or a Matrix with one row in complete_rows_cols().")
      }
    }
    
    # Check that fillcol, if present, is appropriate
    if (!is.null(fillcol)) {
      if (!is_matrix_or_Matrix(fillcol)) {
        stop("fillcol must be a matrix or a Matrix in complete_rows_cols().")
      }
      if (!ncol(fillcol) == 1) {
        stop("fillcol must be a matrix or a Matrix with one column in complete_rows_cols().")
      }
    }
    
    if (is.null(dimnamesmat)) {
      # dimnamesmat is null, even after trying to gather row and column names from mat.  
      # If a is a matrix or Matrix with names, complete it relative to itself.
      if (is_matrix_or_Matrix(a) & !is.null(dimnamesa)) {
        if (!is.null(mat)) {
          # If we get here but matrix is not NULL, the user was probably trying
          # to complete a relative to matrix or a Matrix.
          # But the matrix doesn't have any dimnames.
          # Warn that we're going to complete a relative to itself.
          warning("NULL names in complete_rows_cols(), despite 'mat' being specified. Completing a relative to itself.")
        }
        return(complete_rows_cols(a, mat = t_matrix_or_Matrix(a)))
      }
      # a is a matrix without dimnames.  Just return a.
      return(a)
    }
    
    rt <- rowtype(a)
    ct <- coltype(a)
    if ((is.null(a) | (is_matrix_or_Matrix(a) & all(dim(a) == 0))) & length(dimnamesmat) == 2) {
      # a is NULL or a 0x0 matrix, but dimnamesmat is a nxn list.
      # We can work with this.
      # If we have fillcol, make a matrix consisting of repeated fillcols 
      # and with row and column names of dimnamesmat.
      if (!is.null(fillcol) & is.null(fillrow)) {
        # Verify that we have the right size.
        if (!isTRUE(all.equal(rownames(fillcol), dimnamesmat[[1]]))) {
          stop("rownames of fillcol must match rownames of mat in complete_rows_cols().")
        }
        return(matrix(rep.int(fillcol, times = ncol(mat)), nrow = nrow(mat), ncol = ncol(mat),
                      dimnames = dimnamesmat) %>% 
                 setrowtype(rt) %>% setcoltype(ct))
      }
      if (!is.null(fillrow)) {
        # Verify that we have the right size.
        if (!isTRUE(all.equal(colnames(fillrow), dimnamesmat[[2]]))) {
          stop("colnames of fillrow must match colnames of mat in complete_rows_cols().")
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
    if (is.null(dimnamesa) & !is.null(dimnamesmat)) {
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
      if (is.null(dimnamesa[[mar]])) {
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
        # fillcol is present. Perform some tests.
        # Stop if fillcol or a has any duplicated column names
        if (any(duplicated(rownames(fillcol)))) {
          stop("Duplicated row names found in matrix fillcol in complete_rows_cols().")
        }
        if (any(duplicated(rownames(a)))) {
          stop("Duplicated row names found in matrix a in complete_rows_cols().")
        }
        # Ensure that all of the row names in matrix a are present in fillcol
        if (!all(rownames(a) %in% rownames(fillcol))) {
          stop("Some rows of matrix a are not present in matrix fillcol in complete_rows_cols().")
        }
        # Ensure that the order of rows in fillcol is the same as the order of rows in a.
        fillcol <- fillcol[rownames(a), , drop = FALSE] # drop = FALSE prevents unhelpful conversion to numeric
        # Make the fillcols matrix from the fillrow row vector
        fillcols <- matrix(rep.int(fillcol, length(fillcolnames)), 
                           ncol = length(fillcolnames), nrow = nrow(a), 
                           dimnames = list(rownames(a), fillcolnames))
      }
      a <- cbind_matrix_or_Matrix(a, fillcols) %>% 
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
      a <- rbind_matrix_or_Matrix(a, fillrows) %>% 
        setrowtype(rt) %>% setcoltype(ct)
    }
    return(a)
  }
  
  binaryapply_byname(complete_func, a = a, b = mat, .FUNdots = list(fill = fill, 
                                                                    fillrow = fillrow, fillcol = fillcol, 
                                                                    margin = margin), 
                     match_type = "all", set_rowcoltypes = TRUE, .organize = FALSE)
}