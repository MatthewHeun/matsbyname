
#' Sorts rows and columns of a matrix
#' 
#' Checks that row names are unique and that column names are unique.
#' Then, sorts the rows and columns in a way that ensures
#' any other matrix with the same row and column names will have 
#' the same order.
#' 
#' Default sort order is given by `base::sort()` with `decreasing = FALSE`.
#'
#' @param a A matrix or data frame whose rows and columns are to be sorted.
#' @param margin Specifies the subscript(s) in `a` over which sorting will occur. 
#'               `margin` has nearly the same semantic meaning as in `base::apply`.
#'               For rows only, give `1`; 
#'               for columns only, give `2`;
#'               for both rows and columns, give `c(1,2)`, the default value.
#' @param roworder Specifies the order for rows with default `sort(rownames(a))`. 
#'                 If `NA` (the default), default sort order is used. 
#'                 Unspecified rows are removed from the output, thus providing a way to delete rows from `a`.
#'                 Extraneous row names (row names in `roworder` that do not appear in `a`) are ignored.
#' @param colorder Specifies the order for rows with default `sort(colnames(a))`.
#'                 If `NA` (the default), default sort order is used. 
#'                 Unspecified columns are removed from the output, thus providing a way to delete columns from `a`.
#'                 Extraneous column names (column names in `colorder` that do not appear in `a`) are ignored.
#' 
#' @return A modified version of `a` with sorted rows and columns
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

