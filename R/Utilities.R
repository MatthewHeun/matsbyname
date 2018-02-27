# This file contains several utility functions for the byname package.


#' Organize binary arguments
#'
#' Organizes arguments of binary (2 arguments) \code{_byname} functions.
#' Actions performed are:
#' \itemize{
#'  \item{if only one argument is a list, make the other argument also a list of equal length.}
#'  \item{if both arguments are lists, ensure that they are same length.}
#'  \item{if one argument is a matrix and the other is a constant, make the constant into a matrix.}
#'  \item{ensures that row and column types match for \code{typematch_margins}.}
#'  \item{ensures that list item names match if both \code{a} and \code{b} are lists; 
#'        no complaints are made if neither \code{a} nor \code{b} has names.}
#'  \item{completes and sorts the matrices.}
#' }
#'
#' @param a the first argument to be organized
#' @param b the second argument to be organized
#' @param match_type one of \code{"all"} or \code{"matmult"}.
#' When both \code{a} and \code{b} are matrices,
#' "\code{all}" (the default) indicates that
#' rowtypes of \code{a} must match rowtypes of \code{b} and
#' coltypes of \code{a} must match coltypes of \code{b}.
#' If "\code{matmult}",
#' coltypes of \code{a} must match rowtypes of \code{b}.
#' @param fill a replacement value for \code{a} or \code{b} if either is missing or \code{NULL}.
#'
#' @return a list with two elements (named \code{a} and \code{b}) containing organized versions of the arguments
organize_args <- function(a, b, match_type = "all", fill){
  if (missing(a)) {
    if (missing(fill)) {
      stop("Missing argument a with no fill in organize_args.")
    } else {
      a <- fill
    }
  }
  if (is.null(a)) {
    if (missing(fill)) {
      stop("Null argument a with no fill in organize_args.")
    } else {
      a <- fill
    }
  }
  if (missing(b)) {
    if (missing(fill)) {
      stop("Missing argument b with no fill in organize_args.")
    } else {
      b <- fill
    }
  }
  if (is.null(b)) {
    if (missing(fill)) {
      stop("Null argument b with no fill in organize_args.")
    } else {
      b <- fill
    }
  }
  if (is.list(a) | is.list(b)) {
    if (!is.list(a)) {
      # b is a list, but a is not.  Make a into a list and give it same names as b.
      a <- make_list(a, n = length(b)) %>% set_names(names(b))
    }
    if (!is.list(b)) {
      # a is a list, but b is not.  Make b into a list and give it same names as a.
      b <- make_list(b, n = length(a)) %>% set_names(names(a))
    }
  }
  if (is.list(a) & is.list(b)) {
    # Both a and b are lists. Ensure they're the same length.
    stopifnot(length(a) == length(b))
    # Ensure that a and b have same length of names
    stopifnot(length(names(a)) == length(names(b)))
    # Ensure that a and b have same names if either has names
    stopifnot(names(a) == names(b))
    # Now return the lists.
    return(list(a = a, b = b))
  }
  
  # Neither a nor b are lists.
  if (!is.matrix(a) & !is.matrix(b)) {
    # Neither a nor b are matrices. Assume we have two constants. Return the constants in a vector.
    return(list(a = a, b = b))
  }
  
  # Neither a nor b are lists.
  # We don't know if one or both a and b is a matrix.
  # If one is not a matrix, assume it is a constant and try to make it into an appropriate-sized matrix.
  if (!is.matrix(a) & is.matrix(b)) {
    a <- matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b)) %>%
      setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (is.matrix(a) & !is.matrix(b)) {
    b <- matrix(b, nrow = nrow(a), ncol = ncol(a), dimnames = dimnames(a)) %>%
      setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  }
  
  # Assume that both a and b are now matrices.
  # Need to check whether matchtype is a known type.
  if (!match_type %in% c("all", "matmult", "none"))  {
    stop(paste("Unknown match_type", match_type, "in organize_args."))
  }
  
  # Verify that row and column types are appropriate.
  if (match_type == "all") {
    # If neither rowtype nor coltype are set,
    # skip these tests
    if (!is.null(rowtype(a)) & !is.null(coltype(a)) & !is.null(rowtype(b)) & !is.null(coltype(b))) {
      # Verify that the row type of a and b are the same.
      if (rowtype(a) != rowtype(b)) {
        stop(paste0("rowtype(a) (", rowtype(a), ") != rowtype(b) (", rowtype(b),")."))
      }
      # Verify that the column type of a and b are the same.
      if (coltype(a) != coltype(b)) {
        stop(paste0("coltype(a) (", coltype(a), ") != coltype(b) (", coltype(b),")."))
      }
    }
  } 
  if (match_type == "matmult") {
    # If neither coltype(a) nor rowtype(b) are set,
    # skip this test
    if (!is.null(coltype(a)) & !is.null(rowtype(b))) {
      # Verify that the column type of a and the row type of b are the same.
      if (coltype(a) != rowtype(b)) {
        stop(paste0("coltype(a) != rowtype(b): ", coltype(a), " != ", rowtype(b),"."))
      }
    }
  } 
  
  # Ensure that matrices have correct row and column names and are in same order.
  if (match_type == "all") {
    matrices <- complete_and_sort(a, b)
    outa <- matrices$m1 %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    outb <- matrices$m2 %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (match_type == "matmult") {
    # When the match_type is "matmult", we need to ensure that the columns of a match the rows of b.
    # To do so, we transpose b prior to completing and sorting, and we complete and sort on columns.
    matrices <- complete_and_sort(a, transpose_byname(b), margin = 2)
    outa <- matrices$m1 %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    # Before sending back, we need to re-transpose b.
    outb <- matrices$m2 %>% transpose_byname %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (match_type == "none") {
    outa <- a
    outb <- b
  } else {
    stop(paste("Unknown match_type", match_type, "in organize_args."))
  }
  # Reset row and column types.
  return(list(a = outa, b = outb))
}

#' @title
#' Create regex patterns for row and column selection by name
#'
#' @description
#' This function is intended for use with the \code{select_rows_byname}
#' and \code{select_cols_byname} functions.
#' \code{make_pattern} correctly escapes special characters in \code{row_col_names},
#' such as \code{(} and \code{)}, as needed.
#' Thus, it is highly recommended that \code{make_pattern} be used when
#' constructing patterns for row and column selections with
#' \code{select_rows_byname}
#' and \code{select_cols_byname}.
#'
#' @details
#' \code{pattern_type} controls the type of pattern created:
#' \itemize{
#'   \item{\code{exact} produces a pattern that selects row or column names by exact match.}
#'   \item{\code{leading} produces a pattern that selectes row or column names if the item in \code{row_col_names} matches
#'         the beginnings of row or column names.}
#'   \item{\code{trailing} produces a pattern that selectes row or column names if the item in \code{row_col_names} matches
#'         the ends of row or column names.}
#'   \item{\code{anywhere} produces a pattern that selectes row or column names if the item in \code{row_col_names} matches
#'         any substring of row or column names.}
#' }
#'
#' @param row_col_names a vector of row and column names
#' @param pattern_type one of \code{exact}, \code{leading}, \code{trailing}, or \code{anywhere}.
#'
#' @return an extended regex pattern suitable for use with \code{select_rows_byname} or \code{select_cols_byname}.
#' 
#' @importFrom Hmisc escapeRegex
#' 
#' @export
#'
#' @examples
#' make_pattern(row_col_names = c("a", "b"), pattern_type = "exact")
make_pattern <- function(row_col_names, pattern_type = c("exact", "leading", "trailing", "anywhere")){
  pattern_type <- match.arg(pattern_type)
  out <- Hmisc::escapeRegex(row_col_names)
  # Add leading caret if needed
  if (pattern_type %in% c("exact", "leading")) {
    out <- paste0("^", out)
  }
  # Add trailing dollar sign if needed
  if (pattern_type %in% c("exact", "trailing")) {
    out <- paste0(out, "$")
  }
  paste0(out, collapse = "|")
}

#' Named list of rows or columns of matrices
#' 
#' This function takes matrix \code{m} and converts it to a list of 
#' single-row (if \code{margin == 1}) or single-column(if \code{margin == 2})
#' matrices.
#' Each item in the list is named for its row (if \code{margin == 1}) 
#' or column (if \code{margin == 2}).
#'
#' Note that the result provides column vectors, regardless of the value of \code{margin}.
#'
#' @param m a matrix or list of matrices (say, from a column of a data frame)
#' @param margin the margin of the matrices to be extracted (\code{1} for rows, \code{2} for columns)
#'
#' @return a named list of rows or columns extracted from \code{m}
#' @export
#' @importFrom magrittr set_names
#'
#' @examples
#' library(magrittr)
#' m <- matrix(data = c(1:6), 
#'             nrow = 2, ncol = 3, 
#'             dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>%
#'   setrowtype(rowtype = "Products") %>% setcoltype(coltype = "Industries")
#' list_of_rows_or_cols(m, margin = 1)
#' list_of_rows_or_cols(m, margin = 2)
list_of_rows_or_cols <- function(m, margin){
  # if (is.list(m)) {
  #   margin <- make_list(margin, n = length(m), lenx = 1)
  # }
  lrc.func <- function(m, margin){
    stopifnot(length(margin) == 1)
    stopifnot(margin %in% c(1,2))
    stopifnot("matrix" %in% class(m))
    # Strategy: perform all operations with margin to be split into a list in columns.
    if (margin == 1) {
      # Caller requested rows to be split into list items.
      # Transpose so operations will be easier.
      out <- transpose_byname(m)
    } else {
      out <- m
    }
    lapply(seq_len(ncol(out)), function(i){
      matrix(out[,i], nrow = nrow(out), ncol = 1, dimnames = list(rownames(out), colnames(out)[[i]])) %>%
        setrowtype(rowtype(out)) %>% setcoltype(coltype(out))
    }) %>%
      set_names(colnames(out))
  }
  unaryapply_byname(lrc.func, a = m, .FUNdots = list(margin = margin), rowcoltypes = "none")
}

#' Gets row names
#'
#' Gets row names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param m The matrix or data frame on which row names are to be retrieved
#'
#' @return row names of \code{m}
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' getrownames_byname(m)
#' # This also works for lists
#' getrownames_byname(list(m,m))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' getrownames_byname(DF$m)
getrownames_byname <- function(m){
  unaryapply_byname(rownames, a = m, rowcoltypes = "none")
}

#' Gets column names
#'
#' Gets column names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param m The matrix or data frame from which column names are to be retrieved
#'
#' @return column names of \code{m}
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' getcolnames_byname(m)
#' # This also works for lists
#' getcolnames_byname(list(m,m))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' getcolnames_byname(DF$m)
getcolnames_byname <- function(m){
  unaryapply_byname(colnames, a = m, rowcoltypes = "none")
}

#' Sets row names
#'
#' Sets row names in a way that is amenable to use in piping operations in a functional programming way.
#' If \code{m} is a constant, it is converted to a matrix and \code{rownames} are applied.
#' If \code{m} is a matrix, \code{rownames} should be a vector of new row names
#' that is as long as the number of rows in \code{m}.
#' If \code{m} is a list of matrices, 
#' \code{rownames} can also be a list, and it should be as long \code{m}.
#' Or \code{rownames} can be a vector of row names which will be applied to every matrix in
#' the list of \code{m}.
#' Each item in the list should be a vector containing row names for the corresponding 
#' matrix in \code{m}.
#'
#' @param m A matrix or a list of matrices in which row names are to be set
#' @param rownames A vector of new row names or a list of vectors of new row names
#'
#' @return a copy of \code{m} with new row names
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' setrownames_byname(m, c("a", "b"))
#' setrownames_byname(m %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
#' m %>% setrownames_byname(NULL)
#' m %>% setrownames_byname(NA)
#' 2 %>% setrownames_byname("row")
#' # This also works for lists
#' setrownames_byname(list(m,m), c("a", "b"))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' setrownames_byname(DF$m, c("r1", "r2"))
#' setrownames_byname(DF$m, c("c", "d"))
#' DF <- DF %>% mutate(m = setrownames_byname(m, c("r1", "r2")))
#' DF$m[[1]]
setrownames_byname <- function(m, rownames){
  # if (is.list(m) & !is.matrix(m) & is.vector(rownames) & !is.list(rownames)) {
  #   # rownames is a vector of names (and not a list) to be applied 
  #   # to each matrix in m.
  #   # Thus, we should replicatate it to be same length as m
  #   rownames <- make_list(rownames, n = length(m), lenx = 1)
  # }
  rowname.func <- function(m, rownames){
    if (is.null(dim(m))) {
      # m has no dimensions. It is a constant.
      # Turn it into a matrix and set the row names.
      out <- matrix(m, nrow = 1, ncol = 1)
    } else {
      out <- m
    }
    if (is.null(rownames) || is.na(rownames)) {
      # replace with default row names
      rownames(out) <- NULL
    } else {
      rownames(out) <- rownames
    }
    return(out)
  }
  unaryapply_byname(rowname.func, a = m, .FUNdots = list(rownames = rownames), rowcoltypes = "all")
}

#' Sets column names
#'
#' Sets column names in a way that is amenable to use in piping operations in a functional programming way.
#' If \code{m} is a constant, it is converted to a matrix and \code{colnames} are applied.
#' If \code{m} is a matrix, \code{colnames} should be a vector of new column names
#' that is as long as the number of columns in \code{m}.
#' If \code{m} is a list of matrices, 
#' \code{colnames} can also be a list, and it should be as long \code{m}.
#' Or \code{colnames} can be a vector of column names which will be applied to every matrix in
#' the list of \code{m}.
#' Each item in the list should be a vector containing column names for the corresponding 
#' matrix in \code{m}.
#'
#' @param m A matrix or a list of matrices in which column names are to be set
#' @param colnames A vector of new column names or a list of vectors of new column names
#'
#' @return a copy of \code{m} with new column names
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' setcolnames_byname(m, c("a", "b", "c"))
setcolnames_byname <- function(m, colnames){
  # if (is.list(m) & !is.matrix(m) & is.vector(colnames) & !is.list(colnames)) {
  #   # colnames is a vector of names (and not a list) to be applied 
  #   # to each matrix in m.
  #   # Thus, we should replicatate it to be same length as m
  #   colnames <- make_list(colnames, n = length(m), lenx = 1)
  # }
  colname.func <- function(m, colnames){
    if (is.null(dim(m))) {
      # m has no dimensions. It is a constant.
      # Turn it into a matrix and set the row names.
      out <- matrix(m, nrow = 1, ncol = 1)
    } else {
      out <- m
    }
    if (is.null(colnames) || is.na(colnames)) {
      # replace with default row names
      colnames(out) <- NULL
    } else {
      colnames(out) <- colnames
    }
    return(out)
  }
  unaryapply_byname(colname.func, a = m, .FUNdots = list(colnames = colnames), rowcoltypes = "all")
}

#' Sets row type for a matrix or a list of matrices
#'
#' This function is a wrapper for \code{attr} so that 
#' setting can be accomplished by the pipe operator (\code{\%>\%}).
#' Row types are strings stored in the \code{rowtype} attribute.
#' 
#' If \code{is.null(rowtype)}, the rowtype attribute is deleted
#' and subsequent calls to \code{rowtype} will return \code{NULL}.
#'
#' @param x the matrix on which row type is to be set
#' @param rowtype the type of item stored in rows
#'
#' @return \code{x} with rowtype attribute set to \code{rowtype}.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
#' U %>% setrowtype("Commodities")
#' # This also works for lists
#' setrowtype(list(U,U), rowtype = "Commodities")
#' setrowtype(list(U,U), rowtype = list("Commodities", "Commodities"))
#' DF <- data.frame(U = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' setrowtype(DF$U, "Commodities")
#' DF <- DF %>% mutate(newcol = setrowtype(U, "Commodities"))
#' DF$newcol[[1]]
#' DF$newcol[[2]]
setrowtype <- function(x, rowtype){
  rt.func <- function(x, rowtype){
    attr(x, "rowtype") <- rowtype
    return(x)
  }
  unaryapply_byname(rt.func, a = x, .FUNdots = list(rowtype = rowtype), rowcoltypes = "none")
}

#' Sets column type for a matrix or a list of matrices
#'
#' This function is a wrapper for \code{attr} so that 
#' setting can be accomplished by the pipe operator (\code{\%>\%}).
#' Column types are strings stored in the \code{coltype} attribute.
#' 
#' #' If \code{is.null(coltype)}, the coltype attribute is deleted
#' and subsequent calls to \code{coltype} will return \code{NULL}.
#'
#' @param x the matrix on which column type is to be set
#' @param coltype the type of item stored in columns
#'
#' @return \code{x} with \code{coltype} attribute set.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
#' U %>% setcoltype("Industries")
#' # This also works for lists
#' setcoltype(list(U,U), coltype = "Industries")
#' setcoltype(list(U,U), coltype = list("Industries", "Industries"))
#' DF <- data.frame(U = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' setcoltype(DF$U, "Industries")
#' DF <- DF %>% mutate(newcol = setcoltype(U, "Industries"))
#' DF$newcol[[1]]
#' DF$newcol[[2]]
setcoltype <- function(x, coltype){
  ct.func <- function(x, coltype){
    attr(x, "coltype") <- coltype
    return(x)
  }
  unaryapply_byname(ct.func, a = x, .FUNdots = list(coltype = coltype), rowcoltypes = "none")
}

#' Row type
#'
#' Extracts row type of \code{x}.
#'
#' @param x the object from which you want to extract row types
#'
#' @return the row type of \code{x}
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' rowtype(U)
#' # This also works for lists
#' rowtype(list(U,U))
rowtype <- function(x){
  unaryapply_byname(attr, a = x, .FUNdots = list(which = "rowtype"), rowcoltypes = "none")
}

#' Column type
#'
#' Extracts column type of \code{x}.
#'
#' @param x the object from which you want to extract column types
#'
#' @return the column type of \code{x}
#' @export
#'
#' @examples
#' library(magrittr)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' coltype(U)
#' # This also works for lists
#' coltype(list(U,U))
coltype <- function(x){
  unaryapply_byname(attr, a = x, .FUNdots = list(which = "coltype"), rowcoltypes = "none")
}

#' Select rows of a matrix (or list of matrices) by name
#'
#' Arguments indicate which rows are to be retained and which are to be removed.
#' For maximum flexibility, arguments are extended regex patterns
#' that are matched against row names.
#'
#' Patterns are compared against row names using extended regex.
#' If no row names of \code{m} match the \code{retain_pattern}, \code{NULL} is returned.
#' If no row names of \code{m} match the \code{remove_pattern}, \code{m} is returned.
#' Note that the default \code{retain_pattern} and \code{remove_pattern} (\code{$^}) 
#' retain nothing and remove nothing.
#'
#' Retaining rows takes precedence over removing rows, always.
#'
#' Some typical patterns are:
#' \itemize{
#'   \item{\code{^Electricity$|^Oil$}: row names that are EXACTLY \code{Electricity} or EXACTLY \code{Oil}.}
#'   \item{\code{^Electricity|^Oil}: row names that START WITH \code{Electricity} or START WITH \code{Oil}.}
#'   \item{\code{Electricity|Oil}: row names that CONTAIN \code{Electricity} or CONTAIN \code{Oil} anywhere within them.}
#' }
#'
#' Given a list of row names, a pattern can be constructed easily using the \code{make_pattern} function.
#' \code{make_pattern} escapes regex strings using \code{Hmisc::escapeRegex}.
#' This function assumes that \code{retain_pattern} and \code{remove_pattern} have already been
#' suitably escaped.
#'
#' @param m a matrix or a list of matrices
#' @param retain_pattern an extended regex or list of extended regexes that specifies which rows of \code{m} to retain.
#' Default pattern (\code{$^}) retains nothing.
#' @param remove_pattern an extended regex or list of extended regexes that specifies which rows of \code{m} to remove
#' Default pattern (\code{$^}) removes nothing.
#'
#' @return a matrix that is a subset of \code{m} with rows selected by \code{retain_pattern} and \code{remove_pattern}.
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_rows_byname(m, retain_pattern = make_pattern(c("i1", "i4"), pattern_type = "exact"))
#' select_rows_byname(m, remove_pattern = make_pattern(c("i1", "i3"), pattern_type = "exact"))
#' # Also works for lists and data frames
#' select_rows_byname(list(m,m), retain_pattern = "^i1$|^i4$")
select_rows_byname <- function(m, retain_pattern = "$^", remove_pattern = "$^"){
  # Note default patterns ("$^") retain nothing and remove nothing,
  # because $ means end of line and ^ means beginning of line.
  # The default pattern would match lines where the beginning of the line is the end of the line.
  # That is impossible, so nothing is matched.
  # if (is.list(m)) {
  #   # If m is a list, we need to ensure that the patterns are also lists. 
  #   retain_pattern <- make_list(retain_pattern, n = length(m))
  #   remove_pattern <- make_list(remove_pattern, n = length(m))
  # }
  select.func <- function(m, retain_pattern, remove_pattern){
    retain_indices <- grep(pattern = retain_pattern, x = rownames(m))
    remove_indices <- grep(pattern = remove_pattern, x = rownames(m))
    if (length(retain_indices) == 0) {
      # Nothing to be retained, so try removing columns
      if (length(remove_indices) == 0) {
        # Nothing to be retained and nothing to be removed.
        # If the caller wanted to retain something,
        # which is indicated by a non-default retain_pattern,
        # don't retain anything.
        # Do this first, because retain takes precedence.
        if (retain_pattern != "$^") {
          return(NULL)
        }
        # If the caller wanted to remove something,
        # which is indicated by a non-default remove_pattern,
        # don't remove anything. Simply return m.
        if (remove_pattern != "$^") {
          return(m)
        }
        # Neither retain_pattern nor remove_pattern is different from the default.
        # This is almost surely an error.
        stop("neither retain_pattern nor remove_pattern are differnt from default.")
      }
      # Remove
      return(m[-remove_indices , ] %>%
               # When only 1 row is selected, the natural result will be a numeric vector
               # We want to ensure that the return value is a matrix
               # with correct rowtype and coltype.
               # Thus, we need to take these additional steps.
               matrix(nrow = nrow(m) - length(remove_indices),
                      dimnames = list(dimnames(m)[[1]][setdiff(1:nrow(m), remove_indices)],
                                      dimnames(m)[[2]])) %>%
               setrowtype(rowtype(m)) %>%
               setcoltype(coltype(m))
      )
    }
    # Retain
    return(m[retain_indices , ] %>%
             matrix(nrow = length(retain_indices),
                    dimnames = list(dimnames(m)[[1]][retain_indices],
                                    dimnames(m)[[2]])) %>%
             setrowtype(rowtype(m)) %>%
             setcoltype(coltype(m))
    )
  }
  unaryapply_byname(select.func, a = m, 
                    .FUNdots = list(retain_pattern = retain_pattern, remove_pattern = remove_pattern), 
                    rowcoltypes = "none")
}

#' @title
#' Select columns of a matrix (or list of matrices) by name
#'
#' @description
#' Arguments indicate which columns are to be retained and which are to be removed.
#' For maximum flexibility, arguments are extended regex patterns
#' that are matched against column names.
#'
#' @details
#' Patterns are compared against column names using extended regex.
#' If no column names of \code{m} match the \code{retain_pattern}, \code{NULL} is returned.
#' If no column names of \code{m} match the \code{remove_pattern}, \code{m} is returned.
#'
#' Retaining columns takes precedence over removing columns, always.
#'
#' Some typical patterns are:
#' \itemize{
#'   \item{\code{^Electricity$|^Oil$}: column names that are EXACTLY \code{Electricity} or \code{Oil}.}
#'   \item{\code{^Electricity|^Oil}: column names that START WITH \code{Electricity} or \code{Oil}.}
#'   \item{\code{Electricity|Oil}: column names that CONTAIN \code{Electricity} or \code{Oil} anywhere within them.}
#' }
#'
#' Given a list of column names, a pattern can be constructed easily using the \code{make_pattern} function.
#' 
#' #' \code{make_pattern} escapes regex strings using \code{Hmisc::escapeRegex}.
#' This function assumes that \code{retain_pattern} and \code{remove_pattern} have already been
#' suitably escaped.
#' 
#' Note that the default \code{retain_pattern} and \code{remove_pattern} (\code{$^}) 
#' retain nothing and remove nothing.
#' 
#'
#' @param m a matrix or a list of matrices
#' @param retain_pattern an extended regex or list of extended regexes that specifies which columns of \code{m} to retain.
#' Default pattern (\code{$^}) retains nothing.
#' @param remove_pattern an extended regex or list of extended regexes that specifies which columns of \code{m} to remove
#' Default pattern (\code{$^}) removes nothing.
#'
#' @return a matrix that is a subset of \code{m} with columns selected by \code{retain_pattern} and \code{remove_pattern}.
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_cols_byname(m, retain_pattern = make_pattern(c("p1", "p4"), pattern_type = "exact"))
#' select_cols_byname(m, remove_pattern = make_pattern(c("p1", "p3"), pattern_type = "exact"))
#' # Also works for lists and data frames
#' select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$")
select_cols_byname <- function(m, retain_pattern = "$^", remove_pattern = "$^"){
  # Note default patterns ("$^") retain nothing and remove nothing,
  # because $ means end of line and ^ means beginning of line.
  # The default pattern would match lines where the beginning of the line is the end of the line.
  # That is impossible, so nothing is matched.
  # if (is.list(m)) {
  #   retain_pattern <- make_list(retain_pattern, n = length(m))
  #   remove_pattern <- make_list(remove_pattern, n = length(m))
  # }
  select.func <- function(m, retain_pattern, remove_pattern){
    retain_indices <- grep(pattern = retain_pattern, x = colnames(m))
    remove_indices <- grep(pattern = remove_pattern, x = colnames(m))
    if (length(retain_indices) == 0) {
      # Nothing to be retained, so try removing columns
      if (length(remove_indices) == 0) {
        # Nothing to be retained and nothing to be removed.
        # If the caller wanted to retain something,
        # which is indicated by a non-default retain_pattern,
        # don't retain anything.
        # Do this first, because retain takes precedence.
        if (retain_pattern != "$^") {
          return(NULL)
        }
        # If the caller wanted to remove something,
        # which is indicated by a non-default remove_pattern,
        # don't remove anything. Simply return m.
        if (remove_pattern != "$^") {
          return(m)
        }
        # Neither retain_pattern nor remove_pattern is different from the default.
        # This is almost surely an error.
        stop("neither retain_pattern nor remove_pattern are differnt from default.")
      }
      # Remove
      return(m[ , -remove_indices] %>%
               # When only 1 row is selected, the natural result will be a numeric vector
               # We want to ensure that the return value is a matrix
               # with correct rowtype and coltype.
               # Thus, we need to take these additional steps.
               matrix(ncol = ncol(m) - length(remove_indices),
                      dimnames = list(dimnames(m)[[1]],
                                      dimnames(m)[[2]][setdiff(1:ncol(m), remove_indices)])) %>%
               setrowtype(rowtype(m)) %>%
               setcoltype(coltype(m))
      )
    }
    # Retain
    return(m[ , retain_indices] %>%
             matrix(ncol = length(retain_indices),
                    dimnames = list(dimnames(m)[[1]],
                                    dimnames(m)[[2]][retain_indices])) %>%
             setrowtype(rowtype(m)) %>%
             setcoltype(coltype(m))
    )
    
  }
  unaryapply_byname(select.func, 
                    a = m, 
                    .FUNdots = list(retain_pattern = retain_pattern, remove_pattern = remove_pattern), 
                    rowcoltypes = "none")
}

#' Cleans (deletes) rows or columns of matrices that contain exclusively \code{clean_value}
#'
#' @param m the matrix to be cleaned
#' @param margin the dimension over which cleaning should occur, \code{1} for rows, \code{2} for columns,
#' or \code{c(1,2)} for both rows and columns.
#' @param clean_value the undesirable value
#'
#' When a row (when \code{margin = 1}) or a column (when \code{margin = 2})
#' contains exclusively \code{clean_value}, the row or column is deleted from the matrix.
#'
#' @return a "cleaned" matrix, expunged of rows or columns that contain exclusively \code{clean_value}.
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(-20, 1, -20, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' m
#' m %>% clean_byname(margin = 1, clean_value = -20) # Eliminates -20, -20 row
#' # Nothing cleaned, because no columns contain all 0's (the default clean_value).
#' m %>% clean_byname(margin = 2) 
#' # Also works with lists
#' list(m, m) %>% clean_byname(margin = 1, clean_value = -20)
#' # Also works with data frames
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' DF %>% clean_byname(margin = 1, clean_value = -20)
#' m2 <- matrix(c(-20, -20, 0, -20, -20, 0, -20, -20, -20), nrow = 3,
#'              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")) )
#' m2
#' clean_byname(m2, margin = c(1,2), clean_value = -20)
#' DF2 <- data.frame(m2 = I(list()))
#' DF2[[1, "m2"]] <- m2
#' DF2[[2, "m2"]] <- m2
#' DF2 %>% clean_byname(margin = c(1, 2), clean_value = -20)
clean_byname <- function(m, margin = c(1, 2), clean_value = 0){
  if (1 %in% margin & 2 %in% margin) {
    # Clean both dimensions of m.
    cleaned1 <- clean_byname(m, margin = 1, clean_value = clean_value)
    cleaned2 <- clean_byname(cleaned1, margin = 2, clean_value = clean_value)
    return(cleaned2)
  }
  clean.func <- function(m, margin, clean_value){
    if (margin == 1) {
      # Want to clean rows. Code below assumes want to clean columns.
      # Transpose and then transpose again before returning.
      a <- transpose_byname(m)
    } else if (margin == 2) {
      a <- m
    } else {
      stop(paste("margin =", margin, "in clean_byname. Must be 1 or 2."))
    }
    keepcols <- apply(a, 2, function(x) {!all(x == clean_value)})
    keepcolnames <- names(which(keepcols))
    b <- select_cols_byname(m = a, retain_pattern = make_pattern(row_col_names = keepcolnames, pattern_type = "exact"))
    if (margin == 1) {
      return(transpose_byname(b))
    } else if (margin == 2) {
      return(b)
    }
  }
  unaryapply_byname(clean.func, a = m, .FUNdots = list(margin = margin, clean_value = clean_value), rowcoltypes = "all")
}

#' Test whether this is the zero matrix
#'
#' @param m a matrix of list of matrices
#' @param tol the allowable deviation from 0 for any element
#'
#' @return \code{TRUE} iff this is the zero matrx within \code{tol}.
#' @export
#'
#' @examples
#' zero <- matrix(0, nrow = 50, ncol = 50)
#' iszero_byname(zero)
#' nonzero <- matrix(1:4, nrow = 2)
#' iszero_byname(nonzero)
#' # Also works for lists
#' iszero_byname(list(zero, nonzero))
#' # And it works for data frames
#' DF <- data.frame(A = I(list()), B = I(list()))
#' DF[[1,"A"]] <- zero
#' DF[[2,"A"]] <- nonzero
#' DF[[1,"B"]] <- nonzero
#' DF[[2,"B"]] <- zero
#' iszero_byname(DF$A)
#' iszero_byname(DF$B)
#' iszero_byname(matrix(1e-10, nrow = 2))
#' iszero_byname(matrix(1e-10, nrow = 2), tol = 1e-11)
iszero_byname <- function(m, tol = 1e-6){
  zero.func <- function(m, tol){
    all(abs(m) < tol)
  }
  unaryapply_byname(zero.func, a = m, .FUNdots = list(tol = tol), rowcoltypes = "none")
}

#' Logarithmic mean of two numbers
#' 
#' Calculates the logarithmic mean of two numbers.
#' 
#' This is an internal helper function for \code{logarithmicmean_byname}.
#'
#' @param x1 the first operand (must be non-negative)
#' @param x2 the second operand (must be non-negative)
#' @param base the base of the logarithm used in this calculation. 
#'        (Default is \code{exp(1)}.)
#'
#' @return \code{0} if \code{x1 = 0} or \code{x2 = 0}; \code{x1} if \code{x1 == x2}; and
#'         \code{(x1 - x2) / log(x1/x2, base = base)} 
#'         for all other values of \code{x1} and \code{x2}
#'         
#' @export
#'
#' @examples
#' matsbyname:::logmean(0, 0) # 0
#' matsbyname:::logmean(0, 1) # 0
#' matsbyname:::logmean(1, 0) # 0
#' matsbyname:::logmean(1, 1) # 1
#' matsbyname:::logmean(2, 1)
#' matsbyname:::logmean(1, 2) # commutative
#' matsbyname:::logmean(1, 10) # base = exp(1), the default
#' matsbyname:::logmean(1, 10, base = 10)
logmean <- function(x1, x2, base = exp(1)){
  # Take care of pathological cases.  
  # See https://en.wikipedia.org/wiki/Logarithmic_mean for details.
  if (x1 == 0) {
    return(0)
  }
  if (x2 == 0) {
    return(0)
  }
  if (x1 == x2) {
    return(x1)
  }
  (x1 - x2) / log(x1/x2, base = base)
}

