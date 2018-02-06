library(parallel)
library(magrittr)
library(dplyr)

#' Logarithm of matrix elements
#' 
#' Specify the base of the log with \code{base} argument.
#'
#' @param M a matrix of list of matrices 
#' @param base the base of the logarithm (default is \code{exp(1)}, giving the natural logarithm)
#'
#' @return M with each element replaced by its base \code{base} logarithm
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' elementlog_byname(exp(1))
#' m <- matrix(c(10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' elementlog_byname(m)
#' elementlog_byname(m, base = 10)
elementlog_byname <- function(M, base = exp(1)){
  unaryapply_byname(log, a = M, base = base)
}


#' Exponential of matrix elements
#' 
#' Gives the exponential of all elements of a matrix or list of matrices
#'
#' @param M a matrix of list of matrices 
#'
#' @return M with each element replaced by its exponential
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' elementexp_byname(1)
#' m <- matrix(c(log(10),log(1),log(1),log(100)), 
#'   nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' elementexp_byname(m)
elementexp_byname <- function(M){
  unaryapply_byname(exp, a = M)
}


#' Invert a matrix
#'
#' This function transposes row and column names as well as row and column types.
#' Rows and columns of \code{m} are sorted prior to inverting.
#'
#' @param m the matrix to be inverted. \code{m} must be square.
#'
#' @return the inversion
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' invert_byname(m)
#' matrixproduct_byname(m, invert_byname(m))
#' matrixproduct_byname(invert_byname(m), m)
#' invert_byname(list(m,m))
invert_byname <- function(m){
  unaryapply_byname(solve, a = m, rowcoltypes = "transpose")
}

#' Transpose a matrix by name
#'
#' @param m the matrix to be transposed
#'
#' @return the transposed matrix
#' @export
#'
#' @examples
#' library(magrittr)
#' m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' transpose_byname(m)
#' transpose_byname(list(m,m))
transpose_byname <- function(m){
  unaryapply_byname(t, a = m, rowcoltypes = "transpose")
}

#' Creates a diagonal "hat" matrix from a vector.
#'
#' @param v The vector from which a "hat" matrix is to be created.
#'
#' A "hat" matrix is one in which the only non-zero elements are stored on the diagonal.
#' To "hatize" a vector is to place its elements on the diagonal of an otherwise-zero square matrix.
#' \code{v} must be a matrix with at least one dimension of length 1 (a vector).
#' The names of both dimensions of the hatized matrix are the same and taken from \code{v}.
#' Note that the vector names are sorted prior to forming the "hat" matrix.
#'
#' @return A square "hat" matrix with size equal to the length of \code{v}.
#' @export
#'
#' @examples
#' library(magrittr)
#' v <- matrix(1:10, ncol = 1, dimnames=list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' r <- matrix(1:5, nrow = 1, dimnames=list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatize_byname(v)
#' hatize_byname(r)
#' # This also works with lists.
#' hatize_byname(list(v, v))
hatize_byname <- function(v){
  hatize.func <- function(v){
    v_sorted <- sort_rows_cols(v) # %>% setrowtype(rowtype(v)) %>% setcoltype(coltype(v))
    out <- OpenMx::vec2diag(v_sorted)
    if (ncol(v) == 1) {
      rownames(out) <- rownames(v_sorted)
      colnames(out) <- rownames(v_sorted)
      # This function does not rely on unaryapply_byname to set row and column types.
      # So, we must do so here.
      out <- out %>% setrowtype(rowtype(v)) %>% setcoltype(rowtype(v))
    } else if (nrow(v) == 1) {
      rownames(out) <- colnames(v)
      colnames(out) <- colnames(v)
      # This function does not rely on unaryapply_byname to set row and column types.
      # So, we must do so here.
      out <- out %>% setrowtype(coltype(v)) %>% setcoltype(coltype(v))
    } else {
      stop("matrix v must have at least one dimension of length 1 in hatize")
    }
    return(out)
  }
  unaryapply_byname(hatize.func, a = v, rowcoltypes = "none")
}

#' Named identity matrix or vector
#'
#' Creates an identity matrix (\strong{I}) or vector (\strong{i}) of same size and with same names and
#' same row and column types as \strong{M}.
#' If \code{margin = 1}, makes a column matrix filled with \code{1}s. 
#' Row names and type are taken from row names and type of \strong{M}.
#' Column name and type are same as column type of \strong{M}.
#' If \code{margin = 2}, make a row matrix filled with \code{1}s.
#' Column names and type are taken from column name and type of \strong{M}.
#' Row name and type are same as row type of \strong{M}.
#' If \code{c(1,2)} (the default), make an identity matrix with \code{1}s on the diagonal.
#' Row and column names are sorted on output.
#'
#' @param M the matrix whose names and dimensions are to be preserved in an identity matrix or vector
#' @param margin determines whether an identity vector or matrix is returned
#'
#' @return An identity matrix or vector.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' M <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("c", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' identize_byname(M)
#' identize_byname(M, margin = c(1,2))
#' identize_byname(M, margin = 1)
#' identize_byname(M, margin = 2)
#' N <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' identize_byname(N)
#' # This also works with lists
#' identize_byname(list(M, M))
identize_byname <- function(M, margin = c(1,2)){
  if (is.list(M)) {
    margin <- make_list(margin, n = length(M), lenx = 1)
  }
  identize.func <- function(M, margin){
    if (class(M) == "numeric" & length(M) == 1) {
      # Assume we have a single number here
      # Thus, we return 1.
      return(1)
    }
    
    if (!length(margin) %in% c(1,2)) {
      stop("margin should have length 1 or 2 in fractionize_byname")
    }
    
    if (length(margin) == 2 && all(margin %in% c(1,2))) {
      # M is a matrix. 
      # Return the identity matrix with 1's on diagonal,
      # of same dimensions as M
      # and same names and types as M.
      stopifnot(nrow(M) == ncol(M))
      return(diag(nrow(M)) %>% 
               setrownames_byname(rownames(M)) %>% setcolnames_byname(colnames(M)) %>% 
               setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
    }
    
    if (length(margin) != 1 || !margin %in% c(1,2)) {
      stop(paste("Unknown margin", margin, "in identize_byname. margin should be 1, 2, or c(1,2)."))
    }
    
    if (margin == 1)  {
      # Return a column vector containing 1's
      return(matrix(rep_len(1, nrow(M)), nrow = nrow(M), ncol = 1) %>% 
               setrownames_byname(rownames(M)) %>% setcolnames_byname(coltype(M)) %>% 
               setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
    }
    if (margin == 2) {
      # Return a row vector containing 1's
      return(matrix(rep_len(1, ncol(M)), nrow = 1, ncol = ncol(M)) %>% 
               setrownames_byname(rowtype(M)) %>% setcolnames_byname(colnames(M)) %>% 
               setrowtype(rowtype(M)) %>% setcoltype(coltype(M)))
    } 
    # Should never get here, but just in case:
    stop(paste("Unknown margin", margin, "in identize_byname. margin should be 1, 2, or c(1,2)."))
  }
  unaryapply_byname(identize.func, a = M, margin = margin, rowcoltypes = "none")
}

#' Compute fractions of matrix entries
#' 
#' This function divides all entries in \strong{M} by the specified sum,
#' thereby "fractionizing" the matrix.
#'
#' @param M the matrix to be fractionized
#' @param margin If \code{1} (rows), each entry in \strong{M} is divided by its row's sum.
#' If \code{2}, each entry in \strong{M} is divided by its column's sum.
#' If \code{c(1,2)}, each entry in \strong{M} is divided by the sum of all entries in \strong{M}.
#'
#' @return a fractionized matrix of same dimensions and same row and column types as \strong{M}.
#' @export
#'
#' @examples
#' library(magrittr)
#' M <- matrix(c(1, 5,
#'               4, 5),
#'             nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
#'             setcoltype("Products") %>% setrowtype("Industries")
#' fractionize_byname(M, margin = c(1,2))
#' fractionize_byname(M, margin = 1)
#' fractionize_byname(M, margin = 2)
fractionize_byname <- function(M, margin){
  if (is.list(M)) {
    margin <- make_list(margin, n = length(M), lenx = 1)
  }
  fractionize.func <- function(M, margin){
    if (!"matrix" %in% class(M) && !"data.frame" %in% class(M)) {
      # Assume we have a single number here
      # By dividing M by itself, we could throw a division by zero error,
      # which we would want to do.
      return(M/M)
    }
    if (length(margin) != length(unique(margin))) {
      stop("margin should contain unique integers in fractionize_byname")
    }
    if (!length(margin) %in% c(1,2)) {
      stop("margin should have length 1 or 2 in fractionize_byname")
    }
    
    if (length(margin) == 2 && all(margin %in% c(1,2))) {
      return(M/sumall_byname(M))
    }
    
    if (length(margin) != 1 || !margin %in% c(1,2)) {
      stop(paste("Unknown margin", margin, "in fractionize_byname. margin should be 1, 2, or c(1,2)."))
    }
    
    if (margin == 1) {
      # Divide each entry by its row sum
      # Do this with (M*i)_hat_inv * M
      return(matrixproduct_byname(M %>% rowsums_byname %>% hatize_byname %>% invert_byname, M))
    }
    if (margin == 2) {
      # Divide each entry by its column sum
      # Do this with M * (i^T * M)_hat_inv
      return(matrixproduct_byname(M, colsums_byname(M) %>% hatize_byname %>% invert_byname))
    } 
    # Should never get here, but just in case:
    stop(paste("Unknown margin", margin, "in fractionize_byname. margin should be 1, 2, or c(1,2)."))
  }
  unaryapply_byname(fractionize.func, a = M, margin = margin, rowcoltypes = "all")
}


#' Row sums, sorted by name
#'
#' Calculates row sums for a matrix by post-multiplying by an identity vector (containins all 1's).
#' In contrast to \code{rowSums} (which returns a \code{numeric} result),
#' the return value from \code{rowsums_byname} is a matrix.
#' An optional \code{colname} for the resulting column vector can be supplied.
#' If \code{colname} is \code{NULL} or \code{NA} (the default),
#' the column name is set to the column type as given by \code{coltype(m)}.
#'
#' @param m a matrix or list of matrices from which row sums are desired.
#' @param colname name of the output column containing row sums
#'
#' @return a column vector of type \code{matrix} containing the row sums of \code{m}
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("c", 1:2))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' rowsums_byname(m)
#' rowsums_byname(m, "E.ktoe")
#' # This also works with lists
#' rowsums_byname(list(m, m))
#' rowsums_byname(list(m, m), "E.ktoe")
#' rowsums_byname(list(m, m), NA)
#' rowsums_byname(list(m, m), NULL)
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' rowsums_byname(DF$m[[1]])
#' rowsums_byname(DF$m)
#' ans <- DF %>% mutate(rs = rowsums_byname(m))
#' ans
#' ans$rs[[1]]
#' # Nonsensical
#' \dontrun{rowsums_byname(NULL)}
rowsums_byname <- function(m, colname = NA){
  if (is.null(colname)) {
    # Set to NA so that we can try setting to coltype in rowsum.func
    colname <- NA_character_
  }
  rowsum.func <- function(m, colname){
    if (is.na(colname)) {
      colname <- coltype(m)
    }
    rowSums(m) %>%
      # Preserve matrix structure (i.e., result will be a column vector of type matrix)
      matrix(byrow = TRUE) %>%
      # Preserve row names
      setrownames_byname(rownames(m)) %>%
      # But sort the result on names
      sort_rows_cols %>%
      setcolnames_byname(colname) %>%
      setrowtype(rowtype(m)) %>%
      setcoltype(coltype(m))
  }
  unaryapply_byname(rowsum.func, a = m, colname = colname, rowcoltypes = "none")
}

#' Column sums, sorted by name
#'
#' Calculates column sums for a matrix by pre-multiplying by an identity vector (containins all 1's).
#' In contrast to \code{colSums} (which returns a \code{numeric} result),
#' the return value from \code{colsums_byname} is a matrix.
#' An optional \code{rowname} for the resulting row vector can be supplied.
#' If \code{rowname} is \code{NULL} or \code{NA} (the default),
#' the row name is set to the row type as given by \code{rowtype(m)}.
#'
#' @param m a matrix or list of matrices from which column sums are desired.
#' @param rowname name of the output row containing column sums.
#'
#' @return a row vector of type \code{matrix} containing the column sums of \code{m}.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 3:1))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' colsums_byname(m)
#' colsums_byname(m, rowname = "E.ktoe")
#' m %>% colsums_byname %>% rowsums_byname
#' # This also works with lists
#' colsums_byname(list(m, m))
#' colsums_byname(list(m, m), rowname = "E.ktoe")
#' colsums_byname(list(m, m), rowname = NA)
#' colsums_byname(list(m, m), rowname = NULL)
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' colsums_byname(DF$m[[1]])
#' colsums_byname(DF$m)
#' colsums_byname(DF$m, "sums")
#' res <- DF %>% mutate(
#'   cs = colsums_byname(m),
#'   cs2 = colsums_byname(m, rowname = "sum")
#' )
#' res$cs2
colsums_byname <- function(m, rowname = NA){
   if (is.null(rowname)) {
    # Set to NA so that we can try setting to coltype in rowsum.func
    rowname <- NA_character_
  }
  colsum.func <- function(m, rowname){
    if (is.na(rowname)) {
      rowname <- rowtype(m)
    }
    colSums(m) %>%
      # Preserve matrix structure (i.e., result will be a row vector of type matrix)
      matrix(nrow = 1) %>%
      # Preserve column names
      setcolnames_byname(colnames(m)) %>%
      # But sort the result on names
      sort_rows_cols %>%
      setrownames_byname(rowname) %>%
      setrowtype(rowtype(m)) %>%
      setcoltype(coltype(m))
  }
  unaryapply_byname(colsum.func, a = m, rowname = rowname, rowcoltypes = "none")
}

#' Sum of all elements in a matrix
#'
#' This function is equivalent to \code{m \%>\% rowsums_byname() \%>\% colsums_byname()},
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param m the matrix whose elements are to be summed
#'
#' @return the sum of all elements in \code{m} as a numeric
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' m <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' sumall_byname(m)
#' rowsums_byname(m) %>% colsums_byname
#' # Also works for lists
#' sumall_byname(list(m,m))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' sumall_byname(DF$m[[1]])
#' sumall_byname(DF$m)
#' res <- DF %>% mutate(
#'   sums = sumall_byname(m)
#' )
#' res$sums
sumall_byname <- function(m){
  sum.func <- function(m){
    m %>%
      rowsums_byname %>%
      colsums_byname %>%
      as.numeric
  }
  unaryapply_byname(sum.func, a = m, rowcoltypes = "none")
}

#' Row products, sorted by name
#'
#' Calculates row products (the product of all elements in a row) for a matrix.
#' An optional \code{colname} for the resulting column vector can be supplied.
#' If \code{colname} is \code{NULL} or \code{NA} (the default),
#' the column name is set to the column type as given by \code{coltype(M)}.
#'
#' @param M a matrix or data frame from which row products are desired.
#' @param colname name of the output column containing row products
#'
#' @return a column vector of type \code{matrix} containing the row products of \code{M}
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' M <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("c", 1:2))) %>%
#'   setrowtype("Industries") %>% setcoltype("Products")
#' rowprods_byname(M)
#' rowprods_byname(M, "E.ktoe")
#' # This also works with lists
#' rowprods_byname(list(M, M))
#' rowprods_byname(list(M, M), "E.ktoe")
#' rowprods_byname(list(M, M), NA)
#' rowprods_byname(list(M, M), NULL)
#' DF <- data.frame(M = I(list()))
#' DF[[1,"M"]] <- M
#' DF[[2,"M"]] <- M
#' rowprods_byname(DF$M[[1]])
#' rowprods_byname(DF$M)
#' ans <- DF %>% mutate(rs = rowprods_byname(M))
#' ans
#' ans$rs[[1]]
#' # Nonsensical
#' \dontrun{rowprods_byname(NULL)}
rowprods_byname <- function(M, colname = NA){
  if (is.null(colname)) {
    # Set the column name to NA so we can change it in the function.
    colname <- NA_character_
  }
  rowprod.func <- function(M, colname){
    if (is.na(colname)) {
      colname <- coltype(M)
    }
    apply(M, MARGIN = 1, FUN = prod) %>%
      # Preserve matrix structure (i.e., result will be a column vector of type matrix)
      matrix(byrow = TRUE) %>%
      # Preserve row names
      setrownames_byname(rownames(M)) %>%
      # But sort the result on names
      sort_rows_cols %>%
      setcolnames_byname(colname) %>%
      setrowtype(rowtype(M)) %>%
      setcoltype(coltype(M))
  }
  unaryapply_byname(rowprod.func, a = M, colname = colname, rowcoltypes = "none")
}

#' Column products, sorted by name
#'
#' Calculates column products (the product of all elements in a column) for a matrix.
#' An optional \code{rowname} for the resulting row vector can be supplied.
#' If \code{rowname} is \code{NULL} or \code{NA} (the default),
#' the row name is set to the row type as given by \code{rowtype(M)}.
#'
#' @param M a matrix or data frame from which column products are desired.
#' @param rowname name of the output row containing column products.
#'
#' @return a row vector of type \code{matrix} containing the column products of \strong{\code{M}}.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' M <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 3:1))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' colprods_byname(M)
#' colprods_byname(M, rowname = "E.ktoe")
#' M %>% colprods_byname %>% rowprods_byname
#' # This also works with lists
#' colprods_byname(list(M, M))
#' colprods_byname(list(M, M), rowname = "E.ktoe")
#' colprods_byname(list(M, M), rowname = NA)
#' colprods_byname(list(M, M), rowname = NULL)
#' DF <- data.frame(M = I(list()))
#' DF[[1,"M"]] <- M
#' DF[[2,"M"]] <- M
#' colprods_byname(DF$M[[1]])
#' colprods_byname(DF$M)
#' colprods_byname(DF$M, "prods")
#' res <- DF %>% mutate(
#'   cs = colprods_byname(M),
#'   cs2 = colprods_byname(M, rowname = "prod")
#' )
#' res$cs2
colprods_byname <- function(M, rowname = NA){
  if (is.null(rowname)) {
    # Set the row name to NA so we can change it in the function.
    rowname <- NA_character_
  }
  colprod.func <- function(M, rowname){
    if (is.na(rowname)) {
      rowname <- rowtype(M)
    }
    apply(M, MARGIN = 2, FUN = prod) %>%
      # Preserve matrix structure (i.e., result will be a row vector of type matrix)
      matrix(nrow = 1) %>%
      # Preserve column names
      setcolnames_byname(colnames(M)) %>%
      # But sort the result on names
      sort_rows_cols() %>%
      setrownames_byname(rowname) %>%
      setrowtype(rowtype(M)) %>%
      setcoltype(coltype(M))
  }
  unaryapply_byname(colprod.func, a = M, rowname = rowname, rowcoltypes = "none")
}

#' Product of all elements in a matrix
#'
#' This function is equivalent to \code{M \%>\% rowprods_byname() \%>\% colprods_byname()},
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param M the matrix whose elements are to be multiplied
#'
#' @return the product of all elements in \strong{\code{M}} as a numeric.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' M <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Product")
#' prodall_byname(M)
#' rowprods_byname(M) %>% colprods_byname
#' # Also works for lists
#' prodall_byname(list(M,M))
#' DF <- data.frame(M = I(list()))
#' DF[[1,"M"]] <- M
#' DF[[2,"M"]] <- M
#' prodall_byname(DF$M[[1]])
#' prodall_byname(DF$M)
#' res <- DF %>% mutate(
#'   prods = prodall_byname(M)
#' )
#' res$prods
prodall_byname <- function(M){
  prodall.func <- function(M){
    M %>%
      rowprods_byname() %>%
      colprods_byname() %>%
      as.numeric()
  }
  unaryapply_byname(prodall.func, a = M, rowcoltypes = "none")
}

#' Subtract a matrix with named rows and columns from a suitably named and sized identity matrix (\code{I})
#'
#' The order of rows and columns of \code{m} may change before subtracting from \code{I},
#' because the rows and columns are sorted by name prior to subtracting from \code{I}.
#' Furthermore, if \code{m} is not square, it will be made square
#' before subtracting from \code{I} by calling \code{complete_and_sort}.
#'
#' @param m the matrix to be subtracted from \code{I}
#'
#' @return The difference between an identity matrix (\code{I}) and \code{m}
#' (whose rows and columns have been completed and sorted)
#' @export
#' 
#' @importFrom parallel mcMap
#' @importFrom magrittr %>%
#' 
#' @examples
#' library(magrittr)
#' m <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' # Rows and columns are unsorted
#' diag(1, nrow = 2) - m 
#' # Rows and columns are sorted prior to subtracting from the identity matrix
#' Iminus_byname(m) 
#' # This also works with lists
#' Iminus_byname(list(m,m))
#' # If the m is not square before subtracting from I,
#' # it will be made square by the function complete_and_sort.
#' m2 <- matrix(c(1,2,3,4,5,6), ncol = 2, dimnames = list(c("a", "b", "c"), c("a", "b"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' Iminus_byname(m2)
Iminus_byname <- function(m){
  iminus.func <- function(m){
    A <- complete_and_sort(m) %>%
      setrowtype(rowtype(m)) %>%
      setcoltype(coltype(m))
    difference_byname(identize_byname(A), A)
  }
  unaryapply_byname(iminus.func, a = m, rowcoltypes = "all")
}


#' Cumulative sum that respects row and column names
#'
#' Provides cumulative sums along a list or column of a data frame.
#' If \code{m} is a single number, \code{m} is returned.
#' If \code{m} is a list of numbers, a list representing the cumulative sum of the numbers is returned.
#' If \code{m} is a single matrix, \code{m} is returned.
#' If \code{m} is a list of matrices, a list representing the cumulative sum
#' of the matrices is returned. 
#' In this case, each entry in the list is sum "by name," such that row and column names 
#' of the matrices are respected.
#' 
#' This function respects groups.
#'
#' @param m a number, list of numbers, matrix or list of matrices for which cumulative sum is desired
#'
#' @return a single number, list of numbers, a single matrix, or a list of matrices,
#'         depending on the nature of \code{m}
#'         
#' @export
#'
#' @examples
#' 
cumsum_byname <- function(m){
  # Check for pathological cases.
  if (length(m) == 0) {
    # Nothing to be done here.
    # Note that length(NULL) == 0, so this tests for m == NULL, too.
    return(NULL)
  }
  if (is.matrix(m)) {
    # We have a single matrix. Just return it.
    return(m)
  }
  if (length(m) == 1) {
    # Note that length(NA) == 1, so this test captures cases where m == NA.
    # Nothing to be done.
    return(m)
  }
  # length(m) > 1
  if (all(as.logical(lapply(m, is.matrix)))) {
    # Assume we have a list of matrices
    out <- list()
    out[[1]] <- m[[1]]
    for (i in 2:length(m)) {
      out[[i]] <- sum_byname(m[[i]], out[[i - 1]])
    }
    return(out)
  }
  if (all(as.logical(lapply(m, is.numeric)))) {
    # This is an easy case!
    return(as.list(cumsum(m)))
  }
  # We don't know how to handle anything else.
  stop("Unknown class(m) in cumulativesum_byname:", class(m))

}


