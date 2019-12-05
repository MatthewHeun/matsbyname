#' Absolute value of matrix elements
#'
#' @param a a matrix or list of matrices 
#'
#' @return a with each element replaced by its absolute value
#' 
#' @export
#'
#' @examples
#' abs_byname(1)
#' abs_byname(-1)
#' m <- matrix(c(-10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' abs_byname(m)
abs_byname <- function(a){
  unaryapply_byname(abs, a = a)
}

#' Logarithm of matrix elements
#' 
#' Specify the base of the log with \code{base} argument.
#'
#' @param a a matrix or list of matrices 
#' @param base the base of the logarithm (default is \code{exp(1)}, giving the natural logarithm)
#'
#' @return M with each element replaced by its base \code{base} logarithm
#' 
#' @export
#'
#' @examples
#' log_byname(exp(1))
#' m <- matrix(c(10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' log_byname(m)
#' log_byname(m, base = 10)
log_byname <- function(a, base = exp(1)){
  unaryapply_byname(log, a = a, .FUNdots = list(base = base))
}

#' Exponential of matrix elements
#' 
#' Gives the exponential of all elements of a matrix or list of matrices
#'
#' @param a a matrix of list of matrices 
#'
#' @return \code{M} with each element replaced by its exponential
#' 
#' @export
#'
#' @examples
#' exp_byname(1)
#' m <- matrix(c(log(10),log(1),log(1),log(100)), 
#'   nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' exp_byname(m)
exp_byname <- function(a){
  unaryapply_byname(exp, a = a)
}

#' Invert a matrix
#'
#' This function transposes row and column names as well as row and column types.
#' Rows and columns of \code{a} are sorted prior to inverting.
#'
#' @param a the matrix to be inverted. \code{a} must be square.
#'
#' @return the inversion of \code{a}
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' invert_byname(m)
#' matrixproduct_byname(m, invert_byname(m))
#' matrixproduct_byname(invert_byname(m), m)
#' invert_byname(list(m,m))
invert_byname <- function(a){
  unaryapply_byname(solve, a = a, rowcoltypes = "transpose")
}

#' Transpose a matrix by name
#'
#' Gives the transpose of a matrix or list of matrices
#'
#' @param a the matrix to be transposed
#'
#' @return the transposed matrix
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' transpose_byname(m)
#' transpose_byname(list(m,m))
transpose_byname <- function(a){
  unaryapply_byname(t, a = a, rowcoltypes = "transpose")
}

#' Creates a diagonal "hat" matrix from a vector
#'
#' A "hat" matrix is one in which the only non-zero elements are stored on the diagonal.
#' To "hatize" a vector is to place its elements on the diagonal of an otherwise-zero square matrix.
#' \code{v} must be a matrix object with one of its two dimensions of length 1 (i.e., a vector).
#' The names of both dimensions of the hatized matrix are the same and taken from \code{v}.
#' Note that the vector names are sorted prior to forming the "hat" matrix.
#'
#' @param v The vector from which a "hat" matrix is to be created.
#'
#' @return A square "hat" matrix with size equal to the length of \code{v}.
#' @export
#'
#' @examples
#' v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatize_byname(v)
#' hatize_byname(r)
#' # This also works with lists.
#' hatize_byname(list(v, v))
hatize_byname <- function(v){
  hatize_func <- function(v){
    # Check if this is the right size
    if (!(nrow(v) == 1 | ncol(v) == 1)) {
      stop("matrix v must have at least one dimension of length 1 in hatize_byname")
    }
    v_sorted <- sort_rows_cols(v)
    out <- diag(as.numeric(v_sorted))
    if (ncol(v) == 1) {
      rownames(out) <- rownames(v_sorted)
      colnames(out) <- rownames(v_sorted)
      # This function does not rely on unaryapply_byname to set row and column types.
      # So, we must do so here.
      out <- out %>% setrowtype(rowtype(v)) %>% setcoltype(rowtype(v))
    } else if (nrow(v) == 1) {
      rownames(out) <- colnames(v_sorted)
      colnames(out) <- colnames(v_sorted)
      # This function does not rely on unaryapply_byname to set row and column types.
      # So, we must do so here.
      out <- out %>% setrowtype(coltype(v)) %>% setcoltype(coltype(v))
    }
    return(out)
  }
  unaryapply_byname(hatize_func, a = v, rowcoltypes = "none")
}

#' Hatize and invert a vector
#' 
#' When dividing rows or columns of a matrix by elements of a vector,
#' the vector elements are placed on the diagonal of a new matrix,
#' the diagonal matrix is inverted, and
#' the result is pre- or post-multiplied into the matrix.
#' This function performs the hatizing and inverting of vector \code{v} in one step
#' and takes advantage of computational efficiencies to achieve the desired result.
#' The computational shortcut is apparent when one observes that the matrix produced by hatizing and inverting
#' a vector is a diagonal matrix whose non-zero elements are the numerical inverses of the individual elements of \code{v}.
#' So this function first inverts each element of \code{v} then places the inverted elements on the diagonal of a diagonal matrix.
#' 
#' Note that this function gives the same result as \code{invert_byname(hatize_byname(v))},
#' except that \code{invert_byname(hatize_byname(v))} fails due to a singular matrix error
#' when any of the elements of \code{v} are zero.
#' This function will give \code{inf_becomes} on the diagonal of the result for each zero element of \code{v},
#' arguably a better answer.
#' The sign of \code{Inf} is preserved in the substitution.
#' The default value of \code{inf_becomes} is \code{.Machine$double.xmax}.
#' Set \code{inf_becomes} to \code{NULL} to disable this behavior.
#' 
#' The default behavior is helpful for cases when the result of \code{hatinv_byname} is later multiplied by \code{0}
#' to obtain \code{0}.
#' Multiplying \code{Inf} by \code{0} gives \code{NaN} which would effectively end the stream of calculations.
#' 
#' @param v the vector to be hatized and inverted
#' @param inf_becomes a value to be substitute for any \code{Inf} produced by the inversion process. 
#'        Default is \code{.Machine$double.xmax}.
#'        If \code{FALSE} (the default), \code{Inf} is not handled differently.
#'        If \code{TRUE}, \code{Inf} values in the resulting matrix are converted to zeroes.
#'
#' @return a square diagonal matrix with inverted elements of \code{v} on the diagonal
#' 
#' @export
#'
#' @examples
#' v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatinv_byname(v)
#' hatinv_byname(r)
#' # This function also works with lists.
#' hatinv_byname(list(v, v))
#' # Watch out for 0 values
#' v2 <- matrix(0:1, ncol = 1, dimnames = list(c(paste0("i", 0:1)), c("p1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' # Produces singular matrix error
#' \dontrun{v2 %>% hatize_byname() %>% invert_byname}
#' # Handles 0 values well
#' hatinv_byname(v2)
#' hatinv_byname(v2, inf_becomes = 42)
#' hatinv_byname(v2, inf_becomes = NULL)
hatinv_byname <- function(v, inf_becomes = .Machine$double.xmax){
  hatinv_func <- function(v){
    # Note: there is no need to check that v is, indeed, a vector here.
    # hatize_byname() does that check for us.
    v_inv <- 1/v
    if (!is.null(inf_becomes)) {
      v_inv[v_inv == Inf] <- inf_becomes
      v_inv[v_inv == -Inf] <- -inf_becomes
    }
    hatize_byname(v_inv)
  }
  unaryapply_byname(hatinv_func, a = v, rowcoltypes = "none")
}

#' Named identity matrix or vector
#'
#' Creates an identity matrix (\strong{I}) or vector (\strong{i}) of same size and with same names and
#' same row and column types as \code{a}.
#' If \code{margin = 1}, makes a column matrix filled with \code{1}s. 
#' Row names and type are taken from row names and type of \code{a}.
#' Column name and type are same as column type of \code{a}.
#' If \code{margin = 2}, make a row matrix filled with \code{1}s.
#' Column names and type are taken from column name and type of \code{a}.
#' Row name and type are same as row type of \code{a}.
#' If \code{c(1,2)} (the default), make an identity matrix with \code{1}s on the diagonal.
#' Row and column names are sorted on output.
#'
#' @param a the matrix whose names and dimensions are to be preserved in an identity matrix or vector
#' @param margin determines whether an identity vector or matrix is returned. See details.
#'
#' @return An identity matrix or vector.
#' 
#' @export
#'
#' @examples
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
identize_byname <- function(a, margin = c(1,2)){
  identize_func <- function(a, margin){
    if (inherits(a, "numeric") & length(a) == 1) {
      # Assume we have a single number here
      # Thus, we return 1.
      return(1)
    }
    
    if (!length(margin) %in% c(1,2)) {
      stop("margin should have length 1 or 2 in fractionize_byname")
    }
    
    if (length(margin) == 2 && all(margin %in% c(1,2))) {
      # a is a matrix. 
      # Return the identity matrix with 1's on diagonal,
      # of same dimensions as a
      # and same names and types as a.
      stopifnot(nrow(a) == ncol(a))
      return(diag(nrow(a)) %>% 
               setrownames_byname(rownames(a)) %>% setcolnames_byname(colnames(a)) %>% 
               setrowtype(rowtype(a)) %>% setcoltype(coltype(a)))
    }
    
    if (length(margin) != 1 || !(margin %in% c(1,2))) {
      stop(paste("Unknown margin", margin, "in identize_byname. margin should be 1, 2, or c(1,2)."))
    }
    
    if (margin == 1)  {
      # Return a column vector containing 1's
      return(matrix(rep_len(1, nrow(a)), nrow = nrow(a), ncol = 1) %>% 
               setrownames_byname(rownames(a)) %>% setcolnames_byname(coltype(a)) %>% 
               setrowtype(rowtype(a)) %>% setcoltype(coltype(a)))
    }
    if (margin == 2) {
      # Return a row vector containing 1's
      return(matrix(rep_len(1, ncol(a)), nrow = 1, ncol = ncol(a)) %>% 
               setrownames_byname(rowtype(a)) %>% setcolnames_byname(colnames(a)) %>% 
               setrowtype(rowtype(a)) %>% setcoltype(coltype(a)))
    } 
    # Should never get here, but just in case:
    # stop(paste("Unknown margin", margin, "in identize_byname. margin should be 1, 2, or c(1,2)."))
  }
  unaryapply_byname(identize_func, a = a, .FUNdots = list(margin = margin), 
                    rowcoltypes = "none")
}

#' Compute fractions of matrix entries
#' 
#' This function divides all entries in \code{a} by the specified sum,
#' thereby "fractionizing" the matrix.
#'
#' @param a the matrix to be fractionized
#' @param margin If \code{1} (rows), each entry in \code{a} is divided by its row's sum.
#' If \code{2} (columns), each entry in \code{a} is divided by its column's sum.
#' If \code{c(1,2)} (both rows and columns), 
#' each entry in \code{a} is divided by the sum of all entries in \code{a}.
#'
#' @return a fractionized matrix of same dimensions and same row and column types as \code{a}.
#' @export
#'
#' @examples
#' M <- matrix(c(1, 5,
#'               4, 5),
#'             nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
#'             setcoltype("Products") %>% setrowtype("Industries")
#' fractionize_byname(M, margin = c(1,2))
#' fractionize_byname(M, margin = 1)
#' fractionize_byname(M, margin = 2)
fractionize_byname <- function(a, margin){
  fractionize_func <- function(a, margin){
    if (!inherits(a, "matrix") && !inherits(a, "data.frame")) {
      # Assume we have a single number here
      # By dividing a by itself, we could throw a division by zero error,
      # which we would want to do.
      return(a/a)
    }
    if (length(margin) != length(unique(margin))) {
      stop("margin should contain unique integers in fractionize_byname")
    }
    if (!length(margin) %in% c(1,2)) {
      stop("margin should have length 1 or 2 in fractionize_byname")
    }
    
    if (length(margin) == 2 && all(margin %in% c(1,2))) {
      return(a/sumall_byname(a))
    }
    
    if (length(margin) != 1 || !margin %in% c(1,2)) {
      stop(paste("Unknown margin", margin, "in fractionize_byname. margin should be 1, 2, or c(1,2)."))
    }
    
    if (margin == 1) {
      # Divide each entry by its row sum
      # Do this with (a*i)_hat_inv * a
      # return(matrixproduct_byname(a %>% rowsums_byname %>% hatize_byname %>% invert_byname, a))
      return(sweep(a, margin, rowSums(a), `/`))
    }
    if (margin == 2) {
      # Divide each entry by its column sum
      # Do this with a * (i^T * a)_hat_inv
      # return(matrixproduct_byname(a, colsums_byname(a) %>% hatize_byname %>% invert_byname))
      return(sweep(a, margin, colSums(a), `/`))
    } 
    # Should never get here, but just in case:
    # stop(paste("Unknown margin", margin, "in fractionize_byname. margin should be 1, 2, or c(1,2)."))
  }
  unaryapply_byname(fractionize_func, a = a, .FUNdots = list(margin = margin), 
                    rowcoltypes = "all")
}


#' Row sums, sorted by name
#'
#' Calculates row sums for a matrix by post-multiplying by an identity vector (containing all 1's).
#' In contrast to \code{rowSums} (which returns a \code{numeric} result),
#' the return value from \code{rowsums_byname} is a matrix.
#' An optional \code{colname} for the resulting column vector can be supplied.
#' If \code{colname} is \code{NULL} or \code{NA} (the default),
#' the column name is set to the column type as given by \code{coltype(a)}.
#'
#' @param a a matrix or list of matrices from which row sums are desired.
#' @param colname name of the output column containing row sums
#'
#' @return a column vector of type \code{matrix} containing the row sums of \code{m}
#' @export
#'
#' @examples
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
rowsums_byname <- function(a, colname = NA){
  if (is.null(colname)) {
    # Set to NA so that we can try setting to coltype in rowsum.func
    colname <- NA_character_
  }
  rowsum_func <- function(a, colname){
    if (is.na(colname)) {
      colname <- coltype(a)
    }
    rowSums(a) %>%
      # Preserve matrix structure (i.e., result will be a column vector of type matrix)
      matrix(ncol = 1) %>%
      # Preserve row names
      setrownames_byname(rownames(a)) %>%
      # Set column name
      setcolnames_byname(colname) %>%
      # But sort the result on names
      sort_rows_cols() %>%
      # Set types
      setrowtype(rowtype(a)) %>%
      setcoltype(coltype(a))
  }
  unaryapply_byname(rowsum_func, a = a, .FUNdots = list(colname = colname), 
                    rowcoltypes = "none")
}

#' Column sums, sorted by name
#'
#' Calculates column sums for a matrix by premultiplying by an identity vector (containing all 1's).
#' In contrast to \code{colSums} (which returns a \code{numeric} result),
#' the return value from \code{colsums_byname} is a matrix.
#' An optional \code{rowname} for the resulting row vector can be supplied.
#' If \code{rowname} is \code{NULL} or \code{NA} (the default),
#' the row name is set to the row type as given by \code{rowtype(a)}.
#'
#' @param a a matrix or list of matrices from which column sums are desired.
#' @param rowname name of the output row containing column sums.
#'
#' @return a row vector of type \code{matrix} containing the column sums of \code{a}.
#' @export
#'
#' @examples
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
colsums_byname <- function(a, rowname = NA){
  a %>% 
    transpose_byname() %>% 
    rowsums_byname(colname = rowname) %>% 
    transpose_byname()
}

#' Sum of all elements in a matrix
#'
#' This function is equivalent to \code{a \%>\% rowsums_byname() \%>\% colsums_byname()},
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param a the matrix whose elements are to be summed
#'
#' @return the sum of all elements in \code{a} as a numeric
#' @export
#'
#' @examples
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
sumall_byname <- function(a){
  sum_func <- function(a){
    a %>%
      rowsums_byname %>%
      colsums_byname %>%
      as.numeric
  }
  unaryapply_byname(sum_func, a = a, rowcoltypes = "none")
}

#' Row products, sorted by name
#'
#' Calculates row products (the product of all elements in a row) for a matrix.
#' An optional \code{colname} for the resulting column vector can be supplied.
#' If \code{colname} is \code{NULL} or \code{NA} (the default),
#' the column name is set to the column type as given by \code{coltype(a)}.
#'
#' @param a a matrix or list of matrices from which row products are desired.
#' @param colname name of the output column containing row products
#'
#' @return a column vector of type \code{matrix} containing the row products of \code{a}
#' 
#' @export
#'
#' @examples
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
rowprods_byname <- function(a, colname = NA){
  if (is.null(colname)) {
    # Set the column name to NA so we can change it in the function.
    colname <- NA_character_
  }
  rowprod_func <- function(a, colname){
    if (is.na(colname)) {
      colname <- coltype(a)
    }
    apply(a, MARGIN = 1, FUN = prod) %>%
      # Preserve matrix structure (i.e., result will be a column vector of type matrix)
      matrix(byrow = TRUE) %>%
      # Preserve row names
      setrownames_byname(rownames(a)) %>%
      # But sort the result on names
      sort_rows_cols %>%
      setcolnames_byname(colname) %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(coltype(a))
  }
  unaryapply_byname(rowprod_func, a = a, .FUNdots = list(colname = colname), 
                    rowcoltypes = "none")
}

#' Column products, sorted by name
#'
#' Calculates column products (the product of all elements in a column) for a matrix.
#' An optional \code{rowname} for the resulting row vector can be supplied.
#' If \code{rowname} is \code{NULL} or \code{NA} (the default),
#' the row name is set to the row type as given by \code{rowtype(a)}.
#'
#' @param a a matrix or data frame from which column products are desired.
#' @param rowname name of the output row containing column products.
#'
#' @return a row vector of type \code{matrix} containing the column products of \code{a}.
#' 
#' @export
#'
#' @examples
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
colprods_byname <- function(a, rowname = NA){
  a %>% transpose_byname() %>% 
    rowprods_byname(colname = rowname) %>% 
    transpose_byname()
}

#' Product of all elements in a matrix
#'
#' This function is equivalent to \code{a \%>\% rowprods_byname() \%>\% colprods_byname()},
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param a the matrix whose elements are to be multiplied
#'
#' @return the product of all elements in \code{a} as a numeric.
#' 
#' @export
#'
#' @examples
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
prodall_byname <- function(a){
  prodall_func <- function(a){
    a %>%
      rowprods_byname() %>%
      colprods_byname() %>%
      as.numeric()
  }
  unaryapply_byname(prodall_func, a = a, rowcoltypes = "none")
}

#' Subtract a matrix with named rows and columns from a suitably named and sized identity matrix (\code{I})
#'
#' The order of rows and columns of \code{m} may change before subtracting from \code{I},
#' because the rows and columns are sorted by name prior to subtracting from \code{I}.
#' Furthermore, if \code{m} is not square, it will be made square
#' before subtracting from \code{I} by calling \code{complete_and_sort}.
#'
#' @param a the matrix to be subtracted from \code{I}
#'
#' @return The difference between an identity matrix (\code{I}) and \code{m}
#' (whose rows and columns have been completed and sorted)
#' 
#' @export
#' 
#' @examples
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
Iminus_byname <- function(a){
  iminus_func <- function(a){
    A <- complete_and_sort(a) %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(coltype(a))
    difference_byname(identize_byname(A), A)
  }
  unaryapply_byname(iminus_func, a = a, rowcoltypes = "all")
}


#' Cumulative sum that respects row and column names
#'
#' Provides cumulative sums along a list or column of a data frame.
#' If \code{a} is a single number, \code{a} is returned.
#' If \code{a} is a list of numbers, a list representing the cumulative sum of the numbers is returned.
#' If \code{a} is a single matrix, \code{a} is returned.
#' If \code{a} is a list of matrices, a list representing the cumulative sum
#' of the matrices is returned. 
#' In this case, each entry in the returned list is sum "by name," 
#' such that row and column names of the matrices are respected.
#' 
#' If cumulative sums are desired in the context of a data frame, 
#' groups in the data frame are respected if \code{mutate} is used.
#' See examples.
#'
#' @param a a number, list of numbers, matrix or list of matrices for which cumulative sum is desired
#'
#' @return a single number, list of numbers, a single matrix, or a list of matrices,
#'         depending on the nature of \code{a}
#'         
#' @export
#'
#' @examples
#' library(dplyr)
#' m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' cumsum_byname(list(m1, m2, m3))
#' # Groups are respected in the context of mutate.
#' data.frame(grp = c("A", "A", "B"), m = I(list(m1, m2, m3))) %>% group_by(grp) %>% 
#'   mutate(m2 = cumsum_byname(m))
cumsum_byname <- function(a){
  cumapply_byname(FUN = sum_byname, a)
}

#' Cumulative element-product that respects row and column names
#'
#' Provides cumulative element-products along a list or column of a data frame.
#' If \code{a} is a single number, \code{a} is returned.
#' If \code{a} is a list of numbers, a list representing the cumulative product of the numbers is returned.
#' If \code{a} is a single matrix, \code{a} is returned.
#' If \code{a} is a list of matrices, a list representing the cumulative product
#' of the matrices is returned. 
#' In this case, each entry in the returned list is product "by name," 
#' such that row and column names of the matrices are respected.
#' 
#' This function respects groups if \code{a} is a variable in a data frame.
#'
#' @param a a number, list of numbers, matrix or list of matrices for which cumulative element product is desired
#'
#' @return a single number, list of numbers, a single matrix, or a list of matrices,
#'         depending on the nature of \code{a}
#'         
#' @export
#'
#' @examples
#' cumprod_byname(list(1, 2, 3, 4, 5))
#' m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>%
#'   setrowtype("row") %>% setcoltype("col")
#' m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>%
#'   setrowtype("row") %>% setcoltype("col")
#' m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>%
#'   setrowtype("row") %>% setcoltype("col")
#' cumprod_byname(list(m1, m2, m3))
cumprod_byname <- function(a){
  cumapply_byname(FUN = hadamardproduct_byname, a)
}


#' Replace NaN values with a value
#'
#' In a matrix or within matrices in a list, 
#' replace all \code{NaN} matrix values with \code{val}.
#' 
#' @param a a matrix of list of matrices in which \code{NaN} will be replaced by \code{val}
#' @param val \code{NaN}s are replace by \code{val}
#'
#' @return a matrix or list of matrices in which all \code{NaN} are replaced by \code{val}
#' 
#' @export
#'
#' @examples
#' suppressWarnings(a <- matrix(c(1, sqrt(-1))))
#' replaceNaN_byname(a)
#' replaceNaN_byname(a, 42)
replaceNaN_byname <- function(a, val = 0){
  replace_func <- function(a){
    a[is.nan(a)] <- val
    return(a)
  }
  unaryapply_byname(replace_func, a = a, rowcoltypes = "all")
}


#' Count the number of matrix entries that meet a criterion
#' 
#' Expressions can be written in a natural way such as 
#' \code{count_vals_byname(m, "<=", 1)}.
#' 
#' Either a single matrix or a list of matrices can be given as the \code{a} argument.
#' \code{compare_fun} can be specified as a string (\code{"!="})
#' or as a back-quoted function (\code{`!=`}).
#' 
#' @param a a matrix or list of matrices whose values are to be counted according to \code{compare_fun}
#' @param compare_fun the comparison function, one of "\code{==}", "\code{!=}", 
#'        "\code{<}", "\code{<=}", "\code{>}", or "\code{>=}". Default is "\code{==}".
#' @param val the value against which matrix entries are compared. Default is \code{0}.
#'
#' @return an integer indicating the number of entries in \code{a} 
#'         that meet the specified criterion
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
#' count_vals_byname(m) # uses defaults: compare_fun = "==" and val = 0
#' count_vals_byname(m, compare_fun = "!=")
#' count_vals_byname(m, compare_fun = `!=`)
#' # Write expressions in a natural way
#' count_vals_byname(m, "<=", 1)
#' # Also works for lists
#' count_vals_byname(list(m,m), "<=", 1)
count_vals_byname <- function(a, compare_fun = c("==", "!=", "<", "<=", ">=", ">"), val = 0){
  sumall_byname(compare_byname(a, compare_fun, val))  
}


#' Count the number of matrix entries in rows that meet a criterion
#' 
#' Expressions can be written in a natural way such as 
#' \code{count_vals_inrows_byname(m, "<=", 1)}.
#' 
#' Either a single matrix or a list of matrices can be given as the \code{a} argument.
#' \code{compare_fun} can be specified as a string (\code{"!="})
#' or as a back-quoted function (\code{`!=`}).
#' 
#' @param a a matrix or list of matrices whose values are to be counted by rows according to \code{compare_fun}
#' @param compare_fun the comparison function, one of "\code{==}", "\code{!=}", 
#'        "\code{<}", "\code{<=}", "\code{>}", or "\code{>=}". Default is "\code{==}".
#' @param val the value against which matrix entries are compared. Default is \code{0}.
#'
#' @return an \code{matrix} with a single column indicating the number of entries in \code{a} 
#'         that meet the specified criterion in each row of \code{a}
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
#' count_vals_inrows_byname(m) # uses defaults: compare_fun = "==" and val = 0
#' count_vals_inrows_byname(m, compare_fun = "!=")
#' count_vals_inrows_byname(m, compare_fun = `!=`)
#' # Write expressions in a natural way
#' count_vals_inrows_byname(m, "<=", 1)
#' # Also works for lists
#' count_vals_inrows_byname(list(m,m), "<=", 1)
count_vals_inrows_byname <- function(a, compare_fun = c("==", "!=", "<", "<=", ">=", ">"), val = 0){
  rowsums_byname(compare_byname(a, compare_fun, val))  
}


#' Count the number of matrix entries in columns that meet a criterion
#' 
#' Expressions can be written in a natural way such as 
#' \code{count_vals_incols_byname(m, "<=", 1)}.
#' 
#' Either a single matrix or a list of matrices can be given as the \code{a} argument.
#' \code{compare_fun} can be specified as a string (\code{"!="})
#' or as a back-quoted function (\code{`!=`}).
#' 
#' @param a a matrix or list of matrices whose values are to be counted by columns 
#'        according to \code{compare_fun}
#' @param compare_fun the comparison function, one of "\code{==}", "\code{!=}", 
#'        "\code{<}", "\code{<=}", "\code{>}", or "\code{>=}". Default is "\code{==}"
#' @param val the value against which matrix entries are compared. Default is \code{0}.
#'
#' @return an \code{matrix} with a single row indicating the number of entries in \code{a} 
#'         that meet the specified criterion in each column of \code{a}
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
#' count_vals_incols_byname(m) # uses defaults: compare_fun = "==" and val = 0
#' count_vals_incols_byname(m, compare_fun = "!=")
#' count_vals_incols_byname(m, compare_fun = `!=`)
#' # Write expressions in a natural way
#' count_vals_incols_byname(m, "<=", 1)
#' # Also works for lists
#' count_vals_incols_byname(list(m,m), "<=", 1)
count_vals_incols_byname <- function(a, compare_fun = c("==", "!=", "<", "<=", ">=", ">"), val = 0){
  colsums_byname(compare_byname(a, compare_fun, val))  
}


#' Compare matrix entries to a value
#' 
#' Compares matrix entries to a value, 
#' returning a matrix of same size as \code{a}
#' containing \code{TRUE} or \code{FALSE} values
#' as the result of applying \code{compare_fun} and \code{val}
#' to all entries in \code{a}.
#'
#' @param a a matrix or list of matrices whose values are to be counted according to \code{compare_fun}
#' @param compare_fun the comparison function, one of "\code{==}", "\code{!=}", 
#'        "\code{<}", "\code{<=}", "\code{>=}", or "\code{>}". Default is "\code{==}".
#' @param val a single value against which entries in matrix \code{a} are compared. Default is \code{0}.
#'
#' @return a logical matrix of same size as \code{a} containing \code{TRUE} where the criterion is met,
#'         \code{FALSE} otherwise
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
#' compare_byname(m, "<", 3)
#' compare_byname(list(m,m), "<", 3)
compare_byname <- function(a, compare_fun = c("==", "!=", "<", "<=", ">=", ">"), val = 0){
  if (!is.function(compare_fun)) {
    compare_fun <- match.arg(compare_fun)
  }
  compare_fun <- match.fun(compare_fun)
  test_func <- function(a,  compare_fun, val){
    # At this point, a should be an individual matrix.
    compare_fun(a, val)
  }
  unaryapply_byname(FUN = test_func, a = a, 
                    .FUNdots = c(compare_fun = compare_fun, val = val), rowcoltypes = "all")
}


#' Are all matrix elements \code{TRUE}?
#' 
#' Tells whether all elements in matrix \code{a} are true.
#' 
#' \code{a} can be a matrix or a list of matrices.
#'
#' @param a a matrix or list of matrices
#'
#' @return \code{TRUE} if all elements of \code{a} are \code{TRUE}, \code{FALSE} otherwise
#' 
#' @export
#'
#' @examples
#' all_byname(matrix(rep(TRUE, times = 4), nrow = 2, ncol = 2))
#' all_byname(matrix(c(TRUE, FALSE), nrow = 2, ncol = 1))
all_byname <- function(a){
  all_func <- function(a){
    all(a)
  }
  unaryapply_byname(FUN = all_func, a = a, rowcoltypes = "none")
}

#' Are any matrix elements \code{TRUE}?
#' 
#' Tells whether any elements in matrix \code{a} are true.
#' 
#' \code{a} can be a matrix or a list of matrices.
#'
#' @param a a matrix or list of matrices
#'
#' @return \code{TRUE} if any elements of \code{a} are \code{TRUE}, \code{FALSE} otherwise
#' 
#' @export
#'
#' @examples
#' any_byname(matrix(c(TRUE, FALSE), nrow = 2, ncol = 1))
#' any_byname(matrix(rep(FALSE, times = 4), nrow = 2, ncol = 2))
any_byname <- function(a){
  any_func <- function(a){
    any(a)
  }
  unaryapply_byname(FUN = any_func, a = a, rowcoltypes = "none")
}