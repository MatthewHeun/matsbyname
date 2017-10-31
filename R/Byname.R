library(parallel)
library(magrittr)
library(dplyr)

#' Name-wise addition of matrices.
#' 
#' @param augend Addend matrix or constant
#' @param addend Augend matrix or constant
#' 
#' Performs a union and sorting of row and column names prior to summation.
#' Zeroes are inserted for missing matrix elements.
#'
#' @return A matrix representing the name-wise sum of \code{addend} and \code{augend}
#' @export
#' 
#' @examples
#' sum_byname(2, 2)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>% 
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U + G # Non-sensical.  Row and column names not respected.
#' sum_byname(U, G)
#' sum_byname(U, 100)
#' sum_byname(200, G)
#' V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, commoditynames)) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' U + V # row and column names are non-sensical and blindly taken from first argument (U)
#' \dontrun{sum_byname(U, V)} # Fails, because row and column types are different
#' # This also works with lists
#' sum_byname(list(U,U), list(G,G))
#' sum_byname(list(U,U), list(100,100))
#' sum_byname(list(U,U), as.list(rep_len(100, 2)))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' sum_byname(DF$U, DF$G)
#' DF %>% mutate(sums = sum_byname(U, G))
#' sum_byname(U) # If only one argument, return it.
#' sum_byname(2, NULL) # Gives 2
#' sum_byname(2, NA)   # Gives NA
#' sum_byname(NULL, 1) # Gives 1
#' sum_byname(list(NULL, 1), list(1, 1))
#' DF2 <- data.frame(U = I(list()), G = I(list()))
#' DF2[[1,"U"]] <- NULL
#' DF2[[2,"U"]] <- U
#' DF2[[1,"G"]] <- G
#' DF2[[2,"G"]] <- G
#' sum_byname(DF2$U, DF2$G)
#' DF3 <- DF2 %>% mutate(sums = sum_byname(U, G))
#' DF3
#' DF3$sums[[1]]
#' DF3$sums[[2]]
sum_byname <- function(augend, addend){
  if (missing(addend)){
    return(augend)
  }
  if (is.null(addend)){
    return(augend)
  }
  if (missing(augend)){
    return(addend)
  }
  if (is.null(augend)){
    return(addend)
  }
  args <- organize_args(augend, addend)
  augend <- args$a
  addend <- args$b
  if (is.list(augend) & is.list(addend)){
    return(mcMap(sum_byname, augend, addend))
  }
  (augend + addend) %>%
    setrowtype(rowtype(augend)) %>%
    setcoltype(coltype(augend))
}

#' Name-wise subtraction of matrices.
#' 
#' @param minuend Minuend matrix or constant
#' @param subtrahend Subtrahend matrix or constant
#' 
#' Performs a union and sorting of row and column names prior to differentiation.
#' Zeroes are inserted for missing matrix elements.
#'
#' @return A matrix representing the name-wise difference between \code{minuend} and \code{subtrahend}
#' @export
#' 
#' @examples
#' difference_byname(100, 50)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U - G # Non-sensical. Row and column names not respected.
#' difference_byname(U, G) # Row and column names respected! Should be all zeroes.
#' difference_byname(100, U)
#' difference_byname(10, G)
#' difference_byname(G) # When subtrahend is missing, return minuend (in this case, G).
#' difference_byname(subtrahend = G) # When minuend is missing, return - subtrahend (in this case, -G)
#' # This also works with lists
#' difference_byname(list(100, 100), list(50, 50))
#' difference_byname(list(U,U), list(G,G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' difference_byname(DF$U, DF$G)
#' DF %>% mutate(diffs = difference_byname(U, G))
difference_byname <- function(minuend, subtrahend){
  if (missing(minuend)){
    return(elementproduct_byname(-1, subtrahend))
  }
  if (is.null(minuend)){
    return(elementproduct_byname(-1, subtrahend))
  }
  if (missing(subtrahend)){
    return(minuend)
  }
  if (is.null(subtrahend)){
    return(minuend)
  }
  args <- organize_args(minuend, subtrahend)
  minuend <- args$a
  subtrahend <- args$b
  if (is.list(minuend) & is.list(subtrahend)){
    return(mcMap(difference_byname, minuend, subtrahend))
  }
  (minuend - subtrahend) %>%
    setrowtype(rowtype(minuend)) %>%
    setcoltype(coltype(minuend))
}

#' Name-wise matrix multiplication
#' 
#' @param multiplicand Multiplicand matrix
#' @param multiplier Multiplier matrix
#' 
#' Performs a union and sorting of multiplicand rows and multiplier columns by name prior to multiplication.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that 
#' the dimensions of the \code{multiplicand} and \code{multiplier} will be conformable.
#' I.e., the number of columns in \code{multiplicand}
#' will equal the number of rows in \code{multiplier},
#' so long as the column names of multiplicand are unique and 
#' the row names of multiplier are unique.
#' If column type of \code{multiplicand} is not same as 
#' row type of \code{multiplier}, 
#' the function will fail.
#' The result is matrix product 
#' with row names from \code{multiplicand} and column names from \code{multiplier}.
#'
#' @return A matrix representing the name-wise product of \code{multiplicand} and \code{multiplier}
#' @export
#'
#' @examples
#' V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("c1", "c2", "c3"))) %>% 
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' G <- matrix(1:4, ncol = 2, dimnames = list(c("c2", "c1"), c("g2", "g1"))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' matrixproduct_byname(V, G) # Succeeds because G is completed to include a row named c3 (that contains zeroes).
#' \dontrun{V %*% G} # Fails because E lacks a row named c3.
#' # This also works with lists
#' matrixproduct_byname(list(V,V), list(G,G))
#' DF <- data.frame(V = I(list()), G = I(list()))
#' DF[[1,"V"]] <- V
#' DF[[2,"V"]] <- V
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' matrixproduct_byname(DF$V, DF$G)
#' DF %>% mutate(matprods = matrixproduct_byname(V, G))
matrixproduct_byname <- function(multiplicand, multiplier){
  if (is.list(multiplicand) & is.list(multiplier)){
    stopifnot(length(multiplicand) == length(multiplier))
    return(mcMap(matrixproduct_byname, multiplicand, multiplier))
  }
  # Check that multiplicand columns and multiplier rows are same type
  stopifnot(coltype(multiplicand) == rowtype(multiplier))
  matrices <- complete_and_sort(multiplicand, transpose_byname(multiplier), margin = 2)
  matrices$m2 <- transpose_byname(matrices$m2) # Need to re-transpose, because of transpose_byname above.
  matrices$m1 %*% matrices$m2 %>%
    setrowtype(rowtype(multiplicand)) %>% 
    setcoltype(coltype(multiplier))
}

#' Name-wise matrix element multiplication
#' 
#' @param multiplicand Multiplicand matrix or constant
#' @param multiplier Multiplier matrix or constant
#' 
#' Performs a union and sorting of names of rows and columns for both \code{multiplicand} and \code{multiplier}
#' prior to element multiplication.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that 
#' the dimensions of the \code{multiplicand} and \code{multiplier} will be conformable.
#'
#' @return A matrix representing the name-wise element product of \code{multiplicand} and \code{multiplier}
#' @export
#'
#' @examples
#' elementproduct_byname(2, 2)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U * G # Not what is desired, because names aren't aligned
#' elementproduct_byname(U, G)
#' elementproduct_byname(U, 0)
#' elementproduct_byname(0, G)
#' # This also works with lists
#' elementproduct_byname(list(U, U), list(G, G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' elementproduct_byname(DF$U, DF$G)
#' DF %>% mutate(elementprods = elementproduct_byname(U, G))
elementproduct_byname <- function(multiplicand, multiplier){
  args <- organize_args(multiplicand, multiplier)
  multiplicand <- args$a
  multiplier <- args$b
  if (is.list(multiplicand) & is.list(multiplier)){
    return(mcMap(elementproduct_byname, multiplicand, multiplier))
  }
  (multiplicand * multiplier) %>%
    setrowtype(rowtype(multiplicand)) %>%
    setcoltype(coltype(multiplicand))
}

#' Name-wise matrix element division
#' 
#' @param dividend Dividend matrix or constant
#' @param divisor Divisor matrix or constant
#' 
#' Performs a union and sorting of names of rows and columns for both \code{dividend} and \code{divisor}
#' prior to element division.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that 
#' the dimensions of the \code{dividend} and \code{divisor} will be conformable.
#'
#' @return A matrix representing the name-wise element quotient of \code{dividend} and \code{divisor}
#' @export
#'
#' @examples
#' elementquotient_byname(100, 50)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U / G # Non-sensical.  Names aren't aligned
#' elementquotient_byname(U, G)
#' elementquotient_byname(U, 10)
#' elementquotient_byname(10, G)
#' # This also works with lists
#' elementquotient_byname(10, list(G,G))
#' elementquotient_byname(list(G,G), 10)
#' elementquotient_byname(list(U, U), list(G, G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' elementquotient_byname(DF$U, DF$G)
#' DF %>% mutate(elementquotients = elementquotient_byname(U, G))
elementquotient_byname <- function(dividend, divisor){
  args <- organize_args(dividend, divisor)
  dividend <- args$a
  divisor <- args$b
  if (is.list(dividend) & is.list(divisor)){
    return(mcMap(elementquotient_byname, dividend, divisor))
  }
  (dividend / divisor) %>%
    setrowtype(rowtype(dividend)) %>%
    setcoltype(coltype(dividend))
}

#' Invert a matrix
#'
#' This function transposes row and column names as well as row and column types.
#' 
#' @param m the matrix to be inverted. \code{m} must be square.
#'
#' @return the inversion 
#' @export
#'
#' @examples
#' m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' invert_byname(m)
#' matrixproduct_byname(m, invert_byname(m))
#' matrixproduct_byname(invert_byname(m), m)
#' invert_byname(list(m,m))
invert_byname <- function(m){
  if (is.list(m)){
    return(mcMap(invert_byname, m))
  }
  stopifnot(nrow(m) == ncol(m))
  sort_rows_cols(m) %>%
    solve %>%
    setrowtype(coltype(m)) %>%
    setcoltype(rowtype(m))
}

#' Transpose a matrix by name
#'
#' @param m the matrix to be transposed
#'
#' @return the transposed matrix
#' @export
#'
#' @examples
#' m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' transpose_byname(m)
#' transpose_byname(list(m,m))
transpose_byname <- function(m){
  if (is.list(m)){
    return(mcMap(transpose_byname, m))
  }
  sort_rows_cols(m) %>%
    t %>%
    setrowtype(coltype(m)) %>%
    setcoltype(rowtype(m))
}

#' Creates a diagonal "hat" matrix from a vector.
#'
#' @param v The vector from which a "hat" matrix is to be created.
#' 
#' A "hat" matrix is one in which the only non-zero elements are stored on the diagonal.
#' To "hatize" a vector is to place its elements on the diagonal of an otherwise-zero square matrix.
#' \code{v} must be a matrix with at least one dimension of length 1 (a vector).
#' The names of both dimensions of the hatized matrix are the same and taken from \code{v}.
#' Note that the vector names are sorted prior to forming the "hat" matrix
#'
#' @return A square "hat" matrix with size equal to the length of \code{v}.
#' @export
#'
#' @examples
#' v <- matrix(1:10, ncol = 1, dimnames=list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' r <- matrix(1:5, nrow = 1, dimnames=list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatize_byname(v)
#' hatize_byname(r)
#' # This also works with lists.
#' hatize_byname(list(v, v))
hatize_byname <- function(v){
  if (is.list(v)){
    return(mcMap(hatize_byname, v))
  }
  v_sorted <- sort_rows_cols(v) %>% setrowtype(rowtype(v)) %>% setcoltype(coltype(v))
  out <- OpenMx::vec2diag(v_sorted)
  if (ncol(v) == 1){
    rownames(out) <- rownames(v_sorted)
    colnames(out) <- rownames(v_sorted)
    out <- out %>% setrowtype(rowtype(v)) %>% setcoltype(rowtype(v))
  } else if (nrow(v) == 1){
    rownames(out) <- colnames(v)
    colnames(out) <- colnames(v)
    out <- out %>% setrowtype(coltype(v)) %>% setcoltype(coltype(v))
  } else {
    stop("matrix v must have at least one dimension of length 1 in hatize")
  }
  return(out)
}

#' Named identity matrix
#'
#' Creates an identity matrix (I) with ones on the diagonal with same size and names as \code{m}.
#' \code{m} must be square.
#' 
#' @param m The matrix whose names and dimensions are to be preserved in an identity matrix.
#'
#' @return A square identity matrix with ones on the diagonal. 
#' Row and column names are taken from row and column
#' names of \code{m}.
#' Row and column names are sorted.
#' @export
#'
#' @examples
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("c", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' identize_byname(m)
#' n <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' identize_byname(n)
#' # This also works with lists
#' identize_byname(list(m, m))
identize_byname <- function(m){
  if (is.list(m)){
    return(mcMap(identize_byname, m))
  }
  stopifnot(nrow(m) == ncol(m))
  n <- sort_rows_cols(m)
  rep_len(1, length.out = nrow(n)) %>%
    matrix(ncol = 1, dimnames = list(paste0("r", 1:nrow(n)), c("c1"))) %>%
    hatize_byname %>%
    setrownames_byname(rownames(n)) %>%
    setcolnames_byname(colnames(n)) %>%
    setrowtype(rowtype(m)) %>% 
    setcoltype(coltype(m))
}

#' Select rows of a matrix (or list of matrices) by name
#'
#' @param m a matrix or a list of matrices
#' @param row_names a vector of Strings containing the row names to keep
#' or delete (if names are preceded by a \code{-}).
#' @param exact If \code{TRUE}, an exact match of row names is required.
#' If \code{FALSE}, \code{startsWith} will determine matches.
#' Retaining rows takes precedence over removing rows.
#' If \code{m} contains none of the requested rows to be retained, \code{NULL} is returned.
#' If \code{m} contains none of the requested rows to be removed, \code{m} is returned.
#'   
#' @return a matrix that is a subset of \code{m} with rows selected by \code{row_names}.
#' @export
#'
#' @examples
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("c", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_rows_byname(m, "i1")
#' select_rows_byname(m, c("i1"))
#' select_rows_byname(m, c("i1", "i4"))
#' select_rows_byname(m, c("-i3"))
#' select_rows_byname(m, c("-i1", "-i3"))
#' select_rows_byname(m, c("-i1", "-i3", "i4")) # Keeping has precedence.
#' select_rows_byname(m, c("x")) # Matches nothing.  NULL is returned.
#' select_rows_byname(m, c("-x")) # Matches nothing.  All of m is returned.
#' select_rows_byname(m, "i", exact = FALSE) # Matches all rows, 
#'                                           # because partial match is OK, and
#'                                           # all row names start with "i".
#' # Also works for lists
#' select_rows_byname(list(m,m), row_names = list(c("i1", "i4"), c("i2", "i3")))
#' select_rows_byname(list(m,m), row_names = c("i1", "i4"))
#' # Test inexact matches
#' n <- setrownames_byname(m, c("a1", "a2", "b1", "b2"))
#' select_rows_byname(n, "a", exact = FALSE)
#' select_rows_byname(n, "-a", exact = FALSE)
select_rows_byname <- function(m, row_names, exact = TRUE){
  if (is.list(m)){
    row_names <- make_list(row_names, n = length(m))
    return(mcMap(select_rows_byname, m = m, row_names = row_names))
  }
  retain <- retain_remove(row_names)[["retain_names"]]
  remove <- retain_remove(row_names)[["remove_names"]]
  retain_indices <- if (exact) {
    which(rownames(m) %in% retain)
  } else {
    which(startsWith(rownames(m), retain))
  }
  remove_indices <- if (exact) {
    which(rownames(m) %in% remove)
  } else {
    which(startsWith(rownames(m), remove))
  }
  if (length(retain_indices) == 0){
    # Nothing to be retained, so try removing columns
    if (length(remove_indices) == 0){
      # Nothing to be retained and nothing to be removed.
      # If the caller wanted to retain something, don't retain anything.
      # Do this first, because retain takes precedence.
      if (length(retain) > 0){
        return(NULL)
      }
      # If the caller wanted to remove something, don't remove anything; return m
      if (length(remove) > 0){
        return(m)
      }
      # Neither retain nor remove had any items.
      # This is almost surely an error.
      stop("remove and retain are empty in select_rows_byname.")
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

#' Select columns of a matrix (or list of matrices) by name
#'
#' @param m a matrix or a list of matrices
#' @param col_names a vector of Strings containing the column names to keep
#' or delete (if names are preceded by a \code{-}).
#' @param exact If \code{TRUE}, an exact match of column names is required.
#' If \code{FALSE}, \code{startsWith} will determine matches.
#' Retaining columns takes precedence over removing columns.
#' If \code{m} contains none of the requested columns to be retained, \code{NULL} is returned.
#' If \code{m} contains none of the requested columns to be removed, \code{m} is returned.
#'
#' @return a matrix that is a subset of \code{m} with columns selected according to \code{col_names}.
#' @export
#'
#' @examples
#' m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("c", 1:4))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' select_cols_byname(m, "c2")
#' select_cols_byname(m, c("c1", "c4"))
#' select_cols_byname(m, c("-c1", "-c4", "-----c3", "c2", "c4", "c4")) # Retain take precedence over remove.
#' select_cols_byname(m, "x") # Matches nothing. NULL is returned
#' select_cols_byname(m, "-x") # Matches nothing. m is returned
#' # Also works for lists
#' select_cols_byname(list(m,m), col_names = list(c("c1", "c4"), c("c2", "c3")))
#' select_cols_byname(list(m,m), col_names = c("c1", "c4"))
#' # Test inexact matches
#' n <- setcolnames_byname(m, c("d1", "d2", "e1", "e2"))
#' select_cols_byname(n, "d", exact = FALSE)
#' select_cols_byname(n, "-d", exact = FALSE)
select_cols_byname <- function(m, col_names, exact = TRUE){
  if (is.list(m)){
    col_names <- make_list(col_names, n = length(m))
    return(mcMap(select_cols_byname, m = m, col_names = col_names))
  }
  retain <- retain_remove(col_names)[["retain_names"]]
  remove <- retain_remove(col_names)[["remove_names"]]
  # retain_indices <- which(colnames(m) %in% retain)
  # remove_indices <- which(colnames(m) %in% remove)
  retain_indices <- if (exact) {
    which(colnames(m) %in% retain)
  } else {
    which(startsWith(colnames(m), retain))
  }
  remove_indices <- if (exact) {
    which(colnames(m) %in% remove)
  } else {
    which(startsWith(colnames(m), remove))
  }
  if (length(retain_indices) == 0){
    # Nothing to be retained, so try removing columns
    if (length(remove_indices) == 0){
      # Nothing to be retained and nothing to be removed.
      # If the caller wanted to retain something, don't retain anything.
      # Do this first, because retain takes precedence.
      if (length(retain) > 0){
        return(NULL)
      }
      # If the caller wanted to remove something, don't remove anything; return m
      if (length(remove) > 0){
        return(m)
      }
      # Neither retain nor remove had any items.
      # This is almost surely an error.
      stop("remove and retain are empty in select_cols_byname.")
    }
    # Remove
    return(m[ , -remove_indices] %>% 
             # When only 1 column is selected, the natural result will be a numeric vector
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

#' Decides which columns to retain and remove.
#' 
#' This is a helper function for \code{select_rows_byname} and \code{select_cols_byname}.
#' Specify removal by a leading \code{-}.
#' Names without a leading \code{-} will be retained.
#' Any number of leading \code{-} will be ignored when deciding the names of columns to be removed.
#'
#' @param row_col_names names of rows or columns to be retained or removed. 
#'
#' @return a list with two items: \code{retain_names} and \code{remove_names}.
#'
#' @examples 
#' retain_remove("c1")
#' retain_remove("-c1")
#' retain_remove(c("c1", "-c1")) # Retain takes precedence.
#' names <- c("-c1", "-c4", "-----c3", "c2", "c4", "c4") # Multiple "-" ignored.
#' retain_remove(names)
retain_remove <- function(row_col_names){
  # Get the indices in col_names of columns to be removed and retained
  remove_indices <- which(startsWith(row_col_names, "-"))
  retain_indices <- setdiff(1:length(row_col_names), remove_indices)
  # Get the names of columns to be removed and retained
  remove_names <- row_col_names[remove_indices] %>% sub("^-*", "", .) %>% unique # removes any number of leading "-"
  retain_names <- row_col_names[retain_indices] %>% unique
  # Get the names of columns that are in both remove and retain
  common_names <- intersect(remove_names, retain_names)
  # Remove common names from those columns that are to be deleted.
  # This step means that keeping takes precedence over removing.
  remove_names <- setdiff(remove_names, common_names)
  return(list(retain_names = retain_names, remove_names = remove_names))  
}

#' Row sums, sorted by name
#'
#' @param m a matrix or data frame from which row sums are desired.
#' @param colname name of the output column containing row sums
#' 
#' Calculates row sums for a matrix by post-multiplying by an identity vector (containins all 1's).
#' In contrast to \code{rowSums} (which returns a \code{numeric} result), 
#' the return value from \code{rowsums_byname} is a matrix.
#' An optional \code{colname} for the resulting column vector can be supplied.
#'
#' @return a column vector of type \code{matrix} containing the row sums of \code{m}
#' @export
#'
#' @examples
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
#' rowsums_byname(NULL)
rowsums_byname <- function(m, colname = NA){
  if (is.list(m)){
    if (is.null(colname)){
      colname <- NA
    }
    return(mcMap(rowsums_byname, m, colname))
  }
  # if (is.na(colname) | is.null(colname)){
  #   # Set the column name to the column type, since we added all items of coltype together.
  #   colname <- coltype(m)
  # }
  if (is.null(colname)){
    # Set the column name to the column type, since we added all items of coltype together.
    colname <- coltype(m)
  }
  if (is.na(colname)){
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

#' Column sums, sorted by name
#'
#' @param m a matrix or data frame from which column sums are desired.
#' @param rowname name of the output row containing column sums
#' 
#' Calculates column sums for a matrix by pre-multiplying by an identity vector (containins all 1's).
#' In contrast to \code{colSums} (which returns a \code{numeric} result), 
#' the return value from \code{colsums_byname} is a matrix.
#' An optional \code{rowname} for the resulting row vector can be supplied.
#'
#' @return a row vector of type \code{matrix} containing the column sums of \code{m}.
#' @export
#'
#' @examples
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
  if (is.list(m)){
    if (is.null(rowname)){
      rowname <- NA
    }
    return(mcMap(colsums_byname, m, rowname))
  }
  # if (is.null(rowname) | is.na(rowname)){
  #   # Set the column name to the column type, since we added all items of coltype together.
  #   rowname <- rowtype(m)
  # }
  if (is.null(rowname)){
    # Set the column name to the column type, since we added all items of coltype together.
    rowname <- rowtype(m)
  }
  if (is.na(rowname)){
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

#' Sum of all elements in a matrix
#' 
#' This is functionally equivalent to \code{m %>% rowsum_byname %>% colsum_byname},
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param m the matrix whose elements are to be summed
#'
#' @return the sum of all elements in \code{m} as a numeric
#' @export
#'
#' @examples
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
  if (is.list(m)){
    return(mcMap(sumall_byname, m))
  }
  m %>%
    rowsums_byname %>%
    colsums_byname %>%
    as.numeric
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
#' @examples
#' m <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' diag(1, nrow = 2) - m # Rows and columns of A are unsorted
#' Iminus_byname(m) # Rows and columns of A are sorted prior to subtracting from the identity matrix
#' # This also works with lists
#' Iminus_byname(list(m,m))
#' # If the m is not square before subtracting from I,
#' # it will be made square by the function complete_and_sort.
#' m2 <- matrix(c(1,2,3,4,5,6), ncol = 2, dimnames = list(c("a", "b", "c"), c("a", "b"))) %>% 
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' Iminus_byname(m2)
Iminus_byname <- function(m){
  if (is.list(m)){
    return(mcMap(Iminus_byname, m))
  }
  # sort_rows_cols(m) %>% 
  complete_and_sort(m) %>% 
    setrowtype(rowtype(m)) %>% 
    setcoltype(coltype(m)) %>% 
    difference_byname(identize_byname(.), .)
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
#' m <- matrix(c(-20, 1, -20, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' m
#' m %>% clean_byname(margin = 1, clean_value = -20) # Eliminates -20, -20 row
#' m %>% clean_byname(margin = 2) # Nothing cleaned, because no columns contain all 0's (the default clean_value).
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
#' DF2 %>% clean_byname(margin = c(1,2), clean_value = -20)
clean_byname <- function(m, margin, clean_value = 0){
  if (1 %in% margin & 2 %in% margin){
    # Clean both dimensions of m.
    cleaned1 <- clean_byname(m, margin = 1, clean_value = clean_value)
    cleaned2 <- clean_byname(cleaned1, margin = 2, clean_value = clean_value)
    return(cleaned2)
  }
  if (is.list(m)){
    return(mcMap(clean_byname, m = m, margin = margin, clean_value = clean_value))
  }
  if (margin == 1){
    # Want to clean rows. Code below assumes want to clean columns.
    # Transpose and then transpose again before returning.
    a <- transpose_byname(m)
  } else if (margin == 2){
    a <- m
  } else {
    stop(paste("margin =", margin, "in clean_byname. Must be 1 or 2."))
  }
  keepcols <- apply(a, 2, function(x) {!all(x == clean_value)})
  keepcolnames <- names(which(keepcols))
  b <- select_cols_byname(m = a, col_names = keepcolnames)
  if (margin == 1){
    return(transpose_byname(b))
  } else if (margin == 2){
    return(b)
  }
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
  if (is.list(m)){
    return(mcMap(getrownames_byname, m))
  }
  return(rownames(m))
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
  if (is.list(m)){
    return(mcMap(getcolnames_byname, m))
  }
  return(colnames(m))
}

#' Sets row names
#' 
#' Sets row names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param m The matrix or data frame from which row names are to be set
#' @param rownames The new row names
#'
#' @return a copy of \code{m} with new row names
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' setrownames_byname(m, c("a", "b"))
#' setrownames_byname(m %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
#' m %>% setrownames_byname(NULL)
#' m %>% setrownames_byname(NA)
#' # This also works for lists
#' setrownames_byname(list(m,m), list(c("a", "b"), c("c", "d")))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' setrownames_byname(DF$m, list(c("r1", "r2")))
#' setrownames_byname(DF$m, list(c("a", "b"), c("c", "d")))
#' DF <- DF %>% mutate(m = setrownames_byname(m, list(c("r1", "r2"))))
#' DF$m[[1]]
setrownames_byname <- function(m, rownames){
  if (is.list(m) & is.list(rownames)){
    return(mcMap(setrownames_byname, m, rownames))
  }
  out <- m
  if (is.null(rownames) || is.na(rownames)){
    # replace with default row names
    rownames(out) <- paste0("[", 1:nrow(out),",]")
  } else {
    rownames(out) <- rownames
  }
  return(out)
}

#' Sets column names
#' 
#' Sets column names in a way that is amenable to use in chaining operations in a functional programming way
#'
#' @param m The matrix or data frame on which column names are to be set
#' @param colnames The new column names. If \code{NULL} or \code{NA}, names are reset to default: "[,j]".
#'
#' @return a copy of \code{m} with new column names
#' @export
#'
#' @examples
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>% 
#'   setrowtype("Industries") %>% setcoltype("Commodity")
#' setcolnames_byname(m, c("a", "b", "c"))
#' setcolnames_byname(m, NULL) # Returns to default columns names "[,j]"
#' setcolnames_byname(m, NA) # Returns to default columns names "[,j]"
#' # This also works for lists
#' setcolnames_byname(list(m,m), list(c("a", "b", "c"), c("d", "e", "f")))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' setcolnames_byname(DF$m, list(c("cnew1", "cnew2", "cnew3")))
#' setcolnames_byname(DF$m, list(c("a", "b", "c"), c("d", "e", "f")))
#' DF <- DF %>% mutate(m = setcolnames_byname(m, list(c("cnew1", "cnew2", "cnew3"))))
#' DF$m[[1]]
#' DF$m[[2]]
setcolnames_byname <- function(m, colnames) {
  if (is.list(m) & is.list(colnames)){
    return(mcMap(setcolnames_byname, m, colnames))
  }
  out <- m
  if (is.null(colnames) || is.na(colnames)){
    # replace with default column names
    colnames(out) <- paste0("[,", 1:ncol(out), "]")
  } else {
    colnames(out) <- colnames
  }
  return(out)
}

#' Sets row type for a matrix or a list of matrices
#' 
#' This function is a wrapper for \code{attr} so setting can be accomplished by the chain operator (\code{%>%})
#' in a functional way.
#' Row types are strings stored in the \code{rowtype} attribute.
#'
#' @param x the matrix on which row type is to be set
#' @param rowtype the type of item stored in rows
#'
#' @return a copy of \code{x} with \code{rowtype} attribute set
#' @export
#'
#' @examples
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
  if (is.list(x)){
    return(mcMap(setrowtype, x, rowtype))
  }
  attr(x, "rowtype") <- rowtype
  return(x)
}

#' Sets column type for a matrix or a list of matrices
#' 
#' This function is a wrapper for \code{attr} so setting can be accomplished by the chain operator (\code{%>%})
#' in a functional way.
#' Column types are strings stored in the \code{coltype} attribute.
#'
#' @param x the matrix on which column type is to be set
#' @param coltype the type of item stored in columns
#'
#' @return a copy of \code{x} with \code{coltype} attribute set
#' @export
#'
#' @examples
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
  if (is.list(x)){
    return(mcMap(setcoltype, x, coltype))
  }
  attr(x, "coltype") <- coltype
  return(x)
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
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>% 
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' rowtype(U)
#' # This also works for lists
#' rowtype(list(U,U))
rowtype <- function(x){
  if (is.list(x)){
    return(mcMap(rowtype, x))
  }
  return(attr(x, "rowtype"))
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
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>% 
#'   setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
#' coltype(U)
#' # This also works for lists
#' coltype(list(U,U))
coltype <- function(x){
  if (is.list(x)){
    return(mcMap(coltype, x))
  }
  return(attr(x, "coltype"))
}

#' Compare two matrices (byname)
#'
#' Matries are completed and sorted relative to one another before comparison
#'
#' @param a the first matrix to be compared
#' @param b the second matrix to be compared
#'
#' @return \code{TRUE} iff row and column types are same \code{and} 
#' row and column names are same \code{and}
#' all entries in the matrix are same.
#' @export
#'
#' @examples
#' a <- matrix(1:4, nrow = 2)
#' b <- matrix(1:4, nrow = 2)
#' equal_byname(a, b)
#' a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' equal_byname(a, b) # FALSE because a has row and column types, but b does not.
#' b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' equal_byname(a, b)
#' dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
#' dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
#' equal_byname(a, b) # FALSE, because row and column names are not equal
#' dimnames(b) <- dimnames(a)
#' equal_byname(a, b)
equal_byname <- function(a, b) {
  if (is.list(a) & is.list(b)){
    return(mcMap(equal_byname, a, b))
  }
  mats <- complete_and_sort(a, b)
  m1 <- mats$m1 %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  m2 <- mats$m2 %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  return(isTRUE(all.equal(m1, m2)))
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
  if (is.list(m)){
    return(mcMap(iszero_byname, m, tol))
  }
  test <- abs(m) < tol
  return(all(test))
}

#' Organize binary arguments
#' 
#' In service of binary \code{_byname} functions, 
#' this function organizes their arguments.
#' Actions performed are:
#' * if only one argument is a list, make the other argument also a list of equal length.
#' * if both arguments are lists, ensure that they are same length.
#' * if one argument is a matrix and the other is a constant, make the constant into a matrix.
#' * ensures that row and column types match
#' * completes and sorts the matrices
#'
#' @param a the first argument to be organized
#' @param b the second argument to be organized
#'
#' @return a list with two elements (\code{a} and \code{b}) containing organized versions of the arguments
organize_args <- function(a, b){
  if (missing(a)){
    stop("Missing argument a in organize_args.")
  }
  if (is.null(a)){
    stop("Null argument a in organize_args.")
  }
  if (missing(b)){
    stop("Missing argument b in organize_args.")
  }
  if (is.null(b)){
    stop("Null argument b in organize_args.")
  }
  if (is.list(a) | is.list(b)){
    # One is a list and the other is not.  Make the other into a list.
    if (! is.list(a)){
      a <- make_list(a, n = length(b))
    }
    if (! is.list(b)){
      b <- make_list(b, n = length(a))
    }
  }
  if (is.list(a) & is.list(b)){
    # Both a and b are lists. Ensure they're the same length.
    stopifnot(length(a) == length(b))
    # Now return the lists.
    return(list(a = a, b = b))
  }
  # Neither a nor b are lists.
  if (! is.matrix(a) & ! is.matrix(b)){
    # Neither a nor b are matrices. Assume we have two constants. Return the constants in a vector.
    return(list(a = a, b = b))
  }
  # Neither a nor b are lists.
  # We don't know if one or both a and b is a matrix. 
  # If one is not a matrix, assume it is a constant and try to make it into an appropriate-sized matrix.
  if (! is.matrix(a) & is.matrix(b)){
    a <- matrix(a, nrow = nrow(b), ncol = ncol(b), dimnames = dimnames(b)) %>% 
      setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  } else if (is.matrix(a) & ! is.matrix(b)){
    b <- matrix(b, nrow = nrow(a), ncol = ncol(a), dimnames = dimnames(a)) %>% 
      setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  }
  # Assume that both a and b are now matrices.
  # Verify that row and column types are same for a and b.
  stopifnot(rowtype(a) == rowtype(b))
  stopifnot(coltype(a) == coltype(b))
  # Ensure that matrices have same row and column names and are in same order.
  matrices <- complete_and_sort(a, b)
  outa <- matrices$m1 %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  outb <- matrices$m2 %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
  return(list(a = outa, b = outb))
}