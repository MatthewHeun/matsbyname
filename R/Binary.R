# This file contains binary functions

#' Name-wise addition of matrices.
#'
#' @param augend Addend matrix or constant
#' @param addend Augend matrix or constant
#'
#' Performs a union and sorting of row and column names prior to summation.
#' Zeroes are inserted for missing matrix elements.
#' Treats missing or \code{NULL} \code{augend} and \code{addend} as \code{0}.
#'
#' @return A matrix representing the name-wise sum of \code{addend} and \code{augend}
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' sum_byname(2, 2)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' U + Y # Non-sensical.  Row and column names not respected.
#' sum_byname(U, Y)
#' sum_byname(U, 100)
#' sum_byname(200, Y)
#' V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, productnames)) %>%
#'   setrowtype("Industries") %>% setcoltype("Products")
#' U + V # row and column names are non-sensical and blindly taken from first argument (U)
#' \dontrun{sum_byname(U, V)} # Fails, because row and column types are different
#' # This also works with lists
#' sum_byname(list(U,U), list(Y,Y))
#' sum_byname(list(U,U), list(100,100))
#' sum_byname(list(U,U), as.list(rep_len(100, 2)))
#' DF <- data.frame(U = I(list()), Y = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"Y"]] <- Y
#' DF[[2,"Y"]] <- Y
#' sum_byname(DF$U, DF$Y)
#' DF %>% mutate(sums = sum_byname(U, Y))
#' sum_byname(U) # If only one argument, return it.
#' sum_byname(2, NULL) # Gives 2
#' sum_byname(2, NA)   # Gives NA
#' sum_byname(NULL, 1) # Gives 1
#' sum_byname(list(NULL, 1), list(1, 1))
#' DF2 <- data.frame(U = I(list()), Y = I(list()))
#' DF2[[1,"U"]] <- NULL
#' DF2[[2,"U"]] <- U
#' DF2[[1,"Y"]] <- Y
#' DF2[[2,"Y"]] <- Y
#' sum_byname(DF2$U, DF2$Y)
#' DF3 <- DF2 %>% mutate(sums = sum_byname(U, Y))
#' DF3
#' DF3$sums[[1]]
#' DF3$sums[[2]]
sum_byname <- function(augend, addend){
  binaryapply_byname(`+`, augend, addend)
}

#' Name-wise subtraction of matrices.
#'
#' @param minuend Minuend matrix or constant
#' @param subtrahend Subtrahend matrix or constant
#'
#' Performs a union and sorting of row and column names prior to differencing.
#' Zeroes are inserted for missing matrix elements.
#'
#' @return A matrix representing the name-wise difference between \code{minuend} and \code{subtrahend}
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
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
  binaryapply_byname(`-`, minuend, subtrahend)
}

#' Name-wise matrix multiplication
#'
#' @param multiplicand Multiplicand matrix
#' @param multiplier Multiplier matrix
#'
#' Performs a union and sorting of multiplicand rows and multiplier columns by name 
#' prior to multiplication.
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
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("c1", "c2", "c3"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' G <- matrix(1:4, ncol = 2, dimnames = list(c("c2", "c1"), c("g2", "g1"))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' # Succeeds because G is completed to include a row named c3 (that contains zeroes).
#' matrixproduct_byname(V, G) 
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
  binaryapply_byname(`%*%`, multiplicand, multiplier, match_type = "matmult") %>% 
    # match_type = "matmult" ensures that cols of multiplicand and rows of multiplier
    # are completed and sorted, but rows and cols of the output are not guaranteed 
    # to be sorted.
    # Becase _byname assures that all rows and columns are sorted, 
    # we sort them here before returning. 
    sort_rows_cols()
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
#' library(magrittr)
#' library(dplyr)
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
  binaryapply_byname(`*`, multiplicand, multiplier)
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
#' library(magrittr)
#' library(dplyr)
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
  binaryapply_byname(`/`, dividend, divisor)
}

#' Name- and element-wise arithmetic mean of matrices.
#'
#' Gives the arithmatic mean of corresponding entries of \strong{\code{X1}} and \strong{\code{X2}}.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing arithmetic mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' @param X1 first operand (a matrix or constant value or lists of same)
#' @param X2 second operand (a matrix or constant value or lists of same)
#'
#' @return A matrix representing the name-wise arithmetic mean 
#'         of \strong{\code{X1}} and \strong{\code{X2}}.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' mean_byname(100, 50)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' (U + G) / 2 # Non-sensical. Row and column names not respected.
#' mean_byname(U, G) # Row and column names respected! Should be 1, 2, 3, and 4. 
#' mean_byname(100, U)
#' mean_byname(10, G)
#' # This also works with lists
#' mean_byname(list(100, 100), list(50, 50))
#' mean_byname(list(U,U), list(G,G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' mean_byname(DF$U, DF$G)
#' DF %>% mutate(means = mean_byname(U, G))
mean_byname <- function(X1, X2){
  mean.func <- function(X1, X2){
    sum_byname(X1, X2) %>%
      elementquotient_byname(2)
  }
  binaryapply_byname(mean.func, a = X1, b = X2)
}

#' Name- and element-wise geometric mean of two matrices.
#'
#' Gives the geometric mean of corresponding entries of \strong{\code{X1}} and \strong{\code{X2}}.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing geometric mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' @param X1 first operand (a matrix or constant value or lists of same)
#' @param X2 second operand (a matrix or constant value or lists of same)
#'
#' @return A matrix representing the name-wise geometric mean 
#'         of \strong{\code{X1}} and \strong{\code{X2}}.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' geometricmean_byname(10, 1000)
#' commoditynames <- c("c1", "c2")
#' industrynames <- "i1"
#' U <- matrix(c(10, 1000), ncol = 1, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(c(1e3, 1e5), ncol = 1, nrow = 2, 
#'             dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' # Non-sensical. Row and column names not respected.
#' sqrt(U*G)
#' # Row and column names respected!
#' geometricmean_byname(U, G)
#' geometricmean_byname(1000, U)
#' geometricmean_byname(10, G)
#' # This also works with lists
#' geometricmean_byname(list(10, 1000), list(1000, 10))
#' geometricmean_byname(list(U,U), list(G,G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' geometricmean_byname(DF$U, DF$G)
#' DF %>% mutate(geomeans = geometricmean_byname(U, G))
geometricmean_byname <- function(X1, X2){
  geomean.func <- function(X1, X2){
    if (any((X1 < 0 & X2 > 0) | (X1 > 0 & X2 < 0))) {
      stop(paste0("X1 and X2 must have same sign in geometricmean_byname: X1 = ", X1, ", X2 = ", X2, "."))
    } 
    elementproduct_byname(X1, X2) %>% 
      sqrt()
  }
  binaryapply_byname(geomean.func, a = X1, b = X2)
}

#' Name- and element-wise logarithmic mean of matrices.
#'
#' The logarithmic mean of corresponding entries of \strong{\code{X1}} and \strong{\code{X2}} is 
#' \code{0} if \code{x1 = 0} or \code{x2 = 0}, 
#' \code{x1} if \code{x1 = x2}, or
#' \code{(x2 - x1) / (log(x2) - log(x1))} otherwise.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing logarithmic mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' Internally, the third condition is implemented as 
#' \code{(y - x) / log(y/x)}.
#' 
#' Note that \code{(x2 - x1) / log(x2/x1) = (x1 - x2) / log(x1/x2)},
#' so logarithmic mean is commutative;
#' the order of arguments \strong{\code{X1}} and \strong{\code{X2}}
#' does not change the result.
#' 
#' @param X1 first operand (a matrix or constant value or lists of same).
#' @param X2 second operand (a matrix or constant value or lists of same).
#' @param base the base of the logarithm used when computing the logarithmic mean.
#'        (Default is \code{base = exp(1)}.)
#'
#' @return A matrix representing the name-wise logarithmic mean 
#'         of \strong{\code{X1}} and \strong{\code{X2}}.
#' @export
#'
#' @examples
#' library(magrittr)
#' library(dplyr)
#' m1 <- matrix(c(1:6), nrow = 3, ncol = 2) %>% 
#'   setrownames_byname(c("r1", "r2", "r3")) %>% setcolnames_byname(c("c1", "c2")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' m2 <- matrix(c(7:12), nrow = 3, ncol = 2) %>% 
#'   setrownames_byname(c("r2", "r3", "r4")) %>% setcolnames_byname(c("c2", "c3")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' logarithmicmean_byname(m1, m2)
#' # This also works with lists
#' logarithmicmean_byname(list(m1, m1), list(m2, m2))
#' DF <- data.frame(m1 = I(list()), m2 = I(list()))
#' DF[[1,"m1"]] <- m1
#' DF[[2,"m1"]] <- m1
#' DF[[1,"m2"]] <- m2
#' DF[[2,"m2"]] <- m2
#' logarithmicmean_byname(DF$m1, DF$m2)
#' DF %>% mutate(logmeans = logarithmicmean_byname(m1, m2))
logarithmicmean_byname <- function(X1, X2, base = exp(1)){
  logmean.func <- function(X1, X2, base) {
    # At this point, our list is gone.  
    # X1 and X2 are single matrices or single numbers. 
    # Furthermore, X1 and X2 should have 
    #   * exact same dimensions, 
    #   * same row and column names, and 
    #   * same rowtype and column type.
    # We exploit these facts in the code below.
    # Unwrap each matrix and Map logmean to all elements.
    out <- Map(f = logmean, as.numeric(X1), as.numeric(X2), base = base) %>% 
      # Map produces a list, but we need a numeric vector.
      as.numeric()
    if (is.matrix(X1)) {
      # If X1 and X2 are originally matrices, make out into a matrix
      # by rewrapping it. 
      out <- out %>% 
        matrix(nrow = nrow(X1), ncol = ncol(X1)) %>% 
        # Add the row and column names to it.
        setrownames_byname(rownames(X1)) %>% setcolnames_byname(colnames(X1))
    }
    out %>%  
      setrowtype(rowtype(X1)) %>% setcoltype(coltype(X1))
  }
  binaryapply_byname(logmean.func, a = X1, b = X2, base = base)
}

#' Compare two matrices (byname)
#'
#' Matries are completed and sorted relative to one another before comparison
#'
#' @param a the first matrix to be compared
#' @param b the second matrix to be compared
#'
#' @return \code{TRUE} iff row and column types are same \emph{and}
#' row and column names are same \emph{and}
#' all entries in the matrix are same.
#' @export
#'
#' @examples
#' library(magrittr)
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
  equal.func <- function(a, b){
    mats <- complete_and_sort(a, b)
    return(isTRUE(all.equal(mats$m1, mats$m2)))
  }
  binaryapply_byname(equal.func, a = a, b = b, match_type = "all", rowcoltypes = FALSE)
}

#' Test whether two matrices or lists of matrices have same structure
#' 
#' Two matrices are said to have the same structure 
#' if row and column types are identical 
#' and 
#' if row and column names are identical.
#'
#' @param X1 the first matrix to be tested
#' @param X2 the second matrix to be tested
#'
#' @return \code{TRUE} if \code{X1} and \code{X2} have the same structure, \code{FALSE} otherwise.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' same_structure_byname(2, 2)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' same_structure_byname(U, U)
#' same_structure_byname(U, U %>% setrowtype("row"))
#' same_structure_byname(U %>% setcoltype("col"), U)
#' # Also works with lists
#' same_structure_byname(list(U, U))
same_structure_byname <- function(X1, X2){
  samestruct.func <- function(X1, X2){
    if (!isTRUE(all.equal(rownames(X1), rownames(X2)))) {
      return(FALSE)
    }
    if (!isTRUE(all.equal(colnames(X1), colnames(X2)))) {
      return(FALSE)
    }
    if (!is.null(rowtype(X1)) & !is.null(rowtype(X2))) {
      # When both rowtypes are non-null, we can compare them directly.
      if (!rowtype(X1) == rowtype(X2)) {
        return(FALSE)
      }
    }
    if (!is.null(coltype(X1)) & !is.null(coltype(X2))) {
      # When both coltypes are non-null, we can compare them directly.
      if (!coltype(X1) == coltype(X2)) {
        return(FALSE)
      }
    }
    # At least one of rowtype or coltype on X1 or X2 is null.
    if (xor(is.null(rowtype(X1)), is.null(rowtype(X2)))) {
      # Rowtype on one of X1 or X2 is null, but the other is non-null.
      return(FALSE)
    }
    if (xor(is.null(coltype(X1)), is.null(coltype(X2)))) {
      # Coltype on one of X1 or X2 is null, but the other is non-null.
      return(FALSE)
    }
    return(TRUE)
  }
  binaryapply_byname(samestruct.func, X1, X2, match_type = "none", rowcoltypes = FALSE)
}
