# This file contains binary functions

#' Name-wise addition of matrices
#'
#' Performs a union and sorting of addend and augend row and column names prior to summation.
#' Zeroes are inserted for missing matrix elements.
#' Treats missing or `NULL` operands as `0`.
#'
#' For this function, a list of lists of operands is ambiguous.
#' Should the operands be summed across lists 
#' (first items summed across all lists, second items summed across all list, etc.)
#' or should each list be summed along each list?  
#' In the first case, the return object will have length equal to the length of the lists in the `...` argument.
#' In the second case, the return object will have length equal to the number of lists in the `...` argument.
#' The first case is like summing across rows of a data frame.
#' The second case is like summing down columns of a data frame.
#' The `summarise` argument distinguishes between these two cases.
#' The default value for `summarise` is `FALSE`, giving the first behavior.
#' Set `summarise` to `TRUE` to cause this function to act like `dplyr::summarise()`
#' for its list of arguments.
#' If `.summarise = TRUE`, the 
#' data value is guaranteed to be a list.
#' If the call to `sum_byname(.summarise = TRUE)` is made in the context of a data frame,
#' the column returned is guaranteed to be a list column.
#' See the aggregation vignette for additional details and examples.
#' 
#' @param ... Operands: constants, matrices, or lists of matrices.
#' @param .summarise When `TRUE`, a operands are summed down lists.
#'                   When `FALSE` (the default), items are summed across lists.
#'
#' @return A matrix representing the name-wise sum of arguments.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' sum_byname(2, 2)
#' sum_byname(2, 2, 2)
#' sum_byname(2, 2, -2, -2)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' sum_byname(U, 100)
#' sum_byname(200, Y)
#' U + Y # Non-sensical.  Row and column names not respected.
#' sum_byname(U, U)
#' sum_byname(U, Y)
#' sum_byname(U, U, Y, Y)
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
sum_byname <- function(..., .summarise = FALSE){
  if (length(list(...)) == 1 & !.summarise) {
    return(list(...)[[1]])
  }
  naryapply_byname(`+`, ..., .summarise = .summarise)
}


#' Name-wise subtraction of matrices
#'
#' @param minuend matrix or constant
#' @param subtrahend matrix or constant
#'
#' Performs a union and sorting of row and column names prior to differencing.
#' Zeroes are inserted for missing matrix elements.
#'
#' @return A matrix representing the name-wise difference between `minuend` and `subtrahend`
#' 
#' @export
#'
#' @examples
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


#' Powers of matrix elements
#' 
#' Gives the result of raising all elements of a matrix or list of matrices to a power. 
#'
#' @param a a matrix of list of matrices 
#' @param pow the power to which elements of \code{a} will be raised
#'
#' @return \code{a} with each element raised to \code{pow}
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' pow_byname(2, 3)
#' m <- matrix(2, nrow = 2, ncol = 3, dimnames = list(paste0("r", 1:2), paste0("c", 1:3))) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' pow_byname(m, 2)
#' DF <- data.frame(m = I(list()), pow = I(list()))
#' DF[[1, "m"]] <- m
#' DF[[2, "m"]] <- m
#' DF[[1, "pow"]] <- 0.5
#' DF[[2, "pow"]] <- -1
#' DF %>% mutate(
#'   sqrtm = pow_byname(m, 0.5),
#'   mtopow = pow_byname(m, pow)
#' )
pow_byname <- function(a, pow){
  binaryapply_byname(`^`, a, pow)
}


#' Name-wise matrix multiplication
#'
#' Multiplies operands from left to right
#' (when `.summarise = FALSE`).
#' If `.summarise = TRUE`, 
#' operands are multiplied from first to last.
#' 
#' Performs a union and sorting of multiplicand rows and multiplier columns by name 
#' prior to multiplication.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that
#' the dimensions of multiplicand and multiplier matrices will be conformable.
#' I.e., the number of columns in multiplicand
#' will equal the number of rows in multiplier,
#' so long as the column names of multiplicand are unique and
#' the row names of multiplier are unique.
#' If column type of the multiplicand is not same as
#' row type of the multiplier on any step of the multiplication,
#' the function will fail.
#' The result is matrix product
#' with row names from the first multiplicand and column names from the last multiplier.
#'
#' @param ...  Operands; constants, matrices, or lists of matrices.
#' @param .summarise When `TRUE`, a matrix multiplication proceeds down lists of arguments.
#'                   When `FALSE` (the default), items are multiplied across lists.
#'
#' @return A matrix representing the name-wise product of operands.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("c1", "c2", "c3"))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' G <- matrix(1:4, ncol = 2, dimnames = list(c("c2", "c1"), c("i2", "i1"))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' Z <- matrix(11:14, ncol = 2, dimnames = list(c("i1", "i2"), c("s1", "s2"))) %>% 
#'   setrowtype("Industries") %>% setcoltype("Sectors")
#' # Succeeds because G is completed to include a row named c3 (that contains zeroes).
#' matrixproduct_byname(V, G)
#' \dontrun{V %*% G} # Fails because E lacks a row named c3.
#' matrixproduct_byname(V, G, Z)
#' # This also works with lists
#' matrixproduct_byname(list(V,V), list(G,G))
#' DF <- data.frame(V = I(list()), G = I(list()))
#' DF[[1,"V"]] <- V
#' DF[[2,"V"]] <- V
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' matrixproduct_byname(DF$V, DF$G)
#' DF %>% mutate(matprods = matrixproduct_byname(V, G))
#' # Also works with lists, multiplying down the lists if `.summarise = TRUE`.
#' matrixproduct_byname(list(V, G, Z), .summarise = TRUE)
matrixproduct_byname <- function(..., .summarise = FALSE){
  # match_type = "matmult" ensures that cols of multiplicand and rows of multiplier
  # are completed and sorted, but rows and cols of the output of the 
  # %*% operation are not guaranteed to be sorted.
  # Thus, we sort_rows_cols() prior to returning.
  naryapply_byname(`%*%`, ..., match_type = "matmult", .summarise = .summarise) %>% 
    # Because _byname assures that all rows and columns are sorted, 
    # we sort them here before returning. 
    # We have to specify the margin explicity,
    # because we cannot rely upon the default value for margin.
    # If our matrices have length 2, 
    # sort_rows_cols will assume that we want to sort
    # margin = 1 for the first matrix and
    # margin = 2 for the second matrix.
    # By specifying list(c(1, 2)), we explicitly say that we want 
    # both margins sorted for all matrices.
    sort_rows_cols(margin = list(c(1, 2)))
}


#' Name-wise matrix Hadamard multiplication
#'
#' Performs a union and sorting of names of rows and columns for both multiplicand and multiplier
#' for each sequential multiplication step.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that
#' the dimensions of the multiplicand and multiplier are be conformable for each sequential multiplication.
#' 
#' The Hadamard product is also known as the \code{entrywise} product.
#'
#' @param ... Operands; constants, matrices, or lists of matrices.
#' @param .summarise When `TRUE`, operands are multiplied down lists.
#'                   When `FALSE` (the default), items multiplied across lists. 
#'
#' @return Name-wise element product of operands.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' hadamardproduct_byname(2, 2)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U * G # Not what is desired, because names aren't aligned
#' hadamardproduct_byname(U, G)
#' hadamardproduct_byname(U, G, G)
#' hadamardproduct_byname(U, 0)
#' hadamardproduct_byname(0, G)
#' # This also works with lists
#' hadamardproduct_byname(list(U, U), list(G, G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' hadamardproduct_byname(DF$U, DF$G)
#' DF %>% mutate(entrywiseprods = hadamardproduct_byname(U, G))
#' # Also works down lists with `.summarise = TRUE`.
#' hadamardproduct_byname(list(U, G), .summarise = TRUE)
hadamardproduct_byname <- function(..., .summarise = FALSE){
  # Note that prod(1) returns 1, not 0.
  # So hadamardproduct_byname returns the non-missing argument if only 1 argument is provided.
  if (length(list(...)) == 1 & !.summarise) {
    return(list(...)[[1]])
  }
  naryapply_byname(`*`, ..., .summarise = .summarise)
}


#' Name-wise matrix element division
#'
#' Element-wise division of two matrices.
#' 
#' Performs a union and sorting of names of rows and columns for both \code{dividend} and \code{divisor}
#' prior to element division.
#' Zeroes are inserted for missing matrix elements.
#' Doing so ensures that
#' the dimensions of the \code{dividend} and \code{divisor} will be conformable.
#'
#' @param dividend Dividend matrix or constant
#' @param divisor Divisor matrix or constant
#'
#' @return A matrix representing the name-wise element quotient of \code{dividend} and \code{divisor}
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' quotient_byname(100, 50)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' U / G # Non-sensical.  Names aren't aligned
#' quotient_byname(U, G)
#' quotient_byname(U, 10)
#' quotient_byname(10, G)
#' # This also works with lists
#' quotient_byname(10, list(G,G))
#' quotient_byname(list(G,G), 10)
#' quotient_byname(list(U, U), list(G, G))
#' DF <- data.frame(U = I(list()), G = I(list()))
#' DF[[1,"U"]] <- U
#' DF[[2,"U"]] <- U
#' DF[[1,"G"]] <- G
#' DF[[2,"G"]] <- G
#' quotient_byname(DF$U, DF$G)
#' DF %>% mutate(elementquotients = quotient_byname(U, G))
quotient_byname <- function(dividend, divisor){
  binaryapply_byname(`/`, dividend, divisor)
}


#' Name- and element-wise arithmetic mean of matrices
#'
#' Gives the arithmetic mean of operands in \code{...}.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing arithmetic mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' @param ... Operands: constants, matrices, or lists of matrices.
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'
#' @return name-wise arithmetic mean of operands.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' mean_byname(100, 50)
#' mean_byname(10, 20, 30)
#' commoditynames <- c("c1", "c2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
#'   setrowtype("Commodities") %>% setcoltype("Industries")
#' (U + G) / 2 # Non-sensical. Row and column names not respected.
#' mean_byname(U, G) # Row and column names respected! Should be 1, 2, 3, and 4. 
#' mean_byname(U, G, G)
#' mean_byname(100, U)
#' mean_byname(100, 50, U)
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
mean_byname <- function(..., .summarise = FALSE){
  if (.summarise) {
    # Ensure that all lists are same length.
    ns <- lapply(X = list(...), FUN = function(l) {
      length(l)
    }) %>% 
      unlist(recursive = FALSE)
  } else {
    n <- length(list(...))
  }
  sums <- sum_byname(..., .summarise = .summarise)
  if (.summarise) {
    return(quotient_byname(sums, ns))
  } else {
    return(quotient_byname(sums, n))
  }
}


#' Name- and element-wise geometric mean of two matrices.
#'
#' Gives the geometric mean of corresponding entries of \code{a} and \code{b}.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing geometric mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' @param ... operands; constants, matrices, or lists of matrices
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'
#' @return name-wise geometric mean of operands
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' geometricmean_byname(10, 1000)
#' geometricmean_byname(10, 1000, 100000)
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
geometricmean_byname <- function(..., .summarise = FALSE){
  if (.summarise) {
    # Ensure that all lists are same length.
    ns <- lapply(X = list(...), FUN = function(l) {
      length(l)
    }) %>% 
      unlist(recursive = FALSE)
  } else {
    n <- length(list(...))
  }
  prods <- hadamardproduct_byname(..., .summarise = .summarise)
  if (.summarise) {
    return(pow_byname(prods, 1/ns))
  } else {
    return(pow_byname(prods, 1/n))
  }
}


#' Name- and element-wise logarithmic mean of matrices
#'
#' The logarithmic mean of corresponding entries of \code{a} and \code{b} is 
#' \code{0} if \code{a = 0} or \code{b = 0}, 
#' \code{a} if \code{a = b}, or
#' \code{(b - a) / (log(b) - log(a))} otherwise.
#' 
#' This function performs a union and sorting of row and column names 
#' prior to performing logarithmic mean.
#' Zeroes are inserted for missing matrix elements.
#' 
#' Internally, the third condition is implemented as 
#' \code{(b - a) / log(b/a)}.
#' 
#' Note that \code{(b - a) / log(b/a) = (a - b) / log(a/b)},
#' so logarithmic mean is commutative;
#' the order of arguments \strong{\code{a}} and \strong{\code{b}}
#' does not change the result.
#' 
#' @param a first operand (a matrix or constant value or lists of same).
#' @param b second operand (a matrix or constant value or lists of same).
#' @param base the base of the logarithm used when computing the logarithmic mean.
#'        (Default is \code{base = exp(1)}.)
#'
#' @return A matrix representing the name-wise logarithmic mean 
#'         of \code{a} and \code{b}.
#' @export
#'
#' @examples
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
logarithmicmean_byname <- function(a, b, base = exp(1)){
  logmean_func <- function(a, b, base) {
    # At this point, our list is gone.  
    # a and b are single matrices or single numbers. 
    # Furthermore, a and b should have 
    #   * exact same dimensions, 
    #   * same row and column names, and 
    #   * same rowtype and column type.
    # We exploit these facts in the code below.
    # Unwrap each matrix and Map logmean to all elements.
    out <- Map(f = logmean, as.numeric(a), as.numeric(b), base = base) %>% 
      # Map produces a list, but we need a numeric vector.
      as.numeric()
    if (is.Matrix(a)) {
      out <- out %>% 
        matsbyname::Matrix(nrow = nrow(a), ncol = ncol(a), 
                           dimnames = list(rownames(a), colnames(a)))
    } else if (is.matrix(a)) {
      # If a and b are originally matrices, make out into a matrix
      # by rewrapping it. 
      out <- out %>% 
        matrix(nrow = nrow(a), ncol = ncol(a), 
               dimnames = list(rownames(a), colnames(a)))
    }
    out %>% 
      # Add row and column types
      setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
  }
  binaryapply_byname(logmean_func, a = a, b = b, .FUNdots = list(base = base))
}


#' Compare two matrices "by name" for equality
#'
#' If operands are matrices, they are completed and sorted relative to one another prior to comparison.
#' 
#' Comparisons are made by `equal_matrix_or_Matrix(a, b, tolerance = abs(tol))`
#' so that variations among numbers
#' within `tol` will still return `TRUE.`
#' 
#' If EXACT comparison is needed, use `identical_byname()`, 
#' which compares using `identical(a, b)`.
#'
#' `tol` should be a single value that applies to all items in `...`.
#'
#' @param ... Operands to be compared.
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'                   Default is `FALSE` (across lists).
#' @param tol A double that tells how precisely equal the values
#'            of `a` and `b` must be.
#'            Default is `0`.
#'
#' @return `TRUE` iff all information is equal, including
#' row and column types _and_
#' row and column names _and_
#' entries in the matrices.
#' @export
#'
#' @examples
#' a <- matrix(1:4, nrow = 2)
#' b <- matrix(1:4, nrow = 2)
#' equal_byname(a, b)
#' equal_byname(a, b + 1e-100)
#' identical_byname(a, b + 1e-100)
#' a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' equal_byname(a, b) # FALSE because a has row and column types, but b does not.
#' b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' equal_byname(a, b)
#' dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
#' dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
#' equal_byname(a, b) # FALSE, because row and column names are not equal
#' dimnames(b) <- dimnames(a)
#' equal_byname(a, b)
equal_byname <- function(..., .summarise = FALSE, tol = 0){
  equal_func <- function(a, b){
    # return(isTRUE(base::all.equal(a, b)))
    return(equal_matrix_or_Matrix(a, b, tolerance = abs(tol)))
  }
  naryapplylogical_byname(equal_func, ..., set_rowcoltypes = FALSE, .summarise = .summarise)
}


#' Compare two matrices "by name" for exact equality
#'
#' If operands are matrices, they are completed and sorted relative to one another prior to comparison.
#' 
#' Comparisons are made by \code{identical(a, b)} so that variations among numbers
#' within the computational precision will return \code{FALSE}.
#' 
#' If fuzzy comparison is needed, use \code{\link{equal_byname}}, 
#' which compares using \code{isTRUE(all.equal(a, b))}.
#'
#' @param ... Operands to be compared.
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'
#' @return `TRUE` iff all information is identical, including
#' row and column types \emph{and}
#' row and column names \emph{and}
#' entries in the matrices.
#' @export
#'
#' @examples
#' a <- matrix(1:4, nrow = 2)
#' b <- matrix(1:4, nrow = 2)
#' identical_byname(a, b)
#' identical_byname(a, b + 1e-100)
#' a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' identical_byname(a, b) # FALSE because a has row and column types, but b does not.
#' b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
#' identical_byname(a, b)
#' dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
#' dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
#' identical_byname(a, b) # FALSE, because row and column names are not equal
#' dimnames(b) <- dimnames(a)
#' identical_byname(a, b)
identical_byname <- function(..., .summarise = FALSE){
  ident_func <- function(a, b){
    return(identical(a, b))
  }
  naryapplylogical_byname(ident_func, ..., set_rowcoltypes = FALSE, .summarise = .summarise)
}


#' Test whether matrices or lists of matrices have same structure
#' 
#' Matrices are said to have the same structure 
#' if row and column types are identical 
#' and 
#' if row and column names are identical.
#' Values can be different.
#'
#' @param ... Operands to be compared.
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'
#' @return \code{TRUE} if all operands have the same structure, \code{FALSE} otherwise.
#' 
#' @export
#'
#' @examples
#' samestructure_byname(2, 2)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' samestructure_byname(U, U)
#' samestructure_byname(U, U %>% setrowtype("row"))
#' samestructure_byname(U %>% setcoltype("col"), U)
#' # Also works with lists
#' samestructure_byname(list(U, U), list(U, U))
samestructure_byname <- function(..., .summarise = FALSE){
  samestruct_func <- function(a, b){
    if (!isTRUE(all.equal(rownames(a), rownames(b)))) {
      return(FALSE)
    }
    if (!isTRUE(all.equal(colnames(a), colnames(b)))) {
      return(FALSE)
    }
    if (!is.null(rowtype(a)) & !is.null(rowtype(b))) {
      # When both rowtypes are non-null, we can compare them directly.
      if (!rowtype(a) == rowtype(b)) {
        return(FALSE)
      }
    }
    if (!is.null(coltype(a)) & !is.null(coltype(b))) {
      # When both coltypes are non-null, we can compare them directly.
      if (!coltype(a) == coltype(b)) {
        return(FALSE)
      }
    }
    # At least one of rowtype or coltype on a or b is null.
    if (xor(is.null(rowtype(a)), is.null(rowtype(b)))) {
      # Rowtype on one of a or b is null, but the other is non-null.
      return(FALSE)
    }
    if (xor(is.null(coltype(a)), is.null(coltype(b)))) {
      # Coltype on one of a or b is null, but the other is non-null.
      return(FALSE)
    }
    return(TRUE)
  }
  naryapplylogical_byname(samestruct_func, ..., 
                          match_type = "none", set_rowcoltypes = FALSE, 
                          .organize = FALSE, .summarise = .summarise)
}


#' And "by name"
#' 
#' Operands should be logical, although numerical operands are accepted.
#' Numerical operands are interpreted as `FALSE` when `0` and
#' `TRUE` for any other number.
#'
#' @param ... Operands to the logical `and` function.
#' @param .summarise Tells whether the operation should be accomplished
#'                   across lists (`FALSE`) or down lists (`TRUE`).
#'
#' @return Logical `and` applied to the operands.
#' 
#' @export
#'
#' @examples
#' and_byname(TRUE)
#' and_byname(FALSE)
#' and_byname(list(TRUE, FALSE), list(TRUE, TRUE), list(TRUE, TRUE), list(TRUE, TRUE))
#' m1 <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2, ncol = 2, 
#'   dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' m2 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2,
#'   dimnames = list(c("r1", "r2"), c("c1", "c2")))
#' and_byname(m1, m1)
#' and_byname(m1, m2)
#' and_byname(list(m1, m1), list(m1, m1), list(m2, m2))
#' and_byname(list(m1, m1), list(m1, m1), list(m2, m2), .summarise = TRUE)
and_byname <- function(..., .summarise = FALSE){
  if (length(list(...)) == 1 & !.summarise) {
    return(list(...)[[1]])
  }
  naryapplylogical_byname(`&`, ..., .summarise = .summarise)
}