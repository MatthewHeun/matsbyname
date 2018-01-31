library(parallel)
library(magrittr)
library(dplyr)

#' Apply a unary function byname
#'
#' @param FUN a unary function to be applied "byname" to \code{a}.
#' @param a the argument to \code{FUN}.
#' @param ... other arguments to be passed to \code{FUN}.
#' @param rowcoltypes a string that tells how to transfer row and column types of \code{a} to output. 
#'        Options are:
#'        \itemize{
#'          \item{\code{all}: transfer both row and column types of \code{a} directly to output.}
#'          \item{\code{transpose}: rowtype of \code{a} becomes coltype of output;
#'                                  coltype of \code{a} becomes rowtype of output.}
#'          \item{\code{row}: rowtype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{col}: coltype of \code{a} becomes both rowtype and coltype of output.}
#'          \item{\code{none}: rowtype and coltype not set by this function. 
#'                             Rather, \code{FUN} will rowtype and coltype.}
#'        }
#'
#' @return the result of applying \code{FUN} "byname" to \code{a}.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' difference_byname(0, U)
#' unaryapply_byname(`-`, U)
unaryapply_byname <- function(FUN, a, ..., rowcoltypes = c("all", "transpose", "row", "col", "none")){
  rowcoltypes <- match.arg(rowcoltypes)
  if (is.list(a)) {
    return(Map(unaryapply_byname, make_list(FUN, n = length(a)), a, ..., rowcoltypes = rowcoltypes))
  }
  # if (length(list(...)) == 0) {
  #   out <- FUN(sort_rows_cols(a))
  # } else {
  #   out <- FUN(sort_rows_cols(a), ...)
  # }
  if (length(list(...)) == 0) {
    out <- FUN(a)
  } else {
    out <- FUN(a, ...)
  }
  if (rowcoltypes == "all") {
    out <- out %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(coltype(a))
  } else if (rowcoltypes == "transpose") {
    out <- out %>%
      setrowtype(coltype(a)) %>%
      setcoltype(rowtype(a))
  } else if (rowcoltypes == "row") {
    out <- out %>%
      setrowtype(rowtype(a)) %>%
      setcoltype(rowtype(a))
  } else if (rowcoltypes == "col") {
    out <- out %>%
      setrowtype(coltype(a)) %>%
      setcoltype(coltype(a))
  } else if (rowcoltypes == "none") {
    # Do nothing. rowtype and coltype should have been set by FUN.
  } else {
    stop(paste("Unknown rowcoltypes argument in unaryapply_byname:", rowcoltypes))
  }
  return(out)
}

#' Apply a binary function byname
#' 
#' If either \code{a} or \code{b} is missing or \code{NULL}, 
#' \code{0} is passed to \code{FUN} in its place.
#'
#' @param FUN a binary function to be applied "byname" to \code{a} and \code{b}.
#' @param a the first argument to \code{FUN}.
#' @param b the second argument to \code{FUN}.
#' @param ... additional named arguments passed to \code{FUN}.
#' @param match_type one of \code{"all"} or \code{"matmult"}.
#'        When both \code{a} and \code{b} are matrices,
#'        "\code{all}" (the default) indicates that
#'        rowtypes of \code{a} must match rowtypes of \code{b} and
#'        coltypes of \code{a} must match coltypes of \code{b}.
#'        If "\code{matmult}",
#'        coltypes of \code{a} must match rowtypes of \code{b}.
#' @param rowcoltypes tells whether to apply row and column types from \code{a} and \code{b}
#'        to the output. 
#'        The default (\code{TRUE}) means that row and column types are applied to the output.
#'        If \code{FALSE}, row and column types are \emph{not} applied to the output.
#' @param .organize a boolean that tells whether or not to automatically 
#'        complete \code{a} and \code{b} relative to each other and
#'        sort the rows and columns of the completed matrices.
#'        Normally, this should be \code{TRUE} (the default).
#'
#' @return the result of applying \code{FUN} "byname" to \code{a} and \code{b}.
#' 
#' @export
#'
#' @examples
#' library(magrittr)
#' productnames <- c("p1", "p2")
#' industrynames <- c("i1", "i2")
#' U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
#'   setrowtype("Products") %>% setcoltype("Industries")
#' sum_byname(U, Y)
#' binaryapply_byname(`+`, U, Y)
binaryapply_byname <- function(FUN, a, b, ..., 
                               match_type = c("all", "matmult"), rowcoltypes = TRUE, .organize = TRUE){
  match_type <- match.arg(match_type)
  if (.organize) {
    args <- organize_args(a, b, fill = 0, match_type = match_type)
    a <- args$a
    b <- args$b
  }
  if (is.list(a) & is.list(b)) {
    return(Map(binaryapply_byname, make_list(FUN, n = max(length(a), length(b))), 
               a, b, ..., match_type = match_type, rowcoltypes = rowcoltypes, .organize = .organize))
  }
  if (length(list(...)) == 0) {
    out <- FUN(a, b)
  } else {
    out <- FUN(a, b, ...)
  }
  if (rowcoltypes) {
    # Either match_type is 
    #   "all" 
    #     in which case rowtype(a) == rowtype(b) and coltype(a) == coltype(b), 
    #     and setting rowtype to rowtype(a) and coltype to coltype(b) works fine, or
    #   "matmult"
    #     in which case coltype(a) == rowtype(b) and the result of FUN is
    #     a matrix with rowtype == rowtype(a) and coltype == coltype(b).
    # Given those options, we set rowtype to rowtype(a) and coltype to coltype(b).
    return(out %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(b)))
  }
  return(out)
}

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
#' @examples
#' byname:::logmean(0, 0) # 0
#' byname:::logmean(0, 1) # 0
#' byname:::logmean(1, 0) # 0
#' byname:::logmean(1, 1) # 1
#' byname:::logmean(2, 1)
#' byname:::logmean(1, 2) # commutative
#' byname:::logmean(1, 10) # base = exp(1), the default
#' byname:::logmean(1, 10, base = 10)
logmean <- function(x1, x2, base = exp(1)){
  # Take care of pathological cases.
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
  if (is.list(m)) {
    # If m is a list, we need to ensure that the patterns are also lists. 
    retain_pattern <- make_list(retain_pattern, n = length(m))
    remove_pattern <- make_list(remove_pattern, n = length(m))
  }
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
                    retain_pattern = retain_pattern, 
                    remove_pattern = remove_pattern, rowcoltypes = "none")
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
  if (is.list(m)) {
    retain_pattern <- make_list(retain_pattern, n = length(m))
    remove_pattern <- make_list(remove_pattern, n = length(m))
  }
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
  unaryapply_byname(select.func, a = m, 
                    retain_pattern = retain_pattern, 
                    remove_pattern = remove_pattern, rowcoltypes = "none")
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
#' @param m a matrix or data frame from which row sums are desired.
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
#' @param m a matrix or data frame from which column sums are desired.
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
  unaryapply_byname(clean.func, a = m, margin, clean_value, rowcoltypes = "all")
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
#' setrownames_byname(list(m,m), list(c("a", "b"), c("c", "d")))
#' DF <- data.frame(m = I(list()))
#' DF[[1,"m"]] <- m
#' DF[[2,"m"]] <- m
#' setrownames_byname(DF$m, list(c("r1", "r2")))
#' setrownames_byname(DF$m, list(c("a", "b"), c("c", "d")))
#' DF <- DF %>% mutate(m = setrownames_byname(m, list(c("r1", "r2"))))
#' DF$m[[1]]
setrownames_byname <- function(m, rownames){
  if (is.list(m) & !is.matrix(m) & is.vector(rownames) & !is.list(rownames)) {
    # rownames is a vector of names (and not a list) to be applied 
    # to each matrix in m.
    # Thus, we should replicatate it to be same length as m
    rownames <- make_list(rownames, n = length(m), lenx = 1)
  }
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
  unaryapply_byname(rowname.func, a = m, rownames = rownames, rowcoltypes = "all")
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
  if (is.list(m) & !is.matrix(m) & is.vector(colnames) & !is.list(colnames)) {
    # colnames is a vector of names (and not a list) to be applied 
    # to each matrix in m.
    # Thus, we should replicatate it to be same length as m
    colnames <- make_list(colnames, n = length(m), lenx = 1)
  }
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
  unaryapply_byname(colname.func, a = m, colnames = colnames, rowcoltypes = "all")
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
  unaryapply_byname(rt.func, a = x, rowtype = rowtype, rowcoltypes = "none")
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
  unaryapply_byname(ct.func, a = x, coltype = coltype, rowcoltypes = "none")
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
  unaryapply_byname(attr, a = x, which = "rowtype", rowcoltypes = "none")
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
  unaryapply_byname(attr, a = x, which = "coltype", rowcoltypes = "none")
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
  if (is.list(m)) {
    margin <- make_list(margin, n = length(m), lenx = 1)
  }
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
  unaryapply_byname(lrc.func, a = m, margin = margin, rowcoltypes = "none")
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
  unaryapply_byname(zero.func, a = m, tol = tol, rowcoltypes = "none")
}

#' Organize binary arguments
#'
#' Organizes arguments of binary (2 arguments) \code{_byname} functions.
#' Actions performed are:
#' \itemize{
#'  \item{if only one argument is a list, make the other argument also a list of equal length.}
#'  \item{if both arguments are lists, ensure that they are same length.}
#'  \item{if one argument is a matrix and the other is a constant, make the constant into a matrix.}
#'  \item{ensures that row and column types match for \code{typematch_margins}.}
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
      # b is a list, but a is not.  Make a into a list.
      a <- make_list(a, n = length(b))
    }
    if (!is.list(b)) {
      # a is a list, but b is not.  Make b into a list.
      b <- make_list(b, n = length(a))
    }
  }
  if (is.list(a) & is.list(b)) {
    # Both a and b are lists. Ensure they're the same length.
    stopifnot(length(a) == length(b))
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
  if (!match_type %in% c("all", "matmult"))  {
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
  }
  if (match_type == "matmult") {
    # When the match_type is "matmult", we need to ensure that the columns of a match the rows of b.
    # To do so, we transpose b prior to completing and sorting, and we complete and sort on columns.
    matrices <- complete_and_sort(a, transpose_byname(b), margin = 2)
    outa <- matrices$m1 %>% setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
    # Before sending back, we need to re-transpose b.
    outb <- matrices$m2 %>% transpose_byname %>% setrowtype(rowtype(b)) %>% setcoltype(coltype(b))
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

