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
#' A "hat" matrix (or a diagonal matrix) is one in which the only non-zero elements are along on the diagonal.
#' To "hatize" a vector is to place its elements on the diagonal of an otherwise-zero square matrix.
#' `v` must be a matrix object with at least one of its two dimensions of length 1 (i.e., a vector).
#' The names on both dimensions of the hatized matrix are the same and taken from 
#' the dimension of `v` that is _not_ 1.
#' Note that the row names and column names are sorted prior to forming the "hat" matrix.
#' 
#' Hatizing a 1x1 vector is potentially undefined.
#' The argument `keep` 
#' determines whether to keep "rownames" or "colnames".
#' By default `keep` is `NULL`,
#' meanding that the function should attempt to figure out which dimension's names
#' should be used for the hatized matrix on output. 
#' If vector `v` could ever be 1x1, 
#' it is best to set a value for `keep` when writing code
#' that calls `hatize_byname()`.
#' 
#' If the caller specifies `keep = "colnames"` when `v` is a column vector,
#' an error is thrown.
#' If the caller specifies `keep = "rownames"` when `v` is a row vector,
#' an error is thrown.
#'
#' @param v The vector from which a "hat" matrix is to be created.
#' @param keep One of "rownames" or "colnames" or `NULL`.
#'             If `NULL`, the default, names are kept from 
#'             the dimension that is not size 1.
#'
#' @return A square "hat" matrix with size equal to the length of `v`.
#' 
#' @export
#'
#' @examples
#' v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' hatize_byname(v, keep = "rownames")
#' r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatize_byname(r, keep = "colnames")
#' # This also works with lists.
#' hatize_byname(list(v, v), keep = "rownames")
#' # A 1x1 column vector is a degenerate case. 
#' # Row names and rowtype are transferred to the column.
#' matrix(42, nrow = 1, ncol = 1, dimnames = list("r1")) %>% 
#'   setrowtype("Product -> Industry") %>% 
#'   hatize_byname(keep = "rownames")
#' # A 1x1 row vector is a degenerate case. 
#' # Column names and coltype are transferred to the row.
#' matrix(42, nrow = 1, ncol = 1, dimnames = list(NULL, "c1")) %>% 
#'   setcoltype("Industry -> Product") %>% 
#'   hatize_byname(keep = "colnames")
#' # A 1x1 matrix with both row and column names generates a failure.
#' \dontrun{
#' matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
#'   setrowtype("Product -> Industry") %>% 
#'   setcoltype("Industry -> Product") %>% 
#'   hatize_byname()
#' }
#' # But you could specify which you want keep, row names or column names.
#' m <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
#'   setrowtype("Product -> Industry") %>% 
#'   setcoltype("Industry -> Product")
#' m %>% 
#'   hatize_byname(keep = "rownames")
#' m %>% 
#'   hatize_byname(keep = "colnames")
hatize_byname <- function(v, keep = NULL){

  hatize_func <- function(v_vec){
    # Check the v_vec has at least 1 dimension of size 1.
    if (!(nrow(v_vec) == 1 | ncol(v_vec) == 1)) {
      stop ('In hatize_byname(), matrix v must have at least 1 dimension of length 1.')
    }
    if (is.null(keep)) {
      # Test for the indeterminant case
      if (nrow(v_vec) == 1 & ncol(v_vec) == 1) {
        stop('In hatize_byname(), the keep argument must be set to one of "rownames" or "colnames" when v is a 1x1 matrix.')
      }
    }
    # Figure out which names we should keep.
    should_keep <- NULL
    if (nrow(v_vec) > 1 & ncol(v_vec) == 1) {
      # We should keep column names
      should_keep <- "rownames"
    }
    if (nrow(v_vec) == 1 & ncol(v_vec) > 1) {
      # We should keep row names.
      should_keep <- "colnames"
    }
    # Compare dimnames the caller wants to keep against should_keep.
    if (!is.null(keep) & !is.null(should_keep)) {
      if (should_keep == "rownames" & keep == "colnames") {
        stop('In hatize_byname(), argument "keep" set to "colnames", but you supplied a column vector. Consider setting keep = "rownames".')
      }
      if (should_keep == "colnames" & keep == "rownames") {
        stop('In hatize_byname(), argument "keep" set to "rownames", but you supplied a row vector. Consider setting keep = "colnames".')        
      }
    }
    # Issue a warning if the caller didn't specify keep.
    # Uncomment this code after matsbyname has been accepted.
    # if (missing(keep)) {
    #   warning(paste0("In hatize_byname(), keep is missing. Consider setting to '", should_keep, "'."))
    # }
    
    if (is.null(keep)) {
      # Set keep to should_keep to cover the case when keep is NULL.
      keep <- should_keep
    }
    
    # At this point, we should have a vector and we should know which names to keep.
    if (ncol(v_vec) == 1 & nrow(v_vec) == 1) {
      # Don't send this to diag(), because
      # diag() creates a matrix of size v_vec (when v_vec is an integer).
      v_sorted <- v_vec
      out <- v_sorted
    } else {
      v_sorted <- sort_rows_cols(v_vec)
      out <- diag(as.numeric(v_sorted))
    }
    if (keep == "rownames") {
      # Apply the row names to the columns, set the coltype to row rowtype, and return.
      rownames(out) <- rownames(v_sorted)
      colnames(out) <- rownames(v_sorted)
      return(out %>% setrowtype(rowtype(v_vec)) %>% setcoltype(rowtype(v_vec)))
    } else if (keep == "colnames") {
      rownames(out) <- colnames(v_sorted)
      colnames(out) <- colnames(v_sorted)
      return(out %>% setrowtype(coltype(v_vec)) %>% setcoltype(coltype(v_vec)))
    } else {
      stop('In hatize_byname(), argument "keep" must be one of "colnames" or "rownames".')
    }
  }
  unaryapply_byname(hatize_func, a = v, rowcoltypes = "none")
}


#' Hatize and invert a vector
#' 
#' When dividing rows or columns of a matrix by elements of a vector,
#' the vector elements are placed on the diagonal of a new matrix,
#' the diagonal matrix is inverted, and
#' the result is pre- or post-multiplied into the matrix.
#' This function performs the hatizing and inverting of vector `v` in one step
#' and takes advantage of computational efficiencies to achieve the desired result.
#' The computational shortcut is apparent when one observes that the matrix produced by hatizing and inverting
#' a vector is a diagonal matrix whose non-zero elements are the numerical inverses of the individual elements of `v`.
#' So this function first inverts each element of `v` then places the inverted elements on the diagonal of a diagonal matrix.
#' 
#' Note that this function gives the same result as `invert_byname(hatize_byname(v))`,
#' except that `invert_byname(hatize_byname(v))` fails due to a singular matrix error
#' when any of the elements of `v` are zero.
#' This function will give `inf_becomes` on the diagonal of the result for each zero element of `v`,
#' arguably a better answer.
#' The sign of `Inf` is preserved in the substitution.
#' The default value of `Inf_becomes` is `.Machine$double.xmax`.
#' Set `inf_becomes` to `NULL` to disable this behavior.
#' 
#' The default behavior is helpful for cases when the result of `hatinv_byname` is later multiplied by `0`
#' to obtain `0`.
#' Multiplying `Inf` by `0` gives `NaN` which would effectively end the stream of calculations.
#' 
#' @param v The vector to be hatized and inverted.
#' @param keep See `matsbyname::hatize`.
#' @param inf_becomes A value to be substitute for any `Inf` produced by the inversion process. 
#'        Default is `.Machine$double.xmax`.
#'        If `FALSE` (the default), `Inf` is not handled differently.
#'        If `TRUE`, `Inf` values in the resulting matrix are converted to zeroes.
#'
#' @return a square diagonal matrix with inverted elements of `v` on the diagonal
#' 
#' @export
#'
#' @examples
#' v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' hatinv_byname(v, keep = "rownames")
#' hatinv_byname(r, keep = "colnames")
#' # This function also works with lists.
#' hatinv_byname(list(v, v), keep = "rownames")
#' # Watch out for 0 values
#' v2 <- matrix(0:1, ncol = 1, dimnames = list(c(paste0("i", 0:1)), c("p1"))) %>%
#'   setrowtype("Industries") %>% setcoltype(NA)
#' # Produces singular matrix error
#' \dontrun{v2 %>% hatize_byname() %>% invert_byname}
#' # Handles 0 values well
#' hatinv_byname(v2, keep = "rownames")
#' hatinv_byname(v2, inf_becomes = 42, keep = "rownames")
#' hatinv_byname(v2, inf_becomes = NA, keep = "rownames")
#' # Deals with 1x1 matrices well, if the `keep` argument is set.
#' m <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
#'   setrowtype("Product -> Industry") %>% 
#'   setcoltype("Industry -> Product")
#' m %>% 
#'   hatinv_byname(keep = "rownames")
#' m %>% 
#'   hatinv_byname(keep = "colnames")
hatinv_byname <- function(v, keep = NULL, inf_becomes = .Machine$double.xmax){
  hatinv_func <- function(v_vec){
    # Note: there is no need to check that v is, indeed, a vector here.
    # hatize_byname() does that check for us.
    v_inv <- 1/v_vec
    if (!is.null(inf_becomes)) {
      v_inv[v_inv == Inf] <- inf_becomes
      v_inv[v_inv == -Inf] <- -inf_becomes
    }
    hatize_byname(v_inv, keep = keep)
  }
  unaryapply_byname(hatinv_func, a = v, rowcoltypes = "none")
}


#' Named identity matrix or vector
#'
#' Creates an identity matrix (\strong{I}) or vector (\strong{i}) of same size and with same names and
#' same row and column types as \code{a}.
#' 
#' Behaviour for different values of `margin` are as follows:
#' 
#'   * If \code{margin = 1}, makes a column matrix filled with \code{1}s. 
#'     Row names and type are taken from row names and type of \code{a}.
#'     Column name and type are same as column type of \code{a}.
#'   * If \code{margin = 2}, make a row matrix filled with \code{1}s.
#'     Column names and type are taken from column name and type of \code{a}.
#'     Row name and type are same as row type of \code{a}.
#'   * If \code{list(c(1,2))} (the default), make an identity matrix with \code{1}s on the diagonal.
#'     Row and column names are sorted on output.
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
identize_byname <- function(a, margin = c(1,2)) {
  margin <- prep_vector_arg(a, margin)

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
    
    if (1 %in% margin)  {
      # Return a column vector containing 1's
      return(matrix(rep_len(1, nrow(a)), nrow = nrow(a), ncol = 1) %>% 
               setrownames_byname(rownames(a)) %>% setcolnames_byname(coltype(a)) %>% 
               setrowtype(rowtype(a)) %>% setcoltype(coltype(a)))
    }
    if (2 %in% margin) {
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


#' Vectorize a matrix
#' 
#' Converts a matrix into a column vector.
#' Each element of the matrix becomes an entry in the column vector,
#' with rows named via the `notation` argument.
#' Callers may want to transpose the matrix first with `transpose_byname()`.
#' 
#' The `notation` argument is also applied to `rowtype` and `coltype` attributes.
#'
#' @param a the matrix to be vectorized.
#' @param notation a string vector created by `notation_vec()`.
#'
#' @return a column vector containing all elements of `a`, with row names assigned as "rowname `sep` colname".
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(1, 5,
#'               4, 5),
#'             nrow = 2, ncol = 2, byrow = TRUE, 
#'             dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
#'   setrowtype("Products") %>% setcoltype("Industries")
#' m
#' vectorize_byname(m, notation = arrow_notation())
#' # If a single number is provided, the number will be returned as a 1x1 column vector 
#' # with some additional attributes.
#' vectorize_byname(42, notation = arrow_notation())
#' attributes(vectorize_byname(42, notation = arrow_notation()))
vectorize_byname <- function(a, notation) {
  if (is.null(a)) {
    return(NULL)
  }
  if (is.null(notation)) {
    return(a)
  }
  vectorize_func <- function(a_mat, notation) {
    # At this point, a_mat should be a single matrix.
    if (!is.numeric(a_mat)) {
      stop("a is not numeric in vectorize_byname")
    }
    vec <- a_mat
    n_entries <- nrow(vec) * ncol(vec)
    if (length(n_entries) == 0) {
      # Probably have a single number
      n_entries <- 1
    }
    dim(vec) <- c(n_entries, 1)
    # Figure out names, based on notation
    new_rownames <- purrr::cross2(rownames(a_mat), colnames(a_mat)) %>% 
      lapply(FUN = function(ps) {
        ps %>% magrittr::set_names(value = c("pref", "suff")) %>% 
          paste_pref_suff(notation = notation)
      })
    
    # Put names on the rows of the vector
    vec <- vec %>% setrownames_byname(new_rownames) %>% 
      # Eliminate the column names
      setcolnames_byname(NULL)
    
    # Change the rowtype and coltype if both are not NULL
    if (!is.null(rt <- rowtype(a_mat)) & !is.null(ct <- coltype(a_mat))) {
      new_rowtype <- paste_pref_suff(list(pref = rt, suff = ct), notation = notation)
      vec <- vec %>% 
        setrowtype(new_rowtype) %>% 
        setcoltype(NULL)
    }
    return(vec)
  }
  unaryapply_byname(vectorize_func, a = a, .FUNdots = list(notation = notation), rowcoltypes = "none")
}


#' Matricize a vector
#' 
#' Converts a vector with rows or columns named according to `notation`
#' into a matrix.
#'
#' @param a a row (column) vector to be converted to a matrix based on its row (column) names.
#' @param notation a string vector created by `notation_vec()` that identifies the notation for row or column names.
#'
#' @return a matrix created from vector `a`.
#' 
#' @export
#'
#' @examples
#' v <- matrix(c(1,
#'               2,
#'               3, 
#'               4), 
#'             nrow = 4, ncol = 1, dimnames = list(c("p1 -> i1", 
#'                                                   "p2 -> i1", 
#'                                                   "p1 -> i2", 
#'                                                   "p2 -> i2"))) %>% 
#'   setrowtype("Products -> Industries")
#' # Default separator is " -> ".
#' matricize_byname(v, notation = arrow_notation())
matricize_byname <- function(a, notation) {
  matricize_func <- function(a_mat, notation) {
    # At this point, we should have a single matrix a_mat.
    # Make sure we have the right number of dimensions, i.e. 2.
    dimsa_mat <- dim(a_mat)
    if (length(dimsa_mat) != 2) {
      stop("a must have length(dim(a)) == 2 in matricize_byname")
    }
    # Check if this is a column vector or a row vector
    if ((dimsa_mat[[1]] == 1 & dimsa_mat[[2]] != 1) | 
        (dimsa_mat[[1]] == 1 & dimsa_mat[[2]] == 1 & is.null(dimnames(a_mat)[[1]]))) {
      # This is a row vector and not a 1x1 "vector".
      # Transpose to a column vector, then re-call this function.
      return(transpose_byname(a_mat) %>% matricize_func(notation))
    }
    # If we get here, we know we have a column vector.
    # Gather row names of the vector.
    rownames_a <- dimnames(a_mat)[[1]]
    # Split row names by notation
    matrix_row_col_names <- split_pref_suff(rownames_a, notation = notation)
    if (nrow(a_mat) > 1) {
      # We have more than 1 row of names.
      # Thus, we need to transpose the list.
      matrix_row_col_names <- purrr::transpose(matrix_row_col_names)
    }
    # Get the matrix row and column names separately.
    .rownames <- matrix_row_col_names %>% 
      magrittr::extract2(1) %>% 
      unlist()
    .colnames <- matrix_row_col_names %>% 
      magrittr::extract2(2) %>% 
      unlist()
    uniquerownames <- unique(.rownames)
    uniquecolnames <- unique(.colnames)
    rid <- 1:length(uniquerownames)
    cid <- 1:length(uniquecolnames)
    # Identify row and column numbers
    rnames <- "rownames"
    cnames <- "colnames"
    ridname <- "rid"
    cidname <- "cid"
    rownametable <- tibble::tibble(
      1:length(uniquerownames),
      uniquerownames
    ) %>% magrittr::set_names(c(ridname, rnames))
    colnametable <- tibble::tibble(
      1:length(uniquecolnames),
      uniquecolnames
    ) %>% magrittr::set_names(c(cidname, cnames))

    # Set up a concordance table rowname, rownumber, colname, colnumber, value
    values <- "values"
    mat_info <- tibble::tibble(
      !!rnames := .rownames,
      !!cnames := .colnames,
      !!values := a_mat[ , 1]
    ) %>%
      dplyr::left_join(rownametable, by = rnames) %>% 
      dplyr::left_join(colnametable, by = cnames)
      
    # Create a zero matrix of correct dimension
    m <- matrix(0, 
                nrow = nrow(rownametable), ncol = nrow(colnametable), 
                dimnames = list(rownametable[[rnames]], 
                                colnametable[[cnames]]))
    # Fill it with data from the concordance table
    for (r in 1:nrow(mat_info)) {
      rownum <- mat_info[[ridname]][r]
      colnum <- mat_info[[cidname]][r]
      value <- mat_info[[values]][r]
      m[rownum, colnum] <- value
    }
    # Add row and column types after splitting the rowtype of a_mat
    rt <- rowtype(a_mat)
    rctypes <- split_pref_suff(rt, notation = notation)
    m %>% setrowtype(rctypes[["pref"]]) %>% setcoltype(rctypes[["suff"]])
  } 
  unaryapply_byname(matricize_func, a = a, .FUNdots = list(notation = notation), rowcoltypes = "none")
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
#' 
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
  margin <- prep_vector_arg(a, margin)

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
    
    if (1 %in% margin) {
      # Divide each entry by its row sum
      # Do this with (a*i)_hat_inv * a
      # return(matrixproduct_byname(a %>% rowsums_byname %>% hatize_byname %>% invert_byname, a))
      return(sweep(a, margin, rowSums(a), `/`))
    }
    if (2 %in% margin) {
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
#' In contrast to `rowSums` (which returns a `numeric` result),
#' the return value from `rowsums_byname` is a matrix.
#' An optional `colname` for the resulting column vector can be supplied.
#' If `colname` is `NULL` or `NA` (the default),
#' the column name is set to the column type as given by `coltype(a)`.
#' If `colname` is set to `NULL`, the column name is returned empty.
#'
#' @param a A matrix or list of matrices from which row sums are desired.
#' @param colname The name of the output column containing row sums.
#'
#' @return A column vector of type `matrix` containing the row sums of `m`
#' @export
#'
#' @examples
#' library(dplyr)
#' m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("c", 1:2))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' m
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
  # if (is.null(colname)) {
  #   # Set to NA so that we can try setting to coltype in rowsum.func
  #   colname <- NA_character_
  # }
  rowsum_func <- function(a, colname){
    if (!is.null(colname)) {
      if (is.na(colname)) {
        colname <- coltype(a)
      }
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
#' In contrast to `colSums` (which returns a `numeric` result),
#' the return value from `colsums_byname` is a matrix.
#' An optional `rowname` for the resulting row vector can be supplied.
#' If `rowname` is `NA` (the default),
#' the row name is set to the row type as given by `rowtype(a)`.
#' If `rowname` is set to `NULL`, the row name is returned empty.
#'
#' @param a a matrix or list of matrices from which column sums are desired.
#' @param rowname name of the output row containing column sums.
#'
#' @return a row vector of type `matrix` containing the column sums of `a`.
#' @export
#'
#' @examples
#' library(dplyr)
#' m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 3:1))) %>%
#'   setrowtype("Industries") %>% setcoltype("Commodities")
#' m
#' colsums_byname(m)
#' colsums_byname(m, rowname = "E.ktoe")
#' m %>% 
#'   colsums_byname() %>% 
#'   rowsums_byname()
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
#' library(tibble)
#' m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>% 
#'   setrowtype("row") %>% setcoltype("col")
#' cumsum_byname(list(m1, m2, m3))
#' # Groups are respected in the context of mutate.
#' tibble(grp = c("A", "A", "B"), m = list(m1, m2, m3)) %>% group_by(grp) %>% 
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


#' Aggregate rows and columns in a matrix
#' 
#' Rows (`margin  1`), columns (`margin = 2`), or both (`margin = c(1, 2)`, the default)
#' are aggregated according to `aggregation_map`.
#' 
#' When `aggregation_map` is `NULL` (the default), 
#' rows (or columns or both) of same name are aggregated together. 
#' 
#' If `aggregation_map` is not `NULL`, it must be a named list.
#' The name of each `aggregation_map` item is the name of a row or column in `a` that will contain the specified aggregation.
#' The value of each item in `aggregation_map` must be a vector of names of rows or columns in `a`.
#' The names in the value are aggregated and inserted into `a` with the name of the value.
#' For example `aggregation_map = list(new_row = c("r1", "r2"))` 
#' will aggregate rows "r1" and "r2", delete rows "r1" and "r2", and insert a new row 
#' whose name is "new_row" and whose value is the sum of rows "r1" and "r2'.
#' 
#' The items in `aggregation_map` are interpreted as regular expressions, and 
#' they are escaped using `Hmisc::escapeRegex()` prior to use.
#' 
#' Note that aggregation on one margin only will sort only the aggregated margin, because
#' the other margin is not guaranteed to have unique names.
#'
#' @param a a matrix or list of matrices whose rows or columns are to be aggregated
#' @param aggregation_map a named list of rows or columns to be aggregated (or `NULL`). See `details`.
#' @param margin `1`, `2`, or `c(1, 2)` for row aggregation, column aggregation, or both
#' @param pattern_type See `make_pattern()`.
#'
#' @return a version of `a` with aggregated rows and/or columns
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' m <- matrix(1:9, byrow = TRUE, nrow = 3, 
#'             dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1"))) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' # Aggregate all rows by establishing an aggregation map (am)
#' am <- list(new_row = c("r1", "r2"))
#' aggregate_byname(m, aggregation_map = am, margin = 1)
#' # aggregate_byname() also works with lists and in data frames
#' m1 <- matrix(42, nrow = 1, dimnames = list(c("r1"), c("c1")))
#' m2 <- matrix(1:4, byrow = TRUE, nrow = 2, 
#'              dimnames = list(c("a", "a"), c("a", "a")))
#' m3 <- matrix(1:9, byrow = TRUE, nrow = 3, 
#'              dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1")))
#' DF <- tibble(m = list(m1, m1, m1, m2, m2, m2, m3, m3, m3), 
#'              margin = list(1, 2, c(1,2), 1, 2, c(1, 2), 1, 2, c(1, 2))) %>% 
#'   mutate(
#'     aggregated = aggregate_byname(m, margin = margin), 
#'   )
#' m1
#' DF$aggregated[[1]] # by rows
#' DF$aggregated[[2]] # by cols
#' DF$aggregated[[3]] # by rows and cols
#' m2
#' DF$aggregated[[4]] # by rows
#' DF$aggregated[[5]] # by cols
#' DF$aggregated[[6]] # by rows and cols
#' m3
#' DF$aggregated[[7]] # by rows
#' DF$aggregated[[8]] # by cols
#' DF$aggregated[[9]] # by rows and cols
aggregate_byname <- function(a, aggregation_map = NULL, margin = c(1, 2), pattern_type = "exact") {
  margin <- prep_vector_arg(a, margin)
  
  agg_func <- function(a_mat, aggregation_map, margin, pattern_type) {
    # If we get here, a should be a single matrix.
    assertthat::assert_that(all(margin %in% c(1, 2)))
    # Create our own aggregation_map if it is NULL
    if (is.null(aggregation_map)) {
      rcnames <- list()
      if (1 %in% margin) {
        rcnames[["rnames"]] <- rownames(a_mat)
      }
      if (2 %in% margin) {
        rcnames[["cnames"]] <- colnames(a_mat)
      }
      aggregation_map <- lapply(rcnames, FUN = function(x) {
        # x is one of the sets of row or column names
        # Look for all duplicated names in x
        dupes <- x[duplicated(x)]
        if (length(dupes) == 0) {
          return(NULL)
        }
        # Get rid of extras to get the list of names to aggregate
        names_to_aggregate <- unique(dupes)
        names_to_aggregate <- magrittr::set_names(names_to_aggregate, names_to_aggregate)
        return(names_to_aggregate)
      }) %>% 
        magrittr::set_names(NULL) %>% 
        unique() %>% 
        unlist()
      # If we still have a NULL aggregation_map (i.e., we didn't find any rows or cols that need to be aggregated),
      # just return our original matrix (a).
      if (is.null(aggregation_map)) {
        return(a_mat)
      }
    }
    out <- a_mat
    if (2 %in% margin) {
      # Want to aggregate columns.
      # Easier to transpose, re-call ourselves to aggregate rows, and then transpose again.
      out <- t(a_mat) %>% 
        agg_func(aggregation_map = aggregation_map, margin = 1, pattern_type = pattern_type) %>% 
        t()
    }
    if (1 %in% margin) {
      for (i in 1:length(aggregation_map)) {
        # Isolate rows to be aggregated
        select_pattern <- make_pattern(row_col_names = aggregation_map[[i]], pattern_type = pattern_type)
        rows_to_aggregate <- select_rows_byname(out, retain_pattern = select_pattern)
        if (!is.null(rows_to_aggregate)) {
          # Sum the isolated rows (if any)
          # aggregated_rows <- colsums_byname(rows_to_aggregate, rowname = names(aggregation_map[i]))
          aggregated_rows <- colSums(rows_to_aggregate) %>% 
            # Sadly, colSums simplifies 1-dimensional output to a vector. 
            # So, remake the matrix.
            matrix(nrow = 1, dimnames = list(c(names(aggregation_map[i])), c(colnames(rows_to_aggregate))))
          # If we found rows to aggregate, remove from a the rows that were aggregated and ...
          out <- out %>% 
            select_rows_byname(remove_pattern = select_pattern)
          if (is.null(out)) {
            # If we aggregated all rows that were in a, out will be NULL. 
            # In that case, we can return the aggregated rows that we pulled out.
            out <- aggregated_rows
          } else {
            # out is not NULL, we we need to add the aggregated rows to the remaining rows.
            out <- out %>% 
              rbind(aggregated_rows) 
          }
        }
      }
      # Note: Can't sort on columns, because they are not guaranteed to be unique.
      out <- sort_rows_cols(out, margin = 1)
    }
    return(out)
  }

  unaryapply_byname(agg_func, a, 
                    .FUNdots = list(aggregation_map = aggregation_map, margin = margin, pattern_type = pattern_type))
}


#' Aggregate a matrix to prefixes or suffixes of row and/or column names
#' 
#' Row and column names are often constructed in the form 
#' `prefix_start` `prefix` `prefix_end` `suffix_start` `suffix` `suffix_end`
#' and described by a notation vector.
#' (See `notation_vec()`.)
#' This function performs aggregation by prefix or suffix according to a notation vector..
#' 
#' This function is a convenience function, as it bundles sequential calls to two helper functions,
#' `rename_to_pref_suff_byname()` and `aggregate_byname()`.
#' All arguments are passed to the helper functions.
#'
#' @param a a matrix of list of matrices to be aggregated by prefix or suffix
#' @param aggregation_map See `aggregate_byname()`.
#' @param notation See `notation_vec()`. 
#' @param keep See `rename_to_pref_suff_byname()`
#' @param margin the dimension over which aggregation is to be performed; `1` for rows, `2` for columns, or `c(1, 2)` for both.
#' @param pattern_type See `aggregate_byname()`.
#'
#' @return an aggregated version of `a`
#' 
#' @export
#'
#' @examples
#' m <- matrix((1:9), byrow = TRUE, nrow = 3, 
#'             dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")))
#' m
#' # Aggregation by prefixes does nothing more than rename, because all prefixes are different.
#' # Doing renaming like this (without also aggregating) is potentially dangerous, because  
#' # some rows and some columns could end up with same names.
#' aggregate_to_pref_suff_byname(m, keep = "prefix", notation = arrow_notation())
#' # Aggregation by suffix reduces the number of rows and columns, 
#' # because there are same suffixes in both rows and columns
#' aggregate_to_pref_suff_byname(m, keep = "suffix", notation = arrow_notation())
aggregate_to_pref_suff_byname <- function(a, aggregation_map = NULL, 
                                          keep, margin = c(1, 2), notation,
                                          pattern_type = "exact") {
  a %>% 
    rename_to_pref_suff_byname(keep = keep, margin = margin, notation = notation) %>% 
    aggregate_byname(aggregation_map = aggregation_map, margin = margin, pattern_type = pattern_type)
}
