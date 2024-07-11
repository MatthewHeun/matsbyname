#' Absolute value of matrix elements
#'
#' @param a A matrix or list of matrices.
#'
#' @return `a` with each element replaced by its absolute value.
#' 
#' @export
#'
#' @examples
#' abs_byname(1)
#' abs_byname(-1)
#' m <- matrix(c(-10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
#' abs_byname(m)
abs_byname <- function(a){
  unaryapply_byname(abs, a = a)
}


#' Logarithm of matrix elements
#' 
#' Specify the base of the log with `base` argument.
#'
#' @param a A matrix or list of matrices.
#' @param base The base of the logarithm (default is `exp(1)`, giving the natural logarithm).
#'
#' @return M with each element replaced by its base `base` logarithm
#' 
#' @export
#'
#' @examples
#' log_byname(exp(1))
#' m <- matrix(c(10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
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
#' m <- matrix(c(log(10),log(1),
#'               log(1),log(100)), 
#'               byrow = TRUE, nrow = 2, ncol = 2,
#'               dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
#' exp_byname(m)
exp_byname <- function(a){
  unaryapply_byname(exp, a = a)
}


#' Invert a matrix
#'
#' This function transposes row and column names as well as row and column types.
#' Rows and columns of `a` are sorted prior to inverting.
#' 
#' The `method` argument specifies which method should be used for 
#' calculating the inverse. 
#' "solve" uses `base::solve()` and the value of `tol`.
#' "QR" uses `base::solve.qr()` and the value of `tol`.
#' "SVD" uses `matrixcalc::svd.inverse()`, ignoring the `tol` argument.
#' 
#' Both `tol` and `method` should be a single values and apply to all matrices in `a`.
#'
#' If `a` is a singular matrix, 
#' names of zero rows and columns are reported in the error message.
#' 
#' @param a The matrix to be inverted. `a` must be square.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol The tolerance for detecting linear dependencies in the columns of `a`. 
#'            Default is `.Machine$double.eps`. 
#'
#' @return The inversion of `a`.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
#' invert_byname(m)
#' matrixproduct_byname(m, invert_byname(m))
#' matrixproduct_byname(invert_byname(m), m)
#' invert_byname(list(m,m))
#' invert_byname(m, method = "QR")
#' invert_byname(m, method = "SVD")
invert_byname <- function(a, 
                          method = c("solve", "QR", "SVD"), 
                          tol = .Machine$double.eps) {
  method <- match.arg(method)
  invert_func <- function(a_mat) {
    invert_matrix_or_Matrix(a_mat, method = method, tol = tol)
  }
  unaryapply_byname(invert_func, a = a, rowcoltypes = "transpose")
}


#' Transpose a matrix by name
#'
#' Gives the transpose of a matrix or list of matrices.
#'
#' @param a The matrix to be transposed.
#'
#' @return The transposed matrix.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
#' transpose_byname(m)
#' transpose_byname(list(m,m))
transpose_byname <- function(a) {
  transpose_func <- function(a_mat) {
    if (is_matrix_or_Matrix(a_mat)) {
      return(t_matrix_or_Matrix(a_mat))
    }
    # In the event that we don't have a matrix, 
    # we probably have a single number.
    # Don't try to transpose.
    return(a_mat)
  }
  unaryapply_byname(transpose_func, a = a, rowcoltypes = "transpose")
}


#' Calculate eigenvalues of a matrix
#' 
#' Calculate the eigenvalues of a matrix or a list of matrices.
#' 
#' This function pairs with `eigenvectors_byname()`;
#' the first value of the result is the eigenvalue
#' for the eigenvector reported in the first column of the result from `eigenvectors_byname()`.
#' The second value of the result is the eigenvalue
#' for the eigenvector reported in the second column of the result from `eigenvectors_byname()`.
#' Etc. 
#' 
#' Internally, this function uses `base::eigen(only.values = TRUE)`.
#' 
#' `complete_rows_cols()` is called prior to calculating the eigenvalues.
#'
#' @param a A matrix or list of matrices.
#'
#' @return A vector of eigenvalues.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c( 4,  6, 10, 
#'                3, 10, 13, 
#'               -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
#'             dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
#' m
#' eigenvalues_byname(m)
#' eigenvalues_byname(list(m, 2*m))
#' DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
#'   dplyr::mutate(
#'     eigen_col = eigenvalues_byname(m_col)
#'   )
#' DF$eigen_col[[1]]
#' DF$eigen_col[[2]]
eigenvalues_byname <- function(a) {
  eigenvals_func <- function(a_mat) {
    completed_mat <- complete_rows_cols(a_mat)
    eigen(completed_mat, only.values = TRUE)[["values"]]
  }
  unaryapply_byname(eigenvals_func, a = a, rowcoltypes = "none")
}


#' Calculate eigenvectors of a matrix
#' 
#' Calculate the eigenvectors of a matrix or a list of matrices.
#' 
#' This function pairs with `eigenvalues_byname()`;
#' the first column of the resulting matrix is the eigenvector
#' for the first eigenvalue reported by `eigenvalues_byname()`.
#' The second column of the resulting matrix is the eigenvector
#' for the second eigenvalue reported by `eigenvalues_byname()`.
#' Etc.
#' 
#' Internally, this function uses `base::eigen()`.
#' 
#' `complete_rows_cols()` is called prior to calculating the eigenvectors.
#'
#' @param a A matrix or list of matrices.
#'
#' @return A matrix whose columns are the eigenvectors of `a`.
#' 
#' @export
#'
#' @examples
#' m <- matrix(c( 4,  6, 10, 
#'                3, 10, 13, 
#'               -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
#'             dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
#' m
#' eigenvectors_byname(m)
#' eigenvectors_byname(list(m, 2*m))
#' DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
#'   dplyr::mutate(
#'     eigen_col = eigenvectors_byname(m_col)
#'   )
#' DF$eigen_col[[1]]
#' DF$eigen_col[[2]]
eigenvectors_byname <- function(a) {
  eigenvecs_func <- function(a_mat) {
    completed_mat <- complete_rows_cols(a_mat)
    eigen(completed_mat)[["vectors"]]
  }
  unaryapply_byname(eigenvecs_func, a = a, rowcoltypes = "none")
}


#' Calculate the singular value decomposition of a matrix
#'
#' The singular value decomposition decomposes matrix **A** into
#' **A** = **U** **D** **V**^T, 
#' where **U** and **V** are orthogonal matrices and **D** is a diagonal matrix.
#' **U** is the left singular vectors of **A**. 
#' **V** is the right singular vectors of **A**.
#' 
#' `which` determines the part of the singular value decomposition to be returned.
#' "d" (default) gives the **D** matrix.
#' "u" gives the **U** matrix.
#' "v" gives the **V** matrix (not its transpose).
#' 
#' @param a A matrix to be decomposed.
#' @param which The matrix to be returned. Default is "d". See details.
#'
#' @return A matrix of the singular value decomposition of `a`.
#' 
#' @export
#'
#' @examples
#' A = matrix(c(4, 0, 
#'              3, -5), nrow = 2, ncol = 2, byrow = TRUE, 
#'            dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
#'   setrowtype("Product") %>% setcoltype("Industry")
#' A
#' svd_byname(A) # Gives D matrix, by default
#' svd_byname(A, which = "d")
#' svd_byname(A, which = "u")
#' svd_byname(A, which = "v")
svd_byname <- function(a, which = c("d", "u", "v")) {
  which <- match.arg(which)
  svd_func <- function(a_mat) {
    res <- svd(a_mat)[[which]]
    if (which == "d") {
      res <- diag(res)
      rownames(res) <- rownames(a_mat)
      colnames(res) <- colnames(a_mat)
      res <- res %>% 
        setrowtype(rowtype(a_mat)) %>% 
        setcoltype(coltype(a_mat))
    } else if (which == "u") {
      rownames(res) <- rownames(a_mat)
      colnames(res) <- rownames(a_mat)
      res <- res %>% 
        setrowtype(rowtype(a_mat)) %>% 
        setcoltype(rowtype(a_mat))
    } else if (which == "v") {
      rownames(res) <- colnames(a_mat)
      colnames(res) <- colnames(a_mat)
      res <- res %>% 
        setrowtype(coltype(a_mat)) %>% 
        setcoltype(coltype(a_mat))
    }
  }
  unaryapply_byname(svd_func, a = a, rowcoltypes = "none")
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
#' meaning that the function should attempt to figure out which dimension's names
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
#' v
#' hatize_byname(v, keep = "rownames")
#' r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
#'   setrowtype(NA) %>% setcoltype("Commodities")
#' r
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
#' m
#' m %>% 
#'   hatize_byname(keep = "rownames")
#' m %>% 
#'   hatize_byname(keep = "colnames")
hatize_byname <- function(v, keep = NULL) {

  hatize_func <- function(v_vec){
    # Check the v_vec has at least 1 dimension of size 1.
    if (!(nrow(v_vec) == 1 | ncol(v_vec) == 1)) {
      stop ('In hatize_byname(), matrix v must have at least 1 dimension of length 1.')
    }
    # Figure out which names we should keep, based on the structure of v_vec.
    should_keep <- NULL
    if (nrow(v_vec) == 1 & ncol(v_vec) == 1) {
      # Check if one of row/col names is specified but the other is not.
      if (!is.null(rownames(v_vec)) & is.null(colnames(v_vec))) {
        should_keep <- "rownames"
      } else if (is.null(rownames(v_vec)) & !is.null(colnames(v_vec))) {
        should_keep <- "colnames"
      } else if (is.null(rownames(v_vec)) & is.null(rownames(v_vec))) {
        should_keep <- NULL
      } else if (!is.null(rownames(v_vec)) & !is.null(rownames(v_vec))) {
        should_keep <- keep
      } 
    } else if (nrow(v_vec) > 1 & ncol(v_vec) == 1) {
      # We should keep column names
      should_keep <- "rownames"
    } else if (nrow(v_vec) == 1 & ncol(v_vec) > 1) {
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

    if (is.null(keep)) {
      # Set keep to should_keep to cover the case when keep is NULL.
      keep <- should_keep
    }
    if (is.null(keep)) {
      stop("Unable to determine which names to keep (rows or cols) in hatize_byname(). Try setting the 'keep' argument.")
    }
    
    # At this point, we should have a vector and we should know which names to keep.
    if (ncol(v_vec) == 1 & nrow(v_vec) == 1) {
      # Don't send this to diag(), because
      # diag() creates a matrix of size v_vec (when v_vec is an integer).
      v_sorted <- v_vec
      out <- v_sorted
    } else {
      v_sorted <- sort_rows_cols(v_vec)
      if (is.Matrix(v_sorted)) {
        out <- Matrix::Diagonal(x = as.matrix(v_sorted))
      } else {
        out <- diag(as.numeric(v_sorted))
      }
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
#' The default value of `inf_becomes` is `.Machine$double.xmax`.
#' Set `inf_becomes` to `NULL` to disable this behavior.
#' 
#' The default behavior is helpful for cases when the result of `hatinv_byname()` is later multiplied by `0`
#' to obtain `0`.
#' Multiplying `Inf` by `0` gives `NaN` which would effectively end the stream of calculations.
#' 
#' @param v The vector to be hatized and inverted.
#' @param keep See `hatize_byname()`.
#' @param inf_becomes A value to be substitute for any `Inf` produced by the inversion process. 
#'        Default is `.Machine$double.xmax`. 
#'        Another reasonable value is `Inf`.
#'        Set to `NULL` to disable substitution.
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
      if (is.Matrix(v_vec)) {
        # Matrix objects lose their rowtype and coltype at this point.
        v_inv <- v_inv %>% 
          setrowtype(rowtype(v_vec)) %>% 
          setcoltype(coltype(v_vec))
      }
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
      stop("margin should have length 1 or 2 in identize_byname")
    }
    
    if (length(margin) == 2 && all(margin %in% c(1,2))) {
      # a is a matrix. 
      # Return the identity matrix with 1's on diagonal,
      # of same dimensions as a
      # and same names and types as a.
      stopifnot(nrow(a) == ncol(a))
      if (is.Matrix(a)) {
        out <- Matrix::Diagonal(n = nrow(a), x = 1)
      } else {
        out <- diag(nrow(a))
      }
      return(out %>% 
               setrownames_byname(rownames(a)) %>% setcolnames_byname(colnames(a)) %>% 
               setrowtype(rowtype(a)) %>% setcoltype(coltype(a)))
    }
    
    if (length(margin) != 1 || !(margin %in% c(1,2))) {
      stop(paste("Unknown margin", margin, "in identize_byname. margin should be 1, 2, or c(1,2)."))
    }
    
    if (1 %in% margin)  {
      # Return a column vector containing 1's
      if (is.Matrix(a)) {
        out <- matsbyname::Matrix(rep_len(1, nrow(a)), nrow = nrow(a), ncol = 1)
      } else {
        out <- matrix(rep_len(1, nrow(a)), nrow = nrow(a), ncol = 1)
      }
      out <- out %>% 
        setrownames_byname(rownames(a)) %>% setcolnames_byname(coltype(a))
    }
    if (2 %in% margin) {
      # Return a row vector containing 1's
      if (is.Matrix(a)) {
        out <- matsbyname::Matrix(rep_len(1, ncol(a)), nrow = 1, ncol = ncol(a))
      } else {
        out <- matrix(rep_len(1, ncol(a)), nrow = 1, ncol = ncol(a))
      }
      out <- out %>% 
        setrownames_byname(rowtype(a)) %>% setcolnames_byname(colnames(a))
    } 
    out %>% 
      setrowtype(rowtype(a)) %>% setcoltype(coltype(a))
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
#' The `notation` is also applied to `rowtype` and `coltype` attributes.
#'
#' @param a The matrix to be vectorized.
#' @param notation A string vector created by `notation_vec()`.
#'
#' @return A column vector containing all elements of `a`, with row names assigned as "rowname `sep` colname".
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
#' vectorize_byname(m, notation = RCLabels::arrow_notation)
#' # If a single number is provided, the number will be returned as a 1x1 column vector 
#' # with some additional attributes.
#' vectorize_byname(42, notation = RCLabels::arrow_notation)
#' attributes(vectorize_byname(42, notation = RCLabels::arrow_notation))
vectorize_byname <- function(a, notation) {
  if (is.null(a)) {
    return(NULL)
  }
  if (is.null(notation)) {
    return(a)
  }
  vectorize_func <- function(a_mat, notation) {
    # At this point, a_mat should be a single matrix.
    if (!(is.numeric(a_mat) | is.Matrix(a_mat))) {
      stop("a is not numeric or a Matrix in vectorize_byname")
    }
    rnames_df <- data.frame(rname = rownames(a_mat)) %>% 
      tibble::rowid_to_column("i")
    cnames_df <- data.frame(cname = colnames(a_mat)) %>% 
      tibble::rowid_to_column("j")
    vector_df <- Matrix::mat2triplet(a_mat, uniqT = TRUE) %>% 
      data.frame()
    if (!is.null(rownames(a_mat)) & !is.null(colnames(a_mat))) {
      # Add row names to vector_df
      vector_df <- vector_df %>% 
        dplyr::left_join(rnames_df, by = "i") %>% 
        dplyr::left_join(cnames_df, by = "j") %>% 
        dplyr::mutate(
          vec_rowname = RCLabels::paste_pref_suff(pref = .data[["rname"]], suff = .data[["cname"]], notation = notation)
        ) %>% 
        tibble::column_to_rownames("vec_rowname")  
    }
    vec <- vector_df %>% 
      dplyr::mutate(
        # Eliminate columns we don't need
        i = NULL, 
        j = NULL, 
        rname = NULL, 
        cname = NULL
      ) %>% 
      # And convert to a matrix
      as.matrix() %>% 
      magrittr::set_colnames(NULL)
    if (is.null(dimnames(a_mat))) {
      dimnames(vec) <- NULL
    }
    if (is.Matrix(a_mat)) {
      vec <- matsbyname::Matrix(vec)
    }
    
    # Change the rowtype and coltype if both are not NULL
    if (!is.null(rt <- rowtype(a_mat)) & !is.null(ct <- coltype(a_mat))) {
      new_rowtype <- RCLabels::paste_pref_suff(pref = rt, suff = ct, notation = notation)
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
#' into a `matrix` or a `Matrix`, depending on the type of `a`.
#' Row and column types of the output are taken from the 
#' row or column type of the long dimension of the incoming vector.
#' If the row or column type of the long dimension of the incoming vector is `NULL`,
#' the outgoing matrix will have `NULL` rowtype and `NULL` coltype.
#'
#' @param a A row (column) vector to be converted to a matrix based on its row (column) names.
#' @param notation A string vector created by `RCLabels::notation_vec()` that identifies the notation for row or column names.
#'
#' @return A matrix created from vector `a`.
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
#' matricize_byname(v, notation = RCLabels::arrow_notation)
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
    matrix_row_col_names <- RCLabels::split_pref_suff(rownames_a, notation = notation)
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
    # Convert to a Matrix if that's what came in.
    if (is.Matrix(a_mat)) {
      m <- matsbyname::Matrix(m)
    }
    # Add row and column types after splitting the rowtype of a_mat
    rt <- rowtype(a_mat)
    if (is.null(rt)) {
      out <- m |> 
        setrowtype(NULL) |> 
        setcoltype(NULL)
    } else {
      # rt is not NULL
      rctypes <- RCLabels::split_pref_suff(rt, notation = notation)
      out <- m |> 
        setrowtype(rctypes[["pref"]]) |> 
        setcoltype(rctypes[["suff"]])
    }
    return(out)
  } 
  unaryapply_byname(matricize_func, a = a, .FUNdots = list(notation = notation), rowcoltypes = "none")
}


#' Compute fractions of matrix entries
#' 
#' This function divides all entries in `a` by the specified sum,
#' thereby "fractionizing" the matrix.
#'
#' @param a The matrix to be fractionized.
#' @param margin If `1` (rows), each entry in `a` is divided by its row's sum.
#'               If `2` (columns), each entry in `a` is divided by its column's sum.
#'               If `c(1,2)` (both rows and columns), 
#'               each entry in `a` is divided by the sum of all entries in `a`.
#' @param inf_becomes A value to be substitute for any `Inf` produced by division. 
#'                    Default is `.Machine$double.xmax`. 
#'                    Another reasonable value is `Inf`.
#'                    Set to `NULL` to disable substitution.
#'                    `inf_becomes` is passed to `hatinv_byname()`.
#' @return A fractionized matrix of same dimensions and same row and column types as `a`.
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
fractionize_byname <- function(a, margin, inf_becomes = .Machine$double.xmax){
  margin <- prep_vector_arg(a, margin)

  fractionize_func <- function(a, margin){
    if (!inherits(a, "matrix") && !is.Matrix(a) && !inherits(a, "data.frame")) {
      # Assume we have a single number here
      if (a == 0) {
        return(inf_becomes)
      }
      # Now we can't divide by zero.
      # We should have a/a here, which is always 1
      return(1)
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
      # Could do this with (a*i)_hat_inv * a, 
      # but singular matrix problems can arise.
      return(matrixproduct_byname(a %>% 
                                    rowsums_byname() %>% 
                                    hatinv_byname(inf_becomes = inf_becomes), 
                                  a))
      # The next line works only for matrix objects, not Matrix objects.
      # return(sweep(a, margin, rowSums(a), `/`))
    }
    if (2 %in% margin) {
      # Divide each entry by its column sum
      # Could do this with a * (i^T * a)_hat_inv, 
      # but singula matrix problems can arise.
      return(matrixproduct_byname(a, 
                                  a %>% 
                                    colsums_byname() %>% 
                                    hatinv_byname(inf_becomes = inf_becomes)))
      # The next line works only for matrix objects, not Matrix objects.
      # return(sweep(a, margin, colSums(a), `/`))
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
#' @return A column vector of type `matrix` containing the row sums of `m`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' rowsums_byname(42)
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
  rowsum_func <- function(a_mat, colname){
    if (!is.null(colname)) {
      if (is.na(colname)) {
        colname <- coltype(a_mat)
      }
    }
    if (is_matrix_or_Matrix(a_mat)) {
      if (is.Matrix(a_mat)) {
        out <- a_mat %>% 
          Matrix::rowSums() %>% 
          matsbyname::Matrix(nrow = nrow(a_mat), ncol = 1)
      } else {
        out <- a_mat %>% 
          rowSums() %>%
          # Preserve matrix structure (i.e., result will be a column vector of type matrix)
          matrix(ncol = 1) 
      }
      out <- out %>% 
        # Preserve row names
        setrownames_byname(rownames(a_mat)) %>%
        # Set column name
        setcolnames_byname(colname) %>%
        # But sort the result on names
        sort_rows_cols()
    } else if (is.numeric(a_mat)) {
      out <- a_mat
    } else {
      stop("Unknown type for 'a' in rowsums_byname(). 'a' must be a matrix or numeric.")
    }
    # Set types and return
    out %>% 
      setrowtype(rowtype(a_mat)) %>%
      setcoltype(coltype(a_mat))
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
#' @param a A matrix or list of matrices from which column sums are desired.
#' @param rowname The name of the output row containing column sums.
#'
#' @return A row vector of type `matrix` containing the column sums of `a`.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' colsums_byname(42)
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
  
  colsum_func <- function(a_mat, rowname){

    if (is.matrix(a_mat) | is.Matrix(a_mat)) {
      return(a_mat %>% 
               transpose_byname() %>% 
               rowsums_byname(colname = rowname) %>% 
               transpose_byname())
    } else if (is.numeric(a_mat)) {
      return(a_mat)
    } else {
      stop("Unknown type for 'a' in colsums_byname(). 'a' must be a matrix or numeric.")
    }
  }
  unaryapply_byname(colsum_func, a = a, .FUNdots = list(rowname = rowname), 
                    rowcoltypes = "none")
}


#' Sum of all elements in a matrix
#'
#' This function is equivalent to `a \%>\% rowsums_byname() \%>\% colsums_byname()`,
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param a The matrix whose elements are to be summed.
#'
#' @return The sum of all elements in `a` as a numeric.
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' sumall_byname(42)
#' m <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
#'   setrowtype("Industry") %>% setcoltype("Commodity")
#' m
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
#' sumall_byname(list(m, NULL))
sumall_byname <- function(a){
  sum_func <- function(a_mat){
    if (!(is.matrix(a_mat) | is.Matrix(a_mat) | is.numeric(a_mat))) {
      stop("Unknown type for 'a' in sumall_byname(). 'a' must be a matrix or numeric.")
    }
    a_mat %>%
      rowsums_byname() %>%
      colsums_byname() %>%
      as.numeric()
  }
  unaryapply_byname(sum_func, a = a, rowcoltypes = "none")
}


#' Row products, sorted by name
#'
#' Calculates row products (the product of all elements in a row) for a matrix.
#' An optional `colname` for the resulting column vector can be supplied.
#' If `colname` is `NULL` or `NA` (the default),
#' the column name is set to the column type as given by `coltype(a)`.
#'
#' @param a A matrix or list of matrices from which row products are desired.
#' @param colname The Name of the output column containing row products.
#'
#' @return A column vector of type `matrix` containing the row products of `a`
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
  rowprod_func <- function(a_mat, colname){
    if (is.na(colname)) {
      colname <- coltype(a_mat)
    }
    if (is.Matrix(a_mat)) {
      # There is no equivalents to apply() and prod() for Matrix objects.
      # So convert to a matrix object and then recursively call this function.
      out <- as.matrix(a_mat) %>% 
        rowprod_func(colname = colname) %>% 
        matsbyname::Matrix() %>% 
        setrowtype(rowtype(a_mat)) %>% setcoltype(coltype(a_mat))
      return(out)
    } else {
      # a_mat is probably a matrix
      out <- apply(a_mat, MARGIN = 1, FUN = prod) %>%
        # Preserve matrix structure (i.e., result will be a column vector of type matrix)
        matrix(byrow = TRUE)
    }
    out %>%
      # Preserve row names
      setrownames_byname(rownames(a_mat)) %>%
      # Set column name
      setcolnames_byname(colname) %>%
      # Set types
      setrowtype(rowtype(a_mat)) %>% setcoltype(coltype(a_mat)) %>% 
      # And sort the result on names
      sort_rows_cols()
  }
  unaryapply_byname(rowprod_func, a = a, .FUNdots = list(colname = colname), 
                    rowcoltypes = "none")
}


#' Column products, sorted by name
#'
#' Calculates column products (the product of all elements in a column) for a matrix.
#' An optional `rowname` for the resulting row vector can be supplied.
#' If `rowname` is `NULL` or `NA` (the default),
#' the row name is set to the row type as given by `rowtype(a)`.
#'
#' @param a A matrix or data frame from which column products are desired.
#' @param rowname The Name of the output row containing column products.
#'
#' @return a row vector of type `matrix` containing the column products of `a`.
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
#' This function is equivalent to `a \%>\% rowprods_byname() \%>\% colprods_byname()`,
#' but returns a single numeric value instead of a 1x1 matrix.
#'
#' @param a The matrix whose elements are to be multiplied.
#'
#' @return The product of all elements in `a` as a numeric.
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


#' Subtract a matrix with named rows and columns from a suitably named and sized identity matrix (`I`)
#'
#' The order of rows and columns of `m` may change before subtracting from `I`,
#' because the rows and columns are sorted by name prior to subtracting from `I`.
#' Furthermore, if `m` is not square, it will be made square
#' before subtracting from `I` by calling `complete_and_sort()`.
#'
#' @param a The matrix to be subtracted from `I`.
#'
#' @return The difference between an identity matrix (`I`) and `m`.
#'         (whose rows and columns have been completed and sorted)
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
  iminus_func <- function(a_mat){
    A <- complete_and_sort(a_mat) %>%
      setrowtype(rowtype(a_mat)) %>%
      setcoltype(coltype(a_mat))
    difference_byname(identize_byname(A), A)
  }
  unaryapply_byname(iminus_func, a = a, rowcoltypes = "all")
}


#' Cumulative sum that respects row and column names
#'
#' Provides cumulative sums along a list or column of a data frame.
#' If `a` is a single number, `a` is returned.
#' If `a` is a list of numbers, a list representing the cumulative sum of the numbers is returned.
#' If `a` is a single matrix, `a` is returned.
#' If `a` is a list of matrices, a list representing the cumulative sum
#' of the matrices is returned. 
#' In this case, each entry in the returned list is sum "by name," 
#' such that row and column names of the matrices are respected.
#' 
#' If cumulative sums are desired in the context of a data frame, 
#' groups in the data frame are respected if `mutate` is used.
#' See examples.
#'
#' @param a A number, list of numbers, matrix or list of matrices for which cumulative sum is desired.
#'
#' @return A single number, list of numbers, a single matrix, or a list of matrices,
#'         depending on the nature of `a`.
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
#' If `a` is a single number, `a` is returned.
#' If `a` is a list of numbers, a list representing the cumulative product of the numbers is returned.
#' If `a` is a single matrix, `a` is returned.
#' If `a` is a list of matrices, a list representing the cumulative product
#' of the matrices is returned. 
#' In this case, each entry in the returned list is product "by name," 
#' such that row and column names of the matrices are respected.
#' 
#' This function respects groups if `a` is a variable in a data frame.
#'
#' @param a A number, list of numbers, matrix or list of matrices for which cumulative element product is desired.
#'
#' @return A single number, list of numbers, a single matrix, or a list of matrices,
#'         depending on the nature of `a`.
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


#' Replace `NaN` values with a value
#'
#' In a matrix or within matrices in a list, 
#' replace all `NaN` matrix values with `val.`
#' 
#' @param a A matrix of list of matrices in which `NaN` will be replaced by `val`.
#' @param val `NaN`s are replace by `val.`
#'
#' @return A matrix or list of matrices in which all `NaN` are replaced by `val`.
#' 
#' @export
#'
#' @examples
#' suppressWarnings(a <- matrix(c(1, sqrt(-1))))
#' replaceNaN_byname(a)
#' replaceNaN_byname(a, 42)
replaceNaN_byname <- function(a, val = 0){
  replace_func <- function(a_mat){
    if (is.Matrix(a_mat)) {
      # Get the triplet
      trip <- Matrix::mat2triplet(a_mat) %>% 
        as.data.frame() %>% 
        # Find rows in the triple that are NaN
        # Note that "x" is the name of the value column produced by
        # Matrix::mat2triplet().
        # We can't change it!
        dplyr::filter(is.nan(.data[["x"]]))
      # Set those values to val
      if (nrow(trip) > 0) {
        # But only replace if there is anything to replace, 
        # because a sparseMatrix errors when there is nothing to replace.
        for (k in 1:nrow(trip)) {
          i <- trip[k, "i"]
          j <- trip[k, "j"]
          a_mat[i, j] <- val
        }
      }
    } else {
      # Probably have a regular matrix object.
      a_mat[is.nan(a_mat)] <- val
    }
    return(a_mat)
  }
  unaryapply_byname(replace_func, a = a, rowcoltypes = "all")
}


#' Count the number of matrix entries that meet a criterion
#' 
#' Expressions can be written in a natural way such as 
#' `count_vals_byname(m, "<=", 1)`.
#' 
#' Either a single matrix or a list of matrices can be given as the `a` argument.
#' `compare_fun` can be specified as a string ("!=")
#' or as a back-quoted function (\code{`!=`}).
#' 
#' @param a A matrix or list of matrices whose values are to be counted according to `compare_fun`.
#' @param compare_fun The comparison function, one of "==", "!=", 
#'                    "<", "<=", ">", or ">=". 
#'                    Default is "==".
#' @param val The value against which matrix entries are compared. 
#'            Default is `0`.
#'
#' @return An integer indicating the number of entries in `a` 
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


#' Are any matrix elements `TRUE`?
#' 
#' Tells whether any elements in matrix `a` are true.
#' 
#' `a` can be a matrix or a list of matrices.
#'
#' @param a a matrix or list of matrices
#'
#' @return `TRUE` if any elements of `a` are `TRUE`, `FALSE` otherwise
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


