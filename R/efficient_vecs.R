#' Create a constant vector from matrix `a`
#' 
#' This function creates a vector using `a` as a template
#' and `k` as its value.
#' Row names are taken from the row names of `a`. 
#' The column name of the output is given by `colname`.
#' Row and column types are transferred from `a` to the output, directly.
#' 
#' If `column` is `TRUE`, the output is a column vector with 
#' row names taken from row names of `a` and a column named by `colname`.
#' If `column` is `FALSE`, the output is a row vevtor with 
#' column names taken from column names of `a` and a row named by `colname`.
#'
#' If the class of `a` is `Matrix`, the output object will be a `Matrix`.
#' Otherwise, the class of the output object will be a `matrix`.
#'
#' @param a The template matrix for the column vector.
#' @param k The value of the entries in the output column vector.
#' @param colname The name of the output vector's 1-sized dimension 
#'                (the only column if `column` is `TRUE`, the only row otherwise).
#' @param column Tells whether a column vector (if `TRUE`, the default) or a row vector (if `FALSE`) should be created.
#'
#' @return A vector vector formed from `a`.
#' 
#' @export
#'
#' @examples
#' kvec_from_template_byname(matrix(42, nrow = 4, ncol = 2,
#'                                  dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2"))), 
#'                           colname = "new column")
#' kvec_from_template_byname(matrix(42, nrow = 4, ncol = 2,
#'                                  dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2"))), 
#'                           colname = "new row", column = FALSE)
kvec_from_template_byname <- function(a, k = 1, colname = NA, column = TRUE) {
  
  k_from_template_func <- function(a_mat, k_val, colname_val, column_val) {
    # When we get here, a_mat should be a single matrix
    if (is.Matrix(a_mat)) {
      class_a_mat <- "Matrix"
    } else {
      class_a_mat <- "matrix"
    }
    if (column_val) {
      create_colvec_byname(rep(k_val, nrow(a_mat)), dimnames = list(rownames(a_mat), colname_val), matrix_class = class_a_mat)
    } else {
      create_rowvec_byname(rep(k_val, ncol(a_mat)), dimnames = list(colname_val, colnames(a_mat)), matrix_class = class_a_mat)
    }
  }
  
  unaryapply_byname(FUN = k_from_template_func, a = a, 
                    .FUNdots = list(k_val = k, colname_val = colname, column_val = column), rowcoltypes = "all")
}


#' Create a vector with labels from a matrix and values from a vector store
#' 
#' When a matrix is multiplied by a vector byname, 
#' naming can be tricky. 
#' There are times when pieces of the vector labels should be matched to 
#' pieces of the matrix labels. 
#' This function helps by performing the matching byname.
#' For this function, vector `v` is considered a store of values 
#' from which the output vector is created
#' using special matching rules between matrix `a` and vector `v`.
#' 
#' The output of this function is a vector
#' (a column vector if `column` is `TRUE`, the default; 
#' a row vector if `column` is `FALSE`).
#' The label of the size = 1 dimension is taken from `colname`
#' (so named, because the default is to return a column vector).
#' The labels of the long dimension are taken from matrix `a`
#' (the row names of `a` if `column` is `TRUE`; 
#' the column names of `a` if `column` is `FALSE`).
#' The values of the output vector are obtained from v
#' when `a_piece` matches `v_piece` using the `RCLabels` package.
#' The `v_piece`s of `v` must be unique.
#' The default values for `a_piece` and `v_piece` are "all", 
#' meaning that the entire label should be matched.
#' Other options for `a_piece` and `v_piece` are "pref" and "suff",
#' which will match the prefix or suffix of the labels.
#' Alternatively, prepositions can be given such that 
#' objects of prepositions will be matched.
#' Examples include "from" or "in".
#' Row and column types from `v` are applied to the output.
#' If the piece given in `a_piece` is not present in row or column names of `a`, 
#' `NA_real_` is returned.
#' If the piece given in `v_piece` is not present in row or column names of `v`, 
#' `NA_real_` is returned.
#' 
#' Note that `notation` and `prepositions` should be lists if `a` is a list
#' but a single value otherwise. 
#' The default values of `notation` and `prepositions` take care of this requirement,
#' switching on the type of `a` (list or not).
#' 
#' The class of the output object is determined from `a`.
#' If `a` is a `Matrix`, the output will be a `Matrix`.
#' Otherwise, the output will be a `matrix`.
#'
#' @param a A matrix from which row or column labels are taken.
#'          Can also be a list or the name of a column in a data frame.
#' @param v A vector from which values are taken, when `a_piece` matches `v_piece`.
#'          Can also be a list or the name of a column in a data frame.
#' @param a_piece The piece of labels on `a` that is to be matched. Default is "all".
#' @param v_piece The piece of labels on `v` that is to be matched. Default is "all".
#' @param colname The name of the output vector's 1-sized dimension 
#'                (the only column if `column` is `TRUE`, the only row otherwise).
#'                Default is `NULL`, meaning that the name of the 1-sized dimension in `v` 
#'                should be used.
#' @param margin Tells whether to assess the rows (`1`) or columns (`2`) of `a`
#'               when creating the outgoing vector.
#'               Default is `1`.
#' @param notation The notation for the row and column labels.
#'                 Default is `RCLabels::bracket_notation`, wrapped as a list if `a` is a list.
#' @param prepositions The strings that will count for prepositions.
#'                     Default is `RCLabels::prepositions`, wrapped as a list if `a` is a list.
#' @param missing The value used when the desired value is not found in `v`.
#'                Default is `NA_real_`.
#'
#' @return A column vector with names from `a` and values from `v`.
#' 
#' @export
#'
#' @examples
#' a <- matrix(42, nrow = 3, ncol = 5, 
#'             dimnames = list(c("Electricity [from b in c]", 
#'                               "Coal [from e in f]", 
#'                               "Crude oil [from Production in USA]"), 
#'                             c("Main activity producer electricity plants", 
#'                               "Wind turbines", 
#'                               "Oil refineries", 
#'                               "Coal mines", 
#'                               "Automobiles"))) %>%
#'   setrowtype("Product") %>% setcoltype("Industry")
#' a
#' v <- matrix(1:7, nrow = 7, ncol = 1, 
#'             dimnames = list(c("Electricity", 
#'                               "Peat", 
#'                               "Hydro", 
#'                               "Crude oil",
#'                               "Coal", 
#'                               "Hard coal (if no detail)", 
#'                               "Brown coal"), 
#'                             "phi")) %>%
#'   setrowtype("Product") %>% setcoltype("phi")
#' v
#' vec_from_store_byname(a, v, a_piece = "pref")
#' vec_from_store_byname(a, v, a_piece = "noun")
#' 
#' v2 <- matrix(1:7, nrow = 7, ncol = 1, 
#'              dimnames = list(c("Electricity", 
#'                                "Peat", 
#'                                "USA", 
#'                                "c",
#'                                "Coal", 
#'                                "Hard coal (if no detail)", 
#'                                "f"), 
#'                              "phi")) %>%
#'   setrowtype("Product") %>% setcoltype("phi")
#' vec_from_store_byname(a, v2, a_piece = "in")
#' 
#' # Works with lists
#' v3 <- matrix(1:7, nrow = 7, ncol = 1, 
#'              dimnames = list(c("Electricity [from USA]", 
#'                                "Peat [from nowhere]", 
#'                                "Production [from GHA]", 
#'                                "e [from ZAF]",
#'                                "Coal [from AUS]", 
#'                                "Hard coal (if no detail) [from GBR]", 
#'                                "b [from Nebraska]"), 
#'                              "phi")) %>%
#'   setrowtype("Product") %>% setcoltype("phi")
#' a_list <- list(a, a)
#' v_list <- list(v3, v3)
#' vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from")
#' 
#' # Also works in a data frame
#' df <- tibble::tibble(a = list(a, a, a), 
#'                      v = list(v3, v3, v3))
#' df %>%
#'   dplyr::mutate(
#'     actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from")
#'   )
vec_from_store_byname <- function(a, v, a_piece = "all", v_piece = "all", colname = NULL, 
                                  margin = 1,
                                  notation = if (is.list(a)) {list(RCLabels::bracket_notation)} else {RCLabels::bracket_notation}, 
                                  prepositions = if (is.list(a)) {list(RCLabels::prepositions_list)} else {RCLabels::prepositions_list}, 
                                  missing = NA_real_) {

  vec_func <- function(a_mat, v_vec, a_piece_val, v_piece_val, colname_val, margin_val, 
                       notation_val = notation, 
                       prepositions_val = prepositions) {
    # Get size of the v vector
    v_size <- dim(v_vec)
    # Make sure v is a matrix with 2 dimensions.
    assertthat::assert_that(length(v_size) == 2, 
                            msg = "v must be a matrix or a Matrix with 2 dimensions in vec_from_store_byname()")
    # Make sure one of the dimensions is size 1
    assertthat::assert_that(v_size[[1]] == 1 | v_size[[2]] == 1,
                            msg = "v must be a vector with one dimension of size 1 in vec_from_store_byname()")
    # Get the names that matter to us from the vector store
    if (v_size[[1]] == 1) {
      # Turn it into a column vector, so we can assume a column vector 
      # for the rest of this function.
      return(vec_func(a_mat, transpose_byname(v_vec), a_piece_val, v_piece_val, colname_val, margin_val))
    }
    # If we want to match on columns of a, transpose a_mat so that its columns become rows.
    if (margin_val == 2) {
      return(
        a_mat %>%
          transpose_byname() %>%
          vec_func(v_vec, a_piece_val, v_piece_val, colname_val, margin_val = 1)
      )
    }
    # At this point, we have a_mat and v_vec such that we want
    # names from the rows of a_mat and the values from the rows of v_vec.
    
    # Get row names from the matrix and the vector
    a_rownames <- dimnames(a_mat)[[1]]
    v_rownames <- dimnames(v_vec)[[1]]
    a_pieces <- RCLabels::get_piece(a_rownames, piece = a_piece, notation = notation_val, prepositions = prepositions_val)
    v_pieces <- RCLabels::get_piece(v_rownames, piece = v_piece, notation = notation_val, prepositions = prepositions_val)
    # Ensure that v_pieces are unique
    assertthat::assert_that(length(v_pieces) == length(unique(v_pieces)), 
                            msg = "v_pieces must be unique in vec_from_store_byname()")
    # Find the size of the outgoing column vector
    out_size <- dim(a_mat)[[1]]
    if (is.null(colname_val)) {
      # Pick up the column name from v_vec.
      colname_val <- dimnames(v_vec)[[2]]
    }
    # Build the outgoing vector with NA's everywhere
    if (is.Matrix(a_mat)) {
      out <- matsbyname::Matrix(missing, nrow = out_size, ncol = 1, 
                                dimnames = list(a_rownames, colname_val), 
                                rowtype = rowtype(v_vec), coltype = coltype(v_vec))
    } else {
      out <- matrix(missing, nrow = out_size, ncol = 1, 
                    dimnames = list(a_rownames, colname_val)) %>%
        # Be sure to retain the row and column type of v_vec.
        setrowtype(rowtype(v_vec)) %>% setcoltype(coltype(v_vec))
    }
    # Fill the vector
    for (i in 1:out_size) {
      # Get the value we want
      this_piece <- a_pieces[[i]]
      rownum_in_v <- which(v_pieces == this_piece, arr.ind = TRUE)
      
      # We need both this_piece to be something (not "") and
      # rownum_in_v to be different from 0 (i.e. present somewhere)
      # to assign something different from missing, the default value.
      if (this_piece != "" & length(rownum_in_v) != 0) {
        if (is.Matrix(v_vec)) {
          val <- v_vec[rownum_in_v, 1]
        } else {
          # Assume we have a matrix here.
          val <- v_vec[[rownum_in_v, 1]]
        }
        out[i, 1] <- val
      }
    }
    if (is.Matrix(a_mat)) {
      # Unfortunately, the "out[i, 1] <- val" assignment causes the loss 
      # of rowtype and coltype information on Matrix objects.
      # So reset here.
      out <- out %>% 
        setrowtype(rowtype(v_vec)) %>% setcoltype(coltype(v_vec))
    }        
    return(out)
  }
  
  binaryapply_byname(vec_func, a = a, b = v, .organize = FALSE, set_rowcoltypes = FALSE,
                     .FUNdots = list(a_piece_val = a_piece, v_piece_val = v_piece, 
                                     colname_val = colname, margin_val = margin, 
                                     notation_val = notation, 
                                     prepositions_val = prepositions))
}
