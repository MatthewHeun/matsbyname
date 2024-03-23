#' Convert a matrix or list of matrices from named form to indexed form
#' 
#' Matrices can be in named form or in indexed form.
#' Named form is the usual representation for the `matsindf` package,
#' wherein names for rows and columns are included in the `dimnames`
#' attribute of the matrix object, consuming memory. 
#' In some instances, 
#' many sparse matrices with the same names will be created,
#' leading to inefficiencies. 
#' It would be more memory-efficient to store the matrices in 
#' triplet form (row index (i), column index (j), and value (x))
#' with the `Matrix` package and
#' maintain a separate (external) mapping between
#' row and column indices and row and column names.
#' Indexed form is the more memory-efficient version.
#' (In indexed form, it is the responsibility of the caller
#' to maintain a consistent mapping between row and column indices
#' and row and column names.)
#' This function converts from named form to indexed form
#' using externally supplied mappings (`index_map`).
#' 
#' `index_map` can be a single data frame or 
#' an unnamed list of two data frames or 
#' a named list of data frames.
#' If a single data frame, it must have two columns,
#' one containing exclusively integers
#' (interpreted as row and column indices) 
#' and 
#' the other containing exclusively character strings
#' (interpreted as row and column names).
#' If an unnamed list of two data frames, 
#' the first data frame is interpreted as the mapping
#' between row names and row indices
#' and
#' the second data frame is interpreted as the mapping 
#' between column names and column indices.
#' If a named list of data frames, 
#' names are interpreted as row and column types, 
#' with each named data frame applied as the mapping for the
#' associated row and column type. 
#' For example the data frame named "Industry" would be applied
#' to the dimension with an "Industry" type, regardless
#' of whether rows or columns were of type "Industry".
#'
#' @param a A matrix or list of matrices to be converted.
#' @param index_map A mapping between row and column names
#'                  and row and column indices.
#'
#' @return Indexed versions of the matrices in `a`.
#'
#' @name named_indexed 
#' 
#' @examples



#' @rdname named_indexed
#' @export
to_indexed <- function(a, index_map) {
  
}


to_named <- functino(a, index_map) {
  
}
