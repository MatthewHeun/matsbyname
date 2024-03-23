#' Convert a matrix or list of matrices from named form to indexed form
#' 
#' Matrices can be in named form or indexed form.
#' Named form is the usual representation for the `matsindf` package,
#' wherein names for rows and columns are included in the `dimnames`
#' attribute of the matrix object, consuming memory. 
#' Typically, neither zero rows nor zero columns are present.
#' In some instances, 
#' many sparse matrices with the same names will be created,
#' leading to inefficiencies. 
#' It would be more memory-efficient to store the matrices in 
#' indexed form, 
#' (a sparse format with matrix data represented as a 
#' row integer (i), column integer (j), and value (x) triplet, 
#' made possible by the `Matrix` package) and
#' maintain a separate (external) mapping between
#' row and column indices and row and column names.
#' (In indexed form, it becomes the responsibility of the caller
#' to maintain a consistent mapping between row and column indices
#' and row and column names.)
#' These functions convert from named form to indexed form
#' ([to_indexed()])
#' and vice versa ([to_named()])
#' using externally supplied mappings 
#' in the `index_map` argument.
#' 
#' `index_map` can be a single data frame,
#' an unnamed list of two data frames, or 
#' a named list of one or more data frames.
#' * If a single data frame, 
#'   `index_map` must have two columns,
#'   one containing exclusively integers
#'   (interpreted as row and column indices) 
#'   and 
#'   the other containing exclusively character strings
#'   (interpreted as row and column names).
#' * If an unnamed list of two data frames, 
#'   each data frame must have only 
#'   an integer column and a character column.
#'   The first data frame of `index_map` 
#'   is interpreted as the mapping
#'   between row names and row indices
#'   and
#'   the second data frame of `index_map`
#'   is interpreted as the mapping 
#'   between column names and column indices.
#' * If a named list of data frames, 
#'   all data frames must have only an integer column and 
#'   a character column.
#'   The names of `index_map`
#'   are interpreted as row and column types, 
#'   with each named data frame applied as the mapping for the
#'   associated row or column type. 
#'   For example the data frame named "Industry" would be applied
#'   to the dimension with an "Industry" type, regardless
#'   of whether rows or columns (could be both)
#'   were of type "Industry".
#'   When sending named data frames in `index_map`, 
#'   `a` must have both a row type and a column type.
#'   If an appropriate mapping cannot be found in `index_map`,
#'   an error is raised.
#'
#' It is an error to repeat a name in the name column of an `index_map`.
#' It is an error to repeat an index in the index column
#' of an `index_map`.
#'
#' @param a A matrix or list of matrices to be converted.
#' @param index_map A mapping between row and column names
#'                  and row and column indices.
#'                  See details.
#'
#' @return [to_indexed()] returns `a` in indexed form.
#'         [to_named()] returns `a` in named form. 
#'
#' @name to_named_indexed 
#' 
#' @examples



#' @rdname to_named_indexed
#' @export
to_indexed <- function(a, index_map) {
  
}


#' @rdname to_named_indexed
#' @export
to_named <- function(a, index_map) {
  
}
