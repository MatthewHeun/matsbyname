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
to_indexed <- function(a, 
                       index_map, 
                       row_index_colname = "i", 
                       col_index_colname = "j", 
                       val_colname = "x", 
                       rownames_colname = "rownames", 
                       colnames_colname = "colnames") {
  orig_list <- TRUE
  if (is_matrix_or_Matrix(a)) {
    orig_list <- FALSE
    a <- list(a)
  }  
  out <- lapply(a, function(a_mat) {
    # At this point, we should have a single a_mat. 
    row_col_index_maps <- get_row_col_index_maps(a_mat, index_map)
    # Get the names of the row indices and names columns
    row_indices_colname <- names(row_col_index_maps[[1]])[[1]]
    row_names_colname <- names(row_col_index_maps[[1]])[[2]]
    # Get the names of the column indices and names columns
    col_indices_colname <- names(row_col_index_maps[[2]])[[1]]
    col_names_colname <- names(row_col_index_maps[[2]])[[2]]
    # Expand the matrix. To do so, leverage the Matrix package.
    # First get the dimnames.
    dnames <- dimnames(a_mat)
    assertthat::assert_that(!is.null(dnames))
    # Make index maps out of the original row and column indices
    orig_row_indices_map <- tibble::as_tibble(dnames[[1]]) |> 
      magrittr::set_names(rownames_colname) |> 
      tibble::rowid_to_column(var = row_index_colname)
    orig_col_indices_map <- tibble::as_tibble(dnames[[2]]) |> 
      magrittr::set_names(colnames_colname) |> 
      tibble::rowid_to_column(var = col_index_colname)
    a_mat |> 
      Matrix::Matrix(sparse = TRUE) |> 
      Matrix::mat2triplet() |> 
      tibble::as_tibble() |> 
      # Join with rownames and colnames from a_mat
      dplyr::left_join(orig_row_indices_map, by = row_index_colname) |> 
      # Eliminate the i column, because it is the original i.
      dplyr::mutate("{row_index_colname}" := NULL) |> 
      dplyr::left_join(orig_col_indices_map, by = col_index_colname) |> 
      # Eliminate the j column, because it is the original j.
      dplyr::mutate("{col_index_colname}" := NULL) |> 
      # Add the row indices, which are found in the first map
      dplyr::left_join(row_col_index_maps[[1]], 
                       by = dplyr::join_by({{rownames_colname}} == {{row_names_colname}})) |> 
      # dplyr::rename("{row_index_colname}" := .data[[row_indices_colname]]) |> 
      dplyr::rename("{row_index_colname}" := {{row_indices_colname}}) |> 
      dplyr::mutate("{rownames_colname}" := NULL) |> 
      # Add the column indices, which are found in the second map
      dplyr::left_join(row_col_index_maps[[2]], 
                       by = dplyr::join_by({{colnames_colname}} == {{col_names_colname}})) |> 
      # dplyr::rename("{col_index_colname}" := .data[[col_indices_colname]]) |> 
      dplyr::rename("{col_index_colname}" := {{col_indices_colname}}) |> 
      dplyr::mutate("{colnames_colname}" := NULL) |> 
      dplyr::relocate(dplyr::all_of(val_colname), .after = dplyr::everything())
  })
  if (!orig_list) {
    return(out[[1]])
  }
  return(out)
}


#' @rdname to_named_indexed
#' @export
to_named <- function(a, index_map) {
  
}


#' Figure out row and column index maps
#' 
#' The `index_map` argument can take several forms.
#' This function figures out (for a given `a_mat`)
#' the index maps for rows (first data frame in the return list)
#' and columns (second data frame in the return list).
#' 
#' This is a non-exported function meant only for internal use.
#'
#' @param a_mat A matrix for which index maps should be determined.
#' @param ind_map A data frame or list of data frames of potential
#'                index maps.
#'
#' @return A list of two data frames. 
#'         The first data frame is the index map for the rows of `a_mat`.
#'         The second data frame is the index map for the columns of `a_mat`.
get_row_col_index_maps <- function(a_mat, ind_map) {

  if (is.data.frame(ind_map)) {
    # This is the easiest case. 
    # Double check the structure of the data frame
    ind_map <- structure_index_map(ind_map)
    # Structure is good. Return a list with the same 
    # index map in the first and second slots.
    return(list(ind_map, ind_map))
  }
  if (is.list(ind_map) & is.null(names(ind_map)) & length(ind_map) == 2) {
    # In this case, ensure that the structure is correct for each 
    # index map and return a list of 2.
    return(list(structure_index_map(ind_map[[1]]), 
                structure_index_map(ind_map[[2]])))
  }
  if (is.list(ind_map) & !is.null(names(ind_map))) {
    # Check for rowtype and coltype.
    rtype <- rowtype(a_mat)
    assertthat::assert_that(!is.null(rtype))
    ctype <- coltype(a_mat)
    assertthat::assert_that(!is.null(ctype))
    row_indices <- ind_map[[rtype]]
    assertthat::assert_that(!is.null(row_indices))
    col_indices <- ind_map[[ctype]]
    assertthat::assert_that(!is.null(col_indices))
    return(list(structure_index_map(row_indices), 
                structure_index_map(col_indices)))
  }
  
  stop("Incorrectly formatted index_map in matsbyname::get_row_col_index_maps()")
}



#' Set the structure of an index map
#' 
#' Index maps must be a data frame with one integer column
#' and one character string column. 
#' This functino verifies the structure and ensures
#' that the integer column is first.
#' 
#' This is a non-exported function meant only for internal use.
#'
#' @param index_map The index map data frame to be structured.
#'
#' @return A data frame with an integer column as the first column 
#'         and a character string column as the second column.
structure_index_map <- function(index_map) {
  assertthat::assert_that(is.data.frame(index_map))  
  assertthat::assert_that(ncol(index_map) == 2)
  if (all(is.integer(index_map[[2]]))) {
    # Move to first location
    index_map <- index_map |> 
      dplyr::relocate(2)
  }
  # Now verify that structure is fine.
  assertthat::assert_that(all(is.integer(index_map[[1]])))
  assertthat::assert_that(all(is.character(index_map[[2]])))
  return(index_map)
}