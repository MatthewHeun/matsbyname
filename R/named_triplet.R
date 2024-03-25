#' Convert a matrix or list of matrices between named form and indexed form
#' 
#' Matrices can be in named form or indexed form.
#' Named form is the usual representation for the `matsindf` package,
#' wherein names for rows and columns are included in the `dimnames`
#' attribute of every matrix object, consuming memory. 
#' Typically, neither zero rows nor zero columns are present.
#' In some instances, 
#' many sparse matrices with the same names will be created,
#' leading to inefficiencies. 
#' It can be more memory-efficient to store the matrices in 
#' triplet form, 
#' (a table format with matrix data represented as 
#' a data frame with 
#' row integer (i), 
#' column integer (j), and 
#' value (x) columns.
#' In triplet form, 
#' a separate (external) mapping between
#' row and column indices and row and column names
#' must be maintained.
#' (In triplet form, it becomes the responsibility of the caller
#' to maintain a consistent mapping between row and column indices
#' and row and column names.
#' However, rowtype and coltype are retained as attributes 
#' of the triplet data frame.)
#' These functions convert from named form to triplet form
#' ([to_triplet()])
#' and vice versa ([to_named()])
#' using externally supplied mappings 
#' in the `index_map` argument.
#' [to_triplet()] and [to_named()] are exact inverses of each other.
#' 
#' `index_map` must be 
#' an unnamed list of two data frames or 
#' a named list of two or more data frames.
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
#' * If a named list of two or more data frames, 
#'   the names of `index_map`
#'   are interpreted as row and column types, 
#'   with named data frames applied as the mapping for the
#'   associated row or column type. 
#'   For example the data frame named "Industry" would be applied
#'   to the dimension (row or column)
#'   with an "Industry" type. 
#'   When both row and column have "Industry" type, 
#'   the "Industry" mapping is applied to both.
#'   When sending named data frames in `index_map`, 
#'   `a` must have both a row type and a column type.
#'   If an appropriate mapping cannot be found in `index_map`,
#'   an error is raised.
#'   Both matching data frames must have only
#'   an integer column and 
#'   a character column.
#' 
#' When converting to indexed form, 
#' rowtype and colytpe 
#' are retained as attributes. 
#' See [rowtype()] and [coltype()].
#'
#' If any indices are unavailable in the `index_map`, 
#' an error is raised.
#' It is an error to repeat a name in the name column of an `index_map` 
#' data frame.
#' It is an error to repeat an index in the index column
#' of an `index_map` data frame.
#'
#' @param a For [to_triplet()], a matrix or list of matrices to be converted to triplet form.
#'          For [to_named()], a data frame or list of data frames in triplet form to be converted to named form.
#' @param index_map A mapping between row and column names
#'                  and row and column indices.
#'                  See details.
#'
#' @return [to_triplet()] returns `a` as a data frame or list of data frames in triplet form.
#'         [to_named()] returns `a` as a matrix or a list of matrices in named form.
#'
#' @name to_named_triplet
#' 
#' @examples


#' @rdname to_named_triplet
#' @export
to_triplet <- function(a, 
                       index_map, 
                       row_index_colname = "i", 
                       col_index_colname = "j", 
                       val_colname = "x", 
                       rownames_colname = "rownames", 
                       colnames_colname = "colnames") {
  a_list <- TRUE
  if (is_matrix_or_Matrix(a)) {
    a_list <- FALSE
    a <- list(a)
  }  
  assertthat::assert_that(is.list(index_map) & !is.data.frame(index_map), 
                          msg = "index_map must be a list and not a data frame")
  assertthat::assert_that(length(index_map) >= 2, msg = "index_map must have length of 2 or more")
  out <- lapply(a, function(a_mat) {
    # At this point, we should have a single a_mat. 
    row_col_index_maps <- get_row_col_index_maps_for_named(a_mat, index_map)
    # Get the names of the row indices and names columns
    row_indices_colname <- names(row_col_index_maps[[1]])[[1]]
    row_names_colname <- names(row_col_index_maps[[1]])[[2]]
    # Get the names of the column indices and names columns
    col_indices_colname <- names(row_col_index_maps[[2]])[[1]]
    col_names_colname <- names(row_col_index_maps[[2]])[[2]]
    # Check for repeated values
    for (df in 1:2) {
      for (col in 1:2) {
        n_rows <- nrow(row_col_index_maps[[df]])
        assertthat::assert_that(length(unique(row_col_index_maps[[df]][[col]])) == n_rows,
                                msg = "All indices and names must be unique in to_triplet()")
      }
    }

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
    single_result <- a_mat |> 
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
      dplyr::relocate(dplyr::all_of(val_colname), .after = dplyr::everything()) |> 
      setrowtype(rowtype(a_mat)) |> 
      setcoltype(coltype(a_mat))
    # Verify that all indices have been set and no NA values exist
    assertthat::assert_that(!any(is.na(single_result[[row_index_colname]])))
    assertthat::assert_that(!any(is.na(single_result[[col_index_colname]])))
    return(single_result)
  })
  if (!a_list) {
    return(out[[1]])
  }
  return(out)
}


#' @rdname to_named_index
#' @export
to_named <- function(a, 
                     index_map, 
                     matrix_class = c("matrix", "Matrix"), 
                     row_index_colname = "i", 
                     col_index_colname = "j", 
                     val_colname = "x", 
                     rownames_colname = "rownames", 
                     colnames_colname = "colnames", 
                     rowtypes_colname = "rowtypes", 
                     coltypes_colname = "coltypes") {
  
  matrix_class <- match.arg(matrix_class)
  a_list <- TRUE
  if (is.data.frame(a)) {
    a_list <- FALSE
    a <- list(a)
  }

  out <- lapply(a, function(a_triplet) {
    # We should have one data frame here.
    # Figure out the index maps to use
    row_col_index_maps <- get_row_col_index_maps_for_named(a_triplet, index_map)
    # Ensure that correct columns are present
    assertthat::assert_that(row_index_colname %in% colnames(a_triplet), msg = paste0("'", row_index_colname, "' not found in column names of a_triplet"))
    assertthat::assert_that(col_index_colname %in% colnames(a_triplet), msg = paste0("'", col_index_colname, "' not found in column names of a_triplet"))
    assertthat::assert_that(val_colname %in% colnames(a_triplet), msg = paste0("'", val_colname, "' not found in column names of a_triplet"))
    # Make a Matrix object from the triplet
    out <- Matrix::sparseMatrix(i = a_triplet[[row_index_colname]], 
                         j = a_triplet[[col_index_colname]], 
                         x = a_triplet[[val_colname]], 
                         dims = c(nrow(row_col_index_maps[[1]]),
                                  nrow(row_col_index_maps[[2]])),
                         dimnames = list(row_col_index_maps[[1]][[2]], row_col_index_maps[[2]][[2]])) |> 
    clean_byname() |> 
    sort_rows_cols()
    # Convert to matrix, if needed
    if (matrix_class == "matrix") {
      out <- as.matrix(out)
    }
    # Add row and column types befor returning
    out |> 
      setrowtype(rowtype(a_triplet)) |>
      setcoltype(coltype(a_triplet))
  })

  if (!a_list) {
    return(out[[1]])
  }
  return(out)
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
get_row_col_index_maps_for_named <- function(a_mat, ind_map) {

  if (is.list(ind_map) & is.null(names(ind_map)) & length(ind_map) == 2) {
    # In this case, ensure that the structure is correct for each 
    # index map and return the list of 2.
    return(list(structure_index_map(ind_map[[1]]), 
                structure_index_map(ind_map[[2]])))
  }
  if (is.list(ind_map) & !is.null(names(ind_map))) {
    # Check for rowtype and coltype.
    rtype <- rowtype(a_mat)
    assertthat::assert_that(!is.null(rtype), 
                            msg = "matrix must have a row type")
    ctype <- coltype(a_mat)
    assertthat::assert_that(!is.null(ctype), 
                            msg = "matrix must have a column type")
    row_indices <- ind_map[[rtype]]
    assertthat::assert_that(!is.null(row_indices), 
                            msg = paste0("Suitable index map for row type '", rtype, "' not found."))
    col_indices <- ind_map[[ctype]]
    assertthat::assert_that(!is.null(col_indices), 
                            msg = paste0("Suitable index map for column type '", ctype, "' not found."))
    return(list(structure_index_map(row_indices), 
                structure_index_map(col_indices)))
  }
  
  stop("Incorrectly formatted index_map in matsbyname::get_row_col_index_maps()")
}


# get_row_col_index_maps_for_indexed <- function(a_indexed, ind_map, 
#                                                rowtypes_colname, coltypes_colname) {
#   if (is.list(a_indexed) & is.null(names(ind_map)) & length(ind_map) == 3) {
#     # We have an unnamed list.
#     # First item is assumed to contain row information.
#     # Second item is assumed to contain column information.
#     # Third item is assumed to contain row and column types.
#     # This case is easy. 
#     # Just return the items, after ensuring that the structure is correct.
#     return(list(structure_index_map(ind_map[[1]]), 
#                 structure_index_map(ind_map[[2]]), 
#                 structure_index_map(ind_map[[3]])))
#   }
#   if (is.list(a_indexed) & !is.null(names(ind_map)) & length(ind_map) >= 3) {
#     # Here, we have to figure out rowtype and coltype first.
#     # Then, we can put in the correct order.
#     
#     # Get the rowtype
#     rtype <- unique(a_indexed[[rowtypes_colname]])
#     assertthat::assert_that(length(rtype) == 1, msg = "Only one rowtype allowed")
#     # Get the coltype
#     ctype <- unique(a_indexed[[coltypes_colname]])
#     assertthat::assert_that(length(ctype) == 1, msg = "Only one coltype allowed")
#     
#   }
# }


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
