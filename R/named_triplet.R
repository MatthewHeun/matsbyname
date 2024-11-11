#' Convert a matrix or list of matrices between named form and indexed form
#' 
#' Matrices can be in named matrix form or triplet form.
#' Named matrix form is the usual representation for the `matsindf` package,
#' wherein names for rows and columns are included in the `dimnames`
#' attribute of every matrix or Matrix object, consuming memory. 
#' Typically, neither zero rows nor zero columns are present.
#' In some instances, 
#' many sparse matrices with the same names will be created,
#' leading to inefficiencies due to dimname storage with every matrix object. 
#' It can be more memory-efficient to store named matrices in 
#' integer triplet form, 
#' (a table format with matrix data represented as 
#' a data frame with 
#' row integer (i), 
#' column integer (j), and 
#' value (value) columns.
#' (Row names and column names can be stored as character string
#' in the `i` and `j` columns, too, 
#' called character triplet form.)
#' Integer triplet form is required for databases that
#' do not recognize a matrix as a native storage format.
#' In integer triplet form, 
#' a separate (external) mapping between
#' row and column indices and row and column names
#' must be maintained.
#' (In integer triplet form, it becomes the responsibility of the caller
#' to maintain a consistent mapping between row and column indices
#' and row and column names.
#' However, rowtype and coltype are retained as attributes 
#' of both integer and character triplet data frames.)
#' These functions convert from named matrix form to integer triplet form
#' ([to_triplet()])
#' and vice versa ([to_named_matrix()])
#' using row and column name mappings 
#' supplied in the `index_map` argument.
#' [to_triplet()] and [to_named_matrix()] are inverses of each other, 
#' with row and column order not necessarily preserved.
#' See examples.
#' 
#' `index_map` must be one of the following:
#' * A single data frame of two columns, 
#'   with one an integer column and the other a character column.
#'   When a single data frame, 
#'   it will be applied to both rows and columns.
#' * An unnamed list of exactly two data frames, 
#'   each data frame must have only 
#'   an integer column and a character column.
#'   The first data frame of `index_map` 
#'   is interpreted as the mapping
#'   between row names and row indices
#'   and
#'   the second data frame of `index_map`
#'   is interpreted as the mapping 
#'   between column names and column indices.
#' * A named list of two or more data frames, 
#'   in which the names of `index_map`
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
#' rowtype and coltype 
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
#' If `a` is `NULL`, `NULL` is returned.
#' If `a` is a list and any member of the list is `NULL`, 
#' `NULL` is returned in that position.
#' 
#' By default, [to_triplet()] will return
#' a zero-row data frame when 
#' `a` is a zero matrix.
#' Set `retain_zero_structure = TRUE` 
#' to return all entries in the zero matrix.
#'
#' @param a For [to_triplet()], a matrix or list of matrices to be converted to triplet form.
#'          For [to_named_matrix()], a data frame or list of data frames in triplet form to be converted to named matrix form.
#' @param retain_zero_structure A boolean that tells whether to retain 
#'                              the structure of zero matrices when creating triplets.
#'                              Default is `FALSE`. 
#'                              See details.
#' @param index_map A mapping between row and column names
#'                  and row and column indices.
#'                  See details.
#' @param matrix_class One of "matrix" (standard) or "Matrix" (sparse) representation
#'                     for matrices. 
#'                     Default is "matrix".
#' @param row_index_colname,col_index_colname The names of row and column index columns in data frames.
#'                                            Defaults are "i" and "j", respectively.
#' @param value_colname The name of the value column in data frames. 
#'                      Default is "value".
#' @param rownames_colname,colnames_colname The name of row name and column name columns in data frames.
#'                                          Defaults are "rownames" and "colnames", respectively.
#' @param .rnames,.cnames Column names used internally. 
#'                        Defaults are "rownames" and "colnames".
#'
#' @return [to_triplet()] returns `a` as a data frame or list of data frames in triplet form.
#'         [to_named_matrix()] returns `a` as a named matrix or a list of matrices in named form.
#'
#' @name to_named_triplet
#' 
#' @examples
#' triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
#'                       j = as.integer(c(3, 3, 3, 4, 4, 4)), 
#'                       value = c(1, 2, 3, 4, 5, 6)) |> 
#'   setrowtype("rows") |> setcoltype("cols")
#' triplet
#' rowtype(triplet)
#' coltype(triplet)
#' # We have more indices than actual entries in the martix
#' r_indices <- data.frame(names = paste0("r", 1:101),
#'                         indices = 1:101)
#' head(r_indices)
#' c_indices <- data.frame(names = paste0("c", 1:101),
#'                         indices = 1:101)
#' head(c_indices)
#' # Names are interpreted as row and column types
#' indices <- list(cols = c_indices, rows = r_indices)
#' named <- to_named_matrix(triplet, indices)
#' named
#' triplet2 <- to_triplet(named, indices)
#'
#' # Although not in the same row order, 
#' # triplet and triplet2 are the same.
#' triplet2
#' rowtype(triplet2)
#' coltype(triplet2)
#' # And the same matrix can be recovered from triplet2
#' to_named_matrix(triplet2, indices)


#' @rdname to_named_triplet
#' @export
to_triplet <- function(a, 
                       index_map, 
                       retain_zero_structure = FALSE,
                       row_index_colname = "i", 
                       col_index_colname = "j", 
                       value_colname = "value", 
                       rownames_colname = "rownames", 
                       colnames_colname = "colnames") {
  a_list <- TRUE
  if (is_matrix_or_Matrix(a) | is.null(a)) {
    a_list <- FALSE
    a <- list(a)
  }  
  assertthat::assert_that(is.list(index_map) | is.data.frame(index_map), 
                          msg = "index_map must be a list or a single data frame")
  if (is.list(index_map) & !is.data.frame(index_map)) {
    assertthat::assert_that(length(index_map) >= 2, msg = "index_map must have length of 2 or more when it's a list and not a data frame")
  }
  out <- lapply(a, function(a_mat) {
    if (is.null(a_mat)) {
      return(NULL)
    }
    # At this point, we should have a single a_mat. 
    row_col_index_maps <- get_row_col_index_maps(a_mat, index_map)
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
    rows_matched <- a_mat |> 
      # Matrix::Matrix(sparse = TRUE) |> 
      # Matrix::mat2triplet() |> 
      # tibble::as_tibble() |> 
      create_triplet(retain_zero_structure = retain_zero_structure, 
                     i_col = row_index_colname, 
                     j_col = col_index_colname, 
                     value_col = value_colname) |> 
      # Join with rownames from a_mat
      dplyr::left_join(orig_row_indices_map, by = row_index_colname) |> 
      # Eliminate the i column, because it is the original i.
      dplyr::mutate("{row_index_colname}" := NULL) |> 
      # Add the row indices, which are found in the first map
      dplyr::left_join(row_col_index_maps[[1]], 
                       by = dplyr::join_by({{rownames_colname}} == {{row_names_colname}}))
    # Check for any errors before going on
    if (any(is.na(rows_matched[[row_indices_colname]]))) {
      # Create a helpful error message
      unmatched_rownames <- rows_matched |> 
        dplyr::filter(is.na(.data[[row_indices_colname]])) |> 
        magrittr::extract2(rownames_colname) |> 
        unique()
      err_msg <- paste0("Unmatched row names in to_triplet():\n",
                        paste0(unmatched_rownames, collapse = "\n"), 
                        "\nDo you have a typo? Or should names be added to index_map?")
      stop(err_msg)
    }
    # Complete the work on rows
    rows_matched_completed <- rows_matched |> 
      dplyr::rename("{row_index_colname}" := {{row_indices_colname}}) |> 
      dplyr::mutate("{rownames_colname}" := NULL)
    
    cols_matched <- rows_matched_completed |> 
      # Join with colnames from a_mat
      dplyr::left_join(orig_col_indices_map, by = col_index_colname) |> 
      # Eliminate the j column, because it is the original j.
      dplyr::mutate("{col_index_colname}" := NULL) |> 
      # Add the column indices, which are found in the second map
      dplyr::left_join(row_col_index_maps[[2]], 
                       by = dplyr::join_by({{colnames_colname}} == {{col_names_colname}}))
    # Check for any errors before going on
    if (any(is.na(cols_matched[[col_indices_colname]]))) {
      # Create a helpful error message
      unmatched_colnames <- cols_matched |> 
        dplyr::filter(is.na(.data[[col_indices_colname]])) |> 
        magrittr::extract2(colnames_colname) |> 
        unique()
      err_msg <- paste0("Unmatched column names in to_triplet():\n",
                        paste0(unmatched_colnames, collapse = "\n"), 
                        "\nDo you have a typo? Or should names be added to index_map?")
      stop(err_msg)
    }
    # Complete the work on cols
    cols_matched_completed <- cols_matched |> 
      dplyr::rename("{col_index_colname}" := {{col_indices_colname}}) |> 
      dplyr::mutate("{colnames_colname}" := NULL)
    
    # Finish everything off
    cols_matched_completed |> 
      dplyr::relocate(dplyr::all_of(value_colname), .after = dplyr::everything()) |> 
      setrowtype(rowtype(a_mat)) |> 
      setcoltype(coltype(a_mat))
  })
  if (!a_list) {
    return(out[[1]])
  }
  return(out)
}


#' @rdname to_named_triplet
#' @export
to_named_matrix <- function(a, 
                            index_map, 
                            matrix_class = c("matrix", "Matrix"), 
                            row_index_colname = "i", 
                            col_index_colname = "j", 
                            value_colname = "value", 
                            .rnames = "rownames", 
                            .cnames = "colnames") {
  
  matrix_class <- match.arg(matrix_class)
  a_list <- TRUE
  if (is.data.frame(a)) {
    a_list <- FALSE
    a <- list(a)
  }

  out <- lapply(a, function(a_triplet) {
    # a_triplet should be a single data frame here.
    # Ensure that correct columns are present
    assertthat::assert_that(row_index_colname %in% colnames(a_triplet), msg = paste0("'", row_index_colname, "' not found in column names of a_triplet"))
    assertthat::assert_that(col_index_colname %in% colnames(a_triplet), msg = paste0("'", col_index_colname, "' not found in column names of a_triplet"))
    assertthat::assert_that(value_colname %in% colnames(a_triplet), msg = paste0("'", value_colname, "' not found in column names of a_triplet"))
    if (all(is.integer(a_triplet[[row_index_colname]])) & 
        all(is.integer(a_triplet[[col_index_colname]]))) {
      # Figure out the index maps to use
      row_col_index_maps <- get_row_col_index_maps(a_triplet, index_map)
      # Make a Matrix object from the triplet
      out <- Matrix::sparseMatrix(i = a_triplet[[row_index_colname]], 
                                  j = a_triplet[[col_index_colname]], 
                                  x = a_triplet[[value_colname]], 
                                  dims = c(nrow(row_col_index_maps[[1]]),
                                           nrow(row_col_index_maps[[2]])),
                                  dimnames = list(row_col_index_maps[[1]][[2]], row_col_index_maps[[2]][[2]]))
    } else if (all(is.character(a_triplet[[row_index_colname]])) & 
               all(is.character(a_triplet[[col_index_colname]]))) {
      # All integers have already been converted to names
      # Set up indices
      rownames <- a_triplet[[row_index_colname]] |> 
        unique()
      rowname_df <- data.frame(1:length(rownames), 
                               rownames) |> 
        magrittr::set_colnames(c(row_index_colname, .rnames))
      colnames <- a_triplet[[col_index_colname]] |> 
        unique()
      colname_df <- data.frame(1:length(colnames), 
                               colnames) |> 
        magrittr::set_colnames(c(col_index_colname, .cnames))
      # Join to get the correct row and column indices
      integer_df <- a_triplet |> 
        dplyr::rename(
          "{.rnames}" := dplyr::all_of(row_index_colname), 
          "{.cnames}" := dplyr::all_of(col_index_colname)
        ) |> 
        dplyr::left_join(rowname_df, by = .rnames) |> 
        dplyr::left_join(colname_df, by = .cnames) |> 
        dplyr::mutate(
          "{.rnames}" := NULL, 
          "{.cnames}" := NULL
        )
      out <- Matrix::sparseMatrix(i = integer_df[[row_index_colname]], 
                                  j = integer_df[[col_index_colname]], 
                                  x = integer_df[[value_colname]], 
                                  dims = c(nrow(rowname_df),
                                           nrow(colname_df)),
                                  dimnames = list(rowname_df[[.rnames]], colname_df[[.cnames]])) 
    } else {
      stop("`row_index_colname` and `col_index_colname` must both be all integer or all character in to_named_matrix()")
    }
    
    # If this is NOT a zero or NA matrix, 
    # get rid of zero rows and columns.
    # We don't want to clean if it is a zero or NA matrix, 
    # because information will be lost.
    iszero_out <- iszero_byname(out)
    if (!is.na(iszero_out) & !iszero_out) {
      out <- out |> 
        clean_byname()
    }
    # Sort rows and columns as a courtesy to callers 
    out <- out |> 
      sort_rows_cols()

    # Convert to matrix class, if needed
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
#' `ind_maps` can be a single data frame, 
#' in which case the single data frame will be applied
#' to both rows and columns of `a_mat`.
#' 
#' `ind_maps` can also be a 2-item list, 
#' in which case the first item is applied to rows and the 
#' second item is applied to columns.
#' 
#' Finally, `ind_maps` can be a named list of length 2 or more,
#' in which case the names are interpreted as row or column types.
#' Names in the `ind_maps` list are matched to row and column types
#' and applied as required.
#' 
#' This is a non-exported function meant only for internal use.
#'
#' @param a_mat A matrix for which index maps should be determined.
#' @param ind_maps A single data frame or a list of two or more data frames
#'                 of potential index maps.
#'
#' @return A list of two data frames. 
#'         The first data frame is the index map for the rows of `a_mat`.
#'         The second data frame is the index map for the columns of `a_mat`.
get_row_col_index_maps <- function(a_mat, ind_maps) {

  if (is.data.frame(ind_maps)) {
    # We have only one data frame for the index map.
    # Ensure that the structure is correct and return two copies
    # of the index map, one for each margin of the matrix.
    ind_maps <- structure_index_map(ind_maps)
    return(list(ind_maps, ind_maps))
  }
  
  if (is.list(ind_maps) & is.null(names(ind_maps)) & length(ind_maps) == 2) {
    # In this case, ensure that the structure is correct for each 
    # index map and return the list of 2.
    return(list(structure_index_map(ind_maps[[1]]), 
                structure_index_map(ind_maps[[2]])))
  }
  
  if (is.list(ind_maps) & !is.null(names(ind_maps))) {
    # For this case, match names of the items in ind_maps
    # with row and column types.
    # Check for rowtype and coltype.
    rtype <- rowtype(a_mat)
    assertthat::assert_that(!is.null(rtype), 
                            msg = "matrix must have a row type")
    ctype <- coltype(a_mat)
    assertthat::assert_that(!is.null(ctype), 
                            msg = "matrix must have a column type")
    row_indices <- ind_maps[[rtype]]
    assertthat::assert_that(!is.null(row_indices), 
                            msg = paste0("Suitable index map for row type '", rtype, "' not found."))
    col_indices <- ind_maps[[ctype]]
    assertthat::assert_that(!is.null(col_indices), 
                            msg = paste0("Suitable index map for column type '", ctype, "' not found."))
    return(list(structure_index_map(row_indices), 
                structure_index_map(col_indices)))
  }
  
  stop("Incorrectly formatted index_map in matsbyname::get_row_col_index_maps()")
}


#' Set the structure of an index map
#' 
#' Index maps must be a data frame with one integer column
#' and one character string column. 
#' This function verifies the structure and ensures
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


#' Create a triplet from a matrix
#' 
#' Creates a data frame triplet with columns
#' `i_col`, `j_col`, and `value_col` from matrix `m`.
#' Zero entries are not reported.
#' `i` and `j` integers are directly from `m`
#' and not referenced to any global set of `i` and `j` values.
#' 
#' When `m` is a zero matrix, 
#' a zero-row data frame is returned by default
#' (`retain_zero_structure = FALSE`).
#' But when `retain_zero_structure` is `TRUE`, 
#' zero entries are reported for all rows and columns,
#' thereby preserving the structure of the matrix.
#'
#' @param m A `matrix` or `Matrix` to be converted to triplet form.
#' @param retain_zero_structure A boolean that tells whether
#'                              to retain the structure of zero matrices.
#'                              Default is `FALSE`.
#' @param i_col,j_col,value_col String names of i, j, and x columns.
#'
#' @return A `tibble` triplet representation of `m`.
create_triplet <- function(m, 
                           retain_zero_structure = FALSE, 
                           i_col = "i", j_col = "j", value_col = "value") {
  out <- m |> 
    Matrix::Matrix(sparse = TRUE) |> 
    Matrix::mat2triplet() |> 
    tibble::as_tibble() |> 
    dplyr::rename(
      # Matrix::mat2triplet() gives i, j, x columns
      # with no option to rename.
      # So we rename here.
      "{i_col}" := dplyr::all_of("i"), 
      "{j_col}" := dplyr::all_of("j"),
      "{value_col}" := dplyr::all_of("x")
    )
  if (retain_zero_structure & nrow(out) == 0) {
    # Create an outgoing data frame that includes all the 0 values
    out <- expand.grid(1:dim(m)[[1]], 1:dim(m)[[2]], KEEP.OUT.ATTRS = FALSE) |> 
      tibble::as_tibble() |> 
      magrittr::set_names(c(i_col, j_col)) |> 
      dplyr::mutate(
        "{value_col}" := 0
      )
  }
  return(out)
}
