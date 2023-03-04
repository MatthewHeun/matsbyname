#' Aggregation map conversions
#'
#' @description
#' Aggregation is a many-to-few operation
#' where specifics are summed to comprise broader categories.
#' Examples include "John", "Paul", "George", and "Ringo" 
#' aggregated to "Beatles"; and
#' "Mick", "Keith", "Ronnie", "Bill", and "Charlie"
#' aggregated to "Stones".
#' An aggregation map is a named list that describes the aggregation 
#' to be performed.
#' An aggregation map for the examples above is 
#' `list(Beatles = c("John", "Paul", "George", "Ringo"), 
#'       Stones = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"))`
#' Aggregation maps can be generated from many shapes of data.
#' These functions assist with translating from different data shapes to 
#' aggregation maps.
#' 
#' @param .df A data frame from which an aggregation map is to be extracted.
#' @param few_colname The string name of a column in a data frame that corresponds 
#'                    to the "few" aggregated categories.
#' @param many_colname The string name of a column in a data frame that corresponds
#'                     to the "many" specific items that will be aggregated.
#' @param aggregation_map An aggregation map to be converted to a data frame.
#'
#' @return For `agg_table_to_agg_map()`, an aggregation map.
#'         For `agg_map_to_agg_table()`, a `data.frame`, probably at `tibble`.
#'
#' @examples
#' bands <- tibble::tribble(~band, ~members, 
#'                          "The Beatles", "John", 
#'                          "The Beatles", "Paul", 
#'                          "The Beatles", "George", 
#'                          "The Beatles", "Ringo", 
#'                          # Rejects duplicates and NA
#'                          "The Beatles", "Ringo",
#'                          "The Beatles", NA, 
#'                          "Rolling Stones", "Mick", 
#'                          "Rolling Stones", "Keith",
#'                          "Rolling Stones", "Ronnie",
#'                          "Rolling Stones", "Bill",
#'                          "Rolling Stones", "Charlie")
#' agg_map <- agg_table_to_agg_map(bands, 
#'                                  few_colname = "band",
#'                                  many_colname = "members")
#' agg_map
#' agg_map_to_agg_table(agg_map, few_colname = "bands", many_colname = "members")
#' @name aggregation_map_helpers
NULL


#' @export
#' @rdname aggregation_map_helpers
agg_table_to_agg_map <- function(.df, few_colname, many_colname) {
  out <- .df %>%
    # Select only the columns of interest
    dplyr::select(dplyr::all_of(c(few_colname, many_colname))) %>%
    # Get rid of NA cases.
    dplyr::filter(!is.na(.data[[few_colname]]) & !is.na(.data[[many_colname]])) %>%
    # Get rid of duplicates.
    unique() %>%
    # Create a nested tibble
    dplyr::nest_by(.data[[few_colname]], .key = many_colname) %>%
    dplyr::mutate(
      "{many_colname}" := .data[[many_colname]] %>% as.list() %>% unname()
    )
  out %>%
    magrittr::extract2(many_colname) %>%
    magrittr::set_names(out[[few_colname]])
}


#' @export
#' @rdname aggregation_map_helpers
agg_map_to_agg_table <- function(aggregation_map, few_colname, many_colname) {
  aggregation_map %>%
    unname() %>%
    tibble::tibble() %>%
    magrittr::set_names(many_colname) %>%
    dplyr::mutate(
      "{few_colname}" := names(aggregation_map)
    ) %>%
    tidyr::unnest(cols = dplyr::all_of(many_colname)) %>%
    dplyr::relocate(dplyr::all_of(many_colname), .after = dplyr::all_of(few_colname))
}


#' Aggregate rows and columns in a matrix
#' 
#' Rows (`margin = 1`), columns (`margin = 2`), or both (`margin = c(1, 2)`, the default)
#' are aggregated according to `aggregation_map`.
#' 
#' When `aggregation_map` is `NULL` (the default), 
#' rows (or columns or both) of same name are aggregated together. 
#' 
#' If `aggregation_map` is not `NULL`, it must be a named list.
#' The name of each `aggregation_map` item is the name of a row or column in output
#' that will contain the specified aggregation.
#' The value of each item in `aggregation_map` must be a vector of names of rows or columns in `a`.
#' The names in the value are aggregated and inserted into the output with the name of the value.
#' For example `aggregation_map = list(new_row = c("r1", "r2"))` 
#' will aggregate rows "r1" and "r2", delete rows "r1" and "r2", and insert a new row 
#' whose name is "new_row" and whose value is the sum of rows "r1" and "r2'.
#' 
#' The values in the `aggregation_map` are interpreted as regular expressions, and 
#' they are escaped using `Hmisc::escapeRegex()` prior to use.
#' 
#' `margin` can be a string, in which case it is interpreted as a row or column type.
#' If a string `margin` does not match a row or column type, 
#' `a` is returned unmodified.
#' 
#' Note that aggregation on one margin only will sort only the aggregated margin, because
#' the other margin is not guaranteed to have unique names.
#'
#' @param a A matrix or list of matrices whose rows or columns are to be aggregated.
#' @param aggregation_map A named list of rows or columns to be aggregated (or `NULL`). See `details`.
#' @param margin `1`, `2`, or `c(1, 2)` for row aggregation, column aggregation, or both.
#'               As a string, `margin` can be a row or column type. 
#'               Default is `c(1, 2)`.
#' @param pattern_type See `RCLabels::make_or_pattern()`.
#'                     Default is "exact".
#'
#' @return A version of `a` with aggregated rows and/or columns
#' 
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tibble)
#' m <- matrix(1:9, byrow = TRUE, nrow = 3, 
#'             dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1"))) %>% 
#'   setrowtype("rows") %>% setcoltype("cols")
#' # Aggregate all rows by establishing an aggregation map (`am`)
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
  
  agg_func <- function(a_mat, aggregation_map, this_margin, pattern_type) {
    # If we get here, a should be a single matrix.
    # Figure out the margin.
    this_margin <- margin_from_types_byname(a_mat, this_margin)
    if (length(this_margin) == 1) {
      if (is.na(this_margin)) {
        # Could not resolve the margin.
        # Return the matrix unmodified.
        return(a_mat)
      }
    }
    assertthat::assert_that(all(this_margin %in% c(1, 2)))
    # Create our own aggregation_map if it is NULL
    if (is.null(aggregation_map)) {
      rcnames <- list()
      if (1 %in% this_margin) {
        rcnames[["rnames"]] <- rownames(a_mat)
      }
      if (2 %in% this_margin) {
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
    if (2 %in% this_margin) {
      # Want to aggregate columns.
      # Easier to transpose, re-call ourselves to aggregate rows, and then transpose again.
      out <- t_matrix_or_Matrix(a_mat) %>% 
        agg_func(aggregation_map = aggregation_map, this_margin = 1, pattern_type = pattern_type) %>% 
        t_matrix_or_Matrix()
    }
    if (1 %in% this_margin) {
      for (i in 1:length(aggregation_map)) {
        # Isolate rows to be aggregated
        select_pattern <- RCLabels::make_or_pattern(strings = aggregation_map[[i]], pattern_type = pattern_type)
        rows_to_aggregate <- select_rows_byname(out, retain_pattern = select_pattern)
        if (!is.null(rows_to_aggregate)) {
          # Sum the isolated rows (if any)
          # aggregated_rows <- colsums_byname(rows_to_aggregate, rowname = names(aggregation_map[i]))
          aggregated_rows <- colSums_matrix_or_Matrix(rows_to_aggregate) 
          dimnames(aggregated_rows) <- list(c(names(aggregation_map[i])), c(colnames(rows_to_aggregate)))
          # %>% 
          # Sadly, colSums simplifies 1-dimensional output to a vector. 
          # So, remake the matrix.
          # matrix(nrow = 1, dimnames = list(c(names(aggregation_map[i])), c(colnames(rows_to_aggregate))))
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
              rbind_matrix_or_Matrix(aggregated_rows) 
          }
        }
      }
      # Note: Can't sort on columns, because they are not guaranteed to be unique.
      out <- sort_rows_cols(out, margin = 1)
    }
    return(out)
  }
  
  unaryapply_byname(agg_func, a, 
                    .FUNdots = list(aggregation_map = aggregation_map, this_margin = margin, pattern_type = pattern_type))
}


#' Aggregate a matrix to prefixes or suffixes of row and/or column names
#' 
#' `r lifecycle::badge("superseded")`
#' Row and column names are often constructed in the form 
#' `prefix_start` `prefix` `prefix_end` `suffix_start` `suffix` `suffix_end`
#' and described by a notation vector.
#' (See `notation_vec()`.)
#' This function performs aggregation by prefix or suffix according to a notation vector.
#' 
#' This function is a convenience function, as it bundles sequential calls to two helper functions,
#' `rename_to_pref_suff_byname()` and `aggregate_byname()`.
#' All arguments are passed to the helper functions.
#'
#' @param a A matrix of list of matrices to be aggregated by prefix or suffix.
#' @param aggregation_map See `aggregate_byname()`.
#' @param notation See `notation_vec()`. 
#' @param keep See `rename_to_pref_suff_byname()`
#' @param margin the dimension over which aggregation is to be performed; `1` for rows, `2` for columns, or `c(1, 2)` for both.
#' @param pattern_type See `aggregate_byname()`.
#'
#' @return An aggregated version of `a`.
#' 
#' @export
#'
#' @examples
#' # This function is superseded. 
#' # Instead, use `aggregate_pieces_byname()`.
#' # For example:
#' m <- matrix((1:9), byrow = TRUE, nrow = 3, 
#'             dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")))
#' m
#' aggregate_pieces_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#' aggregate_pieces_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
#' 
#' # Original examples:
#' # Aggregation by prefixes does nothing more than rename, because all prefixes are different.
#' # Doing renaming like this (without also aggregating) is potentially dangerous, because  
#' # some rows and some columns could end up with same names.
#' aggregate_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
#' # Aggregation by suffix reduces the number of rows and columns, 
#' # because there are same suffixes in both rows and columns
#' aggregate_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
aggregate_to_pref_suff_byname <- function(a, aggregation_map = NULL, 
                                          keep, margin = c(1, 2), notation,
                                          pattern_type = "exact") {
  a %>%
    aggregate_pieces_byname(piece = keep, margin = margin, notation = notation,
                            aggregation_map = aggregation_map,
                            pattern_type = pattern_type)
}


#' Aggregate a matrix by pieces of row and/or column names
#' 
#' Aggregate a matrix (or list of matrices or a column in a `matsindf` data frame)
#' by pieces of the row and column names.
#' 
#' This is a convenience function that bundles two others
#' for common use cases: 
#' `rename_to_piece_byname()` followed by `aggregate_byname()`.
#' Note that after renaming to the piece, 
#' there may be rows or columns that are identically named.
#' If those identically named names aren't included in the `aggregation_map`,
#' an error will result.
#' So, `aggregate_byname()` is called twice;
#' first with `aggregation_map = NULL` to sweep up any 
#' rows or columns that are identically named 
#' after renaming and 
#' second with `aggregation_map = aggregation_map` to 
#' sum the desired rows or columns.
#' See examples.
#' 
#' When `aggregation_map` is `NULL` (the default), 
#' rows (or columns or both) of same name are aggregated together. 
#' 
#' If `aggregation_map` is not `NULL`, it must be a named list.
#' The name of each `aggregation_map` item is the name of a row or column in output
#' that will contain the specified aggregation.
#' The value of each item in `aggregation_map` must be a vector of names of rows or columns in `a`.
#' The names in the value are aggregated and inserted into the output with the name of the value.
#' For example `aggregation_map = list(new_row = c("r1", "r2"))` 
#' will aggregate rows "r1" and "r2", delete rows "r1" and "r2", and insert a new row 
#' whose name is "new_row" and whose value is the sum of rows "r1" and "r2'.
#' 
#' The values in the `aggregation_map` are interpreted as regular expressions, and 
#' they are escaped using `Hmisc::escapeRegex()` prior to use.
#' 
#' `aggregation_map` should aggregate by pieces, 
#' not by the full, original row and/or column names.
#' 
#' @param a A matrix or list of matrices.
#' @param piece A character string indicating which piece of the row or column names to retain, 
#'              one of "noun", "pps", "pref" or "suff", or a preposition,
#'              indicating which part of the row or column name is to be retained.
#' @param margin As a character, the row type or column type to be renamed.
#'               As an integer, the margin to be renamed.
#'               Default is `c(1, 2)`, meaning that both 
#'               rows (`margin = 1`) and columns (`margin = 2`)
#'               will be renamed.
#' @param inf_notation A boolean that tells whether to infer notation.
#'                     Default is `TRUE`.
#' @param notation The notation used for row and column labels. 
#'                 Default is `list(RCLabels::notations_list)`.
#'                 The default value is wrapped in a list, 
#'                 because `RCLabels::notations_list` is, itself, a list.
#'                 See `RCLabels`.
#' @param choose_most_specific A boolean that indicates whether the most-specific notation
#'                             will be inferred when more than one of `notation` matches 
#'                             a row or column label
#'                             and `allow_multiple = FALSE`.
#'                             When `FALSE`, the first matching notation in `notations`
#'                             is returned when `allow_multiple = FALSE`.
#'                             Default is `FALSE`.
#' @param prepositions Prepositions that can be used in the row and column label.
#'                     Default is `RCLabels::prepositions_list`.
#' @param aggregation_map A named list of rows or columns to be aggregated (or `NULL`). See `details`.
#' @param pattern_type See `RCLabels::make_or_pattern()`.
#'                     Default is "exact".
#'
#' @return A version of `a` with rows and/or columns aggregated according to `aggregation_map`.
#' 
#' @export
#'
#' @examples
#' a <- matrix(c(1, 2, 3, 
#'               4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("a [from b]", "c [from d]"), 
#'                             c("e [from f]", "g [from h]", "i [from j]")))
#' a %>%
#'   aggregate_pieces_byname(piece = "suff", 
#'                           notation = RCLabels::from_notation,
#'                           aggregation_map = list(rows = c("b", "d"), 
#'                                                  cols = c("h", "j")))
#' m <- matrix(c(1, 0, 0, 
#'               0, 1, 1, 
#'               0, 1, 1), nrow = 3, ncol = 3, byrow = TRUE, 
#'             dimnames = list(c("Gasoline [from Oil refineries]", 
#'                               "Electricity [from Main activity producer electricity plants]", 
#'                               "Electricity [from Hydro]"),
#'                             c("Automobiles", "LED lamps", "CFL lamps"))) %>%
#'   setrowtype("Product") %>% setcoltype("Industry")
#' mT <- transpose_byname(m)
#' # Aggregate the "Electricity" rows.
#' aggregate_pieces_byname(m, piece = "noun", margin = "Product",
#'                         notation = RCLabels::bracket_notation)
#' # Also works in a list.
#' aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
#'                         margin = "Product",
#'                         notation = RCLabels::bracket_notation)
#' # Use an aggregation map
#' aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
#'                         margin = "Product",
#'                         aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
#'                         notation = RCLabels::bracket_notation)
#' # Also works in a data frame.
#' df <- tibble::tibble(m = list(m, mT), 
#'                      pce = "noun",
#'                      mgn = "Product",
#'                      agg_map = list(list(final = c("Electricity", "Gasoline"))), 
#'                      notn = list(RCLabels::bracket_notation)) %>%
#'   dplyr::mutate(
#'     agg = aggregate_pieces_byname(a = m, piece = pce, margin = mgn, 
#'                                   aggregation_map = agg_map,
#'                                   notation = notn)
#'   )
#' df$agg
#' # Works when renaming to the piece results in identical row or col names.
#' b <- matrix(1:6, nrow = 3, ncol = 2, 
#'             dimnames = list(c("a [from b]", "c [from d]", "c [from e]"), 
#'                             c("c1", "c2")))
#' b
#' # This aggregation works, because the "c" rows
#' # are aggregated before applying the aggregation_map,
#' # which, itself, does NOT aggregate the "c" rows.
#' b %>% 
#'   aggregate_pieces_byname(piece = "noun",
#'                           margin = 1,
#'                           inf_notation = FALSE, 
#'                           notation = RCLabels::bracket_notation, 
#'                           aggregation_map = list(f = c("a", "b")))
aggregate_pieces_byname <- function(a, 
                                    piece,
                                    margin = list(c(1, 2)), 
                                    inf_notation = TRUE,
                                    notation = list(RCLabels::notations_list),
                                    choose_most_specific = FALSE,
                                    prepositions = list(RCLabels::prepositions_list), 
                                    aggregation_map = NULL, 
                                    pattern_type = "exact") {
  a %>%
    rename_to_piece_byname(piece = piece, 
                           margin = margin, 
                           inf_notation = inf_notation,
                           notation = notation,
                           choose_most_specific = choose_most_specific,
                           prepositions = prepositions) %>%
    # Aggregate with a NULL aggregation map to sweep up 
    # any rows or columns that are identically named
    # (and may not be in the aggregation_map).
    aggregate_byname(aggregation_map = NULL, margin = margin, 
                     pattern_type = pattern_type) %>% 
    # Now aggregate using the aggregation_map to sum 
    # rows and columns according to the map.
    aggregate_byname(aggregation_map = aggregation_map, margin = margin, 
                     pattern_type = pattern_type)
}



