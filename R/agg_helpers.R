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
    dplyr::select(.data[[few_colname]], .data[[many_colname]]) %>%
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
    tidyr::unnest(cols = .data[[many_colname]]) %>%
    dplyr::relocate(.data[[many_colname]], .after = .data[[few_colname]])
}
