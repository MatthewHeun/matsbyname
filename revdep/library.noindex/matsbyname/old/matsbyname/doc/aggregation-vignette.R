## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(dplyr)
library(tibble)
library(matsbyname)
library(matsindf)

## -----------------------------------------------------------------------------
m <- matrix(c(1, 2, 3, 4, 
              5, 6, 7, 8, 
              9, 10, 11, 12), nrow = 3, ncol = 4, byrow = TRUE,
            dimnames = list(c("duck", "duck", "goose"), 
                            c("John", "Paul", "George", "Ringo")))
m
aggregate_byname(m)

## -----------------------------------------------------------------------------
m
aggregate_byname(m, aggregation_map = list(birds = c("duck", "goose"), 
                                           guitarists = c("John", "Paul", "George")))

## -----------------------------------------------------------------------------
m
aggregate_byname(m, aggregation_map = list(Beatles = c("John", "Paul", "George", "Ringo")), 
                 margin = 2)

## -----------------------------------------------------------------------------
m
aggregate_byname(m, aggregation_map = list(guitarists = "^[JPG]"), 
                 margin = 2, pattern_type = "literal")

## -----------------------------------------------------------------------------
m
aggregate_byname(m, aggregation_map = list(birds = c("duck", "goose"), 
                                           zguitarists = c("John", "Paul", "George")))

## ----eval=FALSE---------------------------------------------------------------
#  # Not run
#  aggregate_byname(m, aggregation_map = list(Beatles = c("John", "Paul", "George", "Ringo")))

## -----------------------------------------------------------------------------
m_pieces <- matrix(c(1, 2, 3,
                     4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                   dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                                   c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
m_pieces

## -----------------------------------------------------------------------------
m_pieces
rename_to_piece_byname(m_pieces, piece = "pref", margin = 1, 
                       notation = RCLabels::bracket_notation)
rename_to_piece_byname(m_pieces, piece = "suff", margin = 1, 
                       notation = RCLabels::bracket_notation)
rename_to_piece_byname(m_pieces, piece = "from", margin = 1, 
                       notation = RCLabels::bracket_notation)
rename_to_piece_byname(m_pieces, piece = "pref", margin = 2,
                       notation = RCLabels::arrow_notation)
rename_to_piece_byname(m_pieces, piece = "suff", margin = 2,
                       notation = RCLabels::arrow_notation)

## -----------------------------------------------------------------------------
m_pieces
rename_to_piece_byname(m_pieces, piece = "pref", margin = 1)

## -----------------------------------------------------------------------------
rename_to_piece_byname(m_pieces, piece = "pref", margin = c(1, 2))

## -----------------------------------------------------------------------------
rename_to_piece_byname(m_pieces, piece = "pref")
rename_to_piece_byname(m_pieces, piece = "suff")

## -----------------------------------------------------------------------------
rename_to_piece_byname(m_pieces, piece = "suff", choose_most_specific = TRUE)

## -----------------------------------------------------------------------------
rename_to_piece_byname(m_pieces, piece = "noun")

## -----------------------------------------------------------------------------
m_pieces_with_types <- m_pieces %>% 
  setrowtype("Product") %>% setcoltype("Industry")
m_pieces_with_types
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "pref", margin = "Product")
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Product")
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "from", margin = "Product")
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Product", choose_most_specific = TRUE)
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Industry")

## -----------------------------------------------------------------------------
m_pieces
# Aggregate Electricity in rows
aggregate_pieces_byname(m_pieces, piece = "pref", margin = 1, 
                        notation = RCLabels::bracket_notation)
# Aggregate useful energy types in columns
aggregate_pieces_byname(m_pieces, piece = "suff", margin = 2,
                        notation = RCLabels::arrow_notation)

## -----------------------------------------------------------------------------
m_pieces
# Aggregate by original energy type
aggregate_pieces_byname(m_pieces, piece = "from", margin = 1, 
                        notation = RCLabels::bracket_notation, 
                        aggregation_map = list(`All sources` = c("Coal", "Solar")))

aggregate_pieces_byname(m_pieces, piece = "suff", margin = 2, 
                        notation = RCLabels::arrow_notation, 
                        aggregation_map = list(`Transport` = "MD"))

## -----------------------------------------------------------------------------
m_pieces
res <- rename_to_piece_byname(list(m_pieces, m_pieces), 
                              piece = list("pref", "suff"), 
                              margin = list(1, 2),
                              notation = list(RCLabels::bracket_notation, 
                                              RCLabels::arrow_notation))
res
df <- tibble::tibble(mats = list(m_pieces, m_pieces), 
                     pce = list("suff", "pref"), 
                     mgn = list(1, 2), 
                     am = list(list(Sources = c("Coal", "Solar")), 
                               list(Transport = c("Motors", "Cars"))), 
                     notn = list(RCLabels::from_notation, RCLabels::arrow_notation))
df
res2 <- df %>%
  dplyr::mutate(
    aggregated = aggregate_pieces_byname(mats, piece = pce, margin = mgn, 
                                         aggregation_map = am, notation = notn)
  )
res2
res2$aggregated[[1]]
res2$aggregated[[2]]

## -----------------------------------------------------------------------------
df_simple <- tibble::tribble(~key, ~val, 
                             "A", 1, 
                             "A", 2, 
                             "B", 10)
df_simple
df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(val = sum(val))

## -----------------------------------------------------------------------------
# 2 rows are expected. 3 are observed.
df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(val = sum_byname(val), .groups = "drop")

## -----------------------------------------------------------------------------
res <- df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(val = sum_byname(val, .summarise = TRUE))
# res$val is a list column.
res
res$val

## -----------------------------------------------------------------------------
m <- matrix(c(11, 12, 13,
              21, 22, 23), nrow = 2, ncol = 3, byrow = TRUE, 
            dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
df <- tibble::tibble(key = c("A", "A", "B"), m = list(m, m, m))
unexpected <- df %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(m = sum_byname(m), .groups = "drop")
# 2 rows are expected. 3 are observed.
unexpected
res <- df %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(m = sum_byname(m, .summarise = TRUE))
res
res$m[[1]]
res$m[[2]]

## -----------------------------------------------------------------------------
df <- tibble::tribble(~member, ~role, ~band, 
                      "John", "guitarists", "The Beatles", 
                      "Paul", "guitarists", "The Beatles", 
                      "George", "guitarists", "The Beatles", 
                      "Ringo", "drummers", "The Beatles", 
                      "Mick", "singers", "Rolling Stones", 
                      "Keith", "guitarists", "Rolling Stones", 
                      "Ronnie", "guitarists", "Rolling Stones", 
                      "Bill", "guitarists", "Rolling Stones", 
                      "Charlie", "drummers", "Rolling Stones")
df
bands_membs_agg_map <- agg_table_to_agg_map(df, few_colname = "band", many_colname = "member")
bands_membs_agg_map
agg_table_to_agg_map(df, few_colname = "role", many_colname = "member")

## -----------------------------------------------------------------------------
agg_map_to_agg_table(bands_membs_agg_map, 
                      few_colname = "bands",
                      many_colname = "members")

