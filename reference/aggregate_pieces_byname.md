# Aggregate a matrix by pieces of row and/or column names

Aggregate a matrix (or list of matrices or a column in a `matsindf` data
frame) by pieces of the row and column names.

## Usage

``` r
aggregate_pieces_byname(
  a,
  piece,
  margin = list(c(1, 2)),
  inf_notation = TRUE,
  notation = list(RCLabels::notations_list),
  choose_most_specific = FALSE,
  prepositions = list(RCLabels::prepositions_list),
  aggregation_map = NULL,
  pattern_type = "exact"
)
```

## Arguments

- a:

  A matrix or list of matrices.

- piece:

  A character string indicating which piece of the row or column names
  to retain, one of "noun", "pps", "pref" or "suff", or a preposition,
  indicating which part of the row or column name is to be retained.

- margin:

  As a character, the row type or column type to be renamed. As an
  integer, the margin to be renamed. Default is `c(1, 2)`, meaning that
  both rows (`margin = 1`) and columns (`margin = 2`) will be renamed.

- inf_notation:

  A boolean that tells whether to infer notation. Default is `TRUE`.

- notation:

  The notation used for row and column labels. Default is
  `list(RCLabels::notations_list)`. The default value is wrapped in a
  list, because
  [`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html)
  is, itself, a list. See `RCLabels`.

- choose_most_specific:

  A boolean that indicates whether the most-specific notation will be
  inferred when more than one of `notation` matches a row or column
  label and `allow_multiple = FALSE`. When `FALSE`, the first matching
  notation in `notations` is returned when `allow_multiple = FALSE`.
  Default is `FALSE`.

- prepositions:

  Prepositions that can be used in the row and column label. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- aggregation_map:

  A named list of rows or columns to be aggregated (or `NULL`). See
  `details`.

- pattern_type:

  See
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  Default is "exact".

## Value

A version of `a` with rows and/or columns aggregated according to
`aggregation_map`.

## Details

This is a convenience function that bundles two others for common use
cases:
[`rename_to_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_piece_byname.md)
followed by
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md).
Note that after renaming to the piece, there may be rows or columns that
are identically named. If those identically named names aren't included
in the `aggregation_map`, an error will result. So,
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md)
is called twice; first with `aggregation_map = NULL` to sweep up any
rows or columns that are identically named after renaming and second
with `aggregation_map = aggregation_map` to sum the desired rows or
columns. See examples.

When `aggregation_map` is `NULL` (the default), rows (or columns or
both) of same name are aggregated together.

If `aggregation_map` is not `NULL`, it must be a named list. The name of
each `aggregation_map` item is the name of a row or column in output
that will contain the specified aggregation. The value of each item in
`aggregation_map` must be a vector of names of rows or columns in `a`.
The names in the value are aggregated and inserted into the output with
the name of the value. For example
`aggregation_map = list(new_row = c("r1", "r2"))` will aggregate rows
"r1" and "r2", delete rows "r1" and "r2", and insert a new row whose
name is "new_row" and whose value is the sum of rows "r1" and "r2'.

The values in the `aggregation_map` are interpreted as regular
expressions, and they are escaped using
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html)
prior to use.

`aggregation_map` should aggregate by pieces, not by the full, original
row and/or column names.

## Examples

``` r
a <- matrix(c(1, 2, 3, 
              4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
            dimnames = list(c("a [from b]", "c [from d]"), 
                            c("e [from f]", "g [from h]", "i [from j]")))
a %>%
  aggregate_pieces_byname(piece = "suff", 
                          notation = RCLabels::from_notation,
                          aggregation_map = list(rows = c("b", "d"), 
                                                 cols = c("h", "j")))
#>      cols f
#> rows   16 5
m <- matrix(c(1, 0, 0, 
              0, 1, 1, 
              0, 1, 1), nrow = 3, ncol = 3, byrow = TRUE, 
            dimnames = list(c("Gasoline [from Oil refineries]", 
                              "Electricity [from Main activity producer electricity plants]", 
                              "Electricity [from Hydro]"),
                            c("Automobiles", "LED lamps", "CFL lamps"))) %>%
  setrowtype("Product") %>% setcoltype("Industry")
mT <- transpose_byname(m)
# Aggregate the "Electricity" rows.
aggregate_pieces_byname(m, piece = "noun", margin = "Product",
                        notation = RCLabels::bracket_notation)
#>             Automobiles LED lamps CFL lamps
#> Electricity           0         2         2
#> Gasoline              1         0         0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
# Also works in a list.
aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                        margin = "Product",
                        notation = RCLabels::bracket_notation)
#> [[1]]
#>             Automobiles LED lamps CFL lamps
#> Electricity           0         2         2
#> Gasoline              1         0         0
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>             Electricity Gasoline
#> Automobiles           0        1
#> LED lamps             2        0
#> CFL lamps             2        0
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
# Use an aggregation map
aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                        margin = "Product",
                        aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
                        notation = RCLabels::bracket_notation)
#> [[1]]
#>       Automobiles LED lamps CFL lamps
#> final           1         2         2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>             final
#> Automobiles     1
#> LED lamps       2
#> CFL lamps       2
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
# Also works in a data frame.
df <- tibble::tibble(m = list(m, mT), 
                     pce = "noun",
                     mgn = "Product",
                     agg_map = list(list(final = c("Electricity", "Gasoline"))), 
                     notn = list(RCLabels::bracket_notation)) %>%
  dplyr::mutate(
    agg = aggregate_pieces_byname(a = m, piece = pce, margin = mgn, 
                                  aggregation_map = agg_map,
                                  notation = notn)
  )
df$agg
#> [[1]]
#>       Automobiles LED lamps CFL lamps
#> final           1         2         2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>             final
#> Automobiles     1
#> LED lamps       2
#> CFL lamps       2
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
# Works when renaming to the piece results in identical row or col names.
b <- matrix(1:6, nrow = 3, ncol = 2, 
            dimnames = list(c("a [from b]", "c [from d]", "c [from e]"), 
                            c("c1", "c2")))
b
#>            c1 c2
#> a [from b]  1  4
#> c [from d]  2  5
#> c [from e]  3  6
# This aggregation works, because the "c" rows
# are aggregated before applying the aggregation_map,
# which, itself, does NOT aggregate the "c" rows.
b %>% 
  aggregate_pieces_byname(piece = "noun",
                          margin = 1,
                          inf_notation = FALSE, 
                          notation = RCLabels::bracket_notation, 
                          aggregation_map = list(f = c("a", "b")))
#>   c1 c2
#> c  5 11
#> f  1  4
```
