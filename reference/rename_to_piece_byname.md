# Rename matrix rows and columns by piece of row or column names

It can be convenient to rename rows or columns of matrices based on
retaining only a piece of the row and/or column names. This function
provides that capability.

## Usage

``` r
rename_to_piece_byname(
  a,
  piece,
  margin = list(c(1, 2)),
  inf_notation = TRUE,
  notation = list(RCLabels::notations_list),
  choose_most_specific = FALSE,
  prepositions = list(RCLabels::prepositions_list)
)
```

## Arguments

- a:

  A matrix or list of matrices whose rows or columns will be renamed.

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
  is, itself, a list. If `notation` is not a list, unexpected behavior
  can result. See `RCLabels`.

- choose_most_specific:

  A boolean that indicates whether the most-specific notation will be
  inferred when more than one of `notation` matches a row or column
  label and `allow_multiple = FALSE`. When `FALSE`, the first matching
  notation in `notations` is returned when `allow_multiple = FALSE`.
  Default is `FALSE`.

- prepositions:

  Prepositions that can be used in the row and column label. Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

## Value

A version of `a` with renamed rows and columns.

## Details

Internally, this function finds pieces of row and column names via the
`RCLabels` package. `piece` can be anything that
[`RCLabels::get_piece()`](https://matthewheun.github.io/RCLabels/reference/get_piece.html)
understands. Note that `margin` can be either an integer vector or a
character vector. If `margin` is a character vector, it is interpreted
as a row or column type, and
[`margin_from_types_byname()`](https://matthewheun.github.io/matsbyname/reference/margin_from_types_byname.md)
is called internally to resolve the integer margins of interest.

Note that if row and/or column type are present, the row and/or column
type are also renamed according to `piece`. This behavior is usually
helpful. For example, if the prefix is requested (`piece = "pref"`) and
the row/coltype is a single word that does not conform to the notation,
the entire row/coltype string is retained. However, if the suffix is
requested (`piece = "suff"`) and the row/coltype is a single word that
does not conform to the notation, an empty string ("") is returned. In
those circumstances, the caller is responsible for setting the
row/coltype if an empty string ("") is not desired. See the examples for
details.

## Examples

``` r
m <- matrix(c(1, 2, 
              3, 4, 
              5, 6), nrow = 3, byrow = TRUE, 
            dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
m
#>        a -> b c -> d
#> a -> b      1      2
#> r2          3      4
#> r3          5      6
rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#>    a c
#> a  1 2
#> r2 3 4
#> r3 5 6
m2 <- m %>%
  setrowtype("rows") %>% setcoltype("cols")
m2
#>        a -> b c -> d
#> a -> b      1      2
#> r2          3      4
#> r3          5      6
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
# In this example, 
# rowtype and coltype are unchanged, because the 
# whole string is considered to be the prefix.
rename_to_piece_byname(m2, piece = "pref", margin = "rows",
                       notation = RCLabels::arrow_notation)
#>    a -> b c -> d
#> a       1      2
#> r2      3      4
#> r3      5      6
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
# Here, the rowtype is set to the empty string ("")
# because there is no suffix for the type of the "rows" margin.
rename_to_piece_byname(m2, piece = "suff", margin = "rows",
                       notation = RCLabels::arrow_notation)
#>   a -> b c -> d
#> b      1      2
#>        3      4
#>        5      6
#> attr(,"rowtype")
#> [1] ""
#> attr(,"coltype")
#> [1] "cols"
m3 <- m2 |> 
  setrowtype("Industry -> Product")
m3
#>        a -> b c -> d
#> a -> b      1      2
#> r2          3      4
#> r3          5      6
#> attr(,"rowtype")
#> [1] "Industry -> Product"
#> attr(,"coltype")
#> [1] "cols"
# Note that the rowtype becomes the prefix for the rowtype, 
# in this example "Industry".
rename_to_piece_byname(m3, piece = "pref", margin = 1,
                       notation = RCLabels::arrow_notation)
#>    a -> b c -> d
#> a       1      2
#> r2      3      4
#> r3      5      6
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "cols"
# And when a suffix is present, 
# the rowtype becomes the suffix, 
# in this example "Product".
rename_to_piece_byname(m3, piece = "suff", margin = 1,
                       notation = RCLabels::arrow_notation)
#>   a -> b c -> d
#> b      1      2
#>        3      4
#>        5      6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "cols"
```
