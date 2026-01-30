# Trim rows and/or columns from a matrix

By default, the `matsbyname` package expands matrices with `0` rows or
columns prior to matrix operations to ensure that rows and columns
match. There are times when trimming rows or columns is preferred over
the default behavior. This function trims rows or columns in `a` to
match the rows or columns of `mat`. The return value will have rows or
columns of `a` removed if they do not appear in `mat`.

## Usage

``` r
trim_rows_cols(
  a = NULL,
  mat = NULL,
  margin = c(1, 2),
  warn_if_a_incomplete = TRUE,
  a_piece = "all",
  mat_piece = "all",
  notation = RCLabels::bracket_notation,
  prepositions = RCLabels::prepositions_list
)
```

## Arguments

- a:

  A matrix to be trimmed.

- mat:

  The matrix to be used as the template for rows and/or columns of `a`.

- margin:

  The dimension of `a` to be trimmed. `1` means rows; `2` means columns.
  Default is `c(1,2)`.

- warn_if_a_incomplete:

  When `TRUE` (the default), a warning is emitted if `a` is missing
  entries on `margin` that are present in `mat`. Default is `TRUE`.

- a_piece:

  The portion of `a` labels to be used for comparison. Default is "all".

- mat_piece:

  The portion of `mat` labels to be used for comparison. Default is
  "all".

- notation:

  The notation for row and column labels. Default is
  [`RCLabels::bracket_notation`](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html).

- prepositions:

  The strings to be treated as prepositions in row and column labels.
  Default is
  [`RCLabels::prepositions_list`](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

## Value

Matrix `a` with rows or columns trimmed to match `mat`.

## Details

If `a` is `NULL`, `NULL` is returned. If `mat` is `NULL`, `a` is
returned unmodified. If `mat` has `NULL` dimnames, `a` is returned
unmodified. If `mat` has `NULL` for dimnames on `margin`, an error is
returned.

A common use case for this function is to trim `a`, because it has too
many entries on `margin`s compared to `mat`. This trimming will result
in a smaller result for any mathematical operations involving `a` and
`mat`. Typically, `a` should cover all the entries in `mat` on `margin`.
Thus, by default, this function warns if `a` is missing entries on
`margin` that are present in `mat`. To turn off this checking behavior,
set `warn_if_a_incomplete = FALSE`.

`a_piece` and `mat_piece` control which part of row and column names are
compared before trimming. The default values for `a_piece` and
`mat_piece` are "all", meaning that the entire label should be matched.
Other options for `a_piece` and `mat_piece` are "pref" and "suff", which
will match the prefix or suffix of the labels. Alternatively,
prepositions can be given such that objects of prepositions will be
matched. Examples include "from" or "in". See
[`RCLabels::get_piece()`](https://matthewheun.github.io/RCLabels/reference/get_piece.html)
for details.

## See also

[`RCLabels::get_piece()`](https://matthewheun.github.io/RCLabels/reference/get_piece.html),
which is used internally.

## Examples

``` r
a <- matrix(c(1, 2, 3, 
              4, 5, 6, 
              7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
            dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3"))) %>% 
 setrowtype("rowtype") %>% setcoltype("coltype")
mat <- matrix(c(1, 2, 3,
                4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
            dimnames = list(c("r1", "bogus"), c("c1", "bogus", "c2"))) %>% 
 setrowtype("rowtype") %>% setcoltype("coltype")
trim_rows_cols(a, mat, margin = 1)
#> Warning: In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat': bogus
#>    c1 c2 c3
#> r1  1  2  3
#> attr(,"rowtype")
#> [1] "rowtype"
#> attr(,"coltype")
#> [1] "coltype"
trim_rows_cols(a, mat, margin = 2)
#> Warning: In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat': bogus
#>    c1 c2
#> r1  1  2
#> r2  4  5
#> r3  7  8
#> attr(,"rowtype")
#> [1] "rowtype"
#> attr(,"coltype")
#> [1] "coltype"
trim_rows_cols(a, mat)
#> Warning: In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat': bogus
#> Warning: In trim_rows_cols, 'a' is missing the following rows or columns relative to 'mat': bogus
#>    c1 c2
#> r1  1  2
#> attr(,"rowtype")
#> [1] "rowtype"
#> attr(,"coltype")
#> [1] "coltype"
```
