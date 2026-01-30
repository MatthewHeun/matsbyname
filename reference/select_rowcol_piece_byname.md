# Select or remove rows or columns based on pieces of the names.

[`select_rows_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rows_byname.md)
and
[`select_cols_byname()`](https://matthewheun.github.io/matsbyname/reference/select_cols_byname.md)
select rows and columns using regex patterns This function performs
similar actions based on the pieces of row and column labels.

## Usage

``` r
select_rowcol_piece_byname(
  a,
  retain = NULL,
  remove = NULL,
  piece = "all",
  pattern_type = "exact",
  prepositions = RCLabels::prepositions_list,
  notation = RCLabels::notations_list,
  inf_notation = TRUE,
  choose_most_specific = FALSE,
  margin = c(1, 2)
)
```

## Arguments

- a:

  A matrix or list of matrices whose rows or columns are to be selected.

- retain:

  The row or column names to be retained. Default is `NULL`, meaning
  that removal is requested.

- remove:

  The row or column names to be removed. Default is `NULL`, meaning that
  retaining is requested.

- piece:

  The piece of row or column names to be assessed. Default is "all",
  indicating that the entire label will be assessed.

- pattern_type:

  The way to match label pieces. `pattern_type` is passed to
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  See
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
  for details. Default is "exact", meaning that exact matches are
  retained or removed. Other options are "leading", "trailing",
  "anywhere", and "literal".

- prepositions:

  The prepositions that can be used for identifying pieces. Default is
  [RCLabels::prepositions_list](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- notation:

  The notation for the row and column names. Default is
  [RCLabels::notations_list](https://matthewheun.github.io/RCLabels/reference/notations_list.html),
  meaning that all notations known to `RCLabels` will be assessed.

- inf_notation:

  A boolean that tells whether to infer notation for rows and columns.
  Default is `TRUE`. See
  [`RCLabels::infer_notation()`](https://matthewheun.github.io/RCLabels/reference/infer_notation.html)
  for details.

- choose_most_specific:

  A boolean that tells whether to choose the most specific notation from
  `notation` when inferring notation. Default is `FALSE` so that a less
  specific notation can be inferred. In combination with
  [RCLabels::notations_list](https://matthewheun.github.io/RCLabels/reference/notations_list.html),
  the default value of `FALSE` means that
  [RCLabels::bracket_notation](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html)
  will be selected instead of anything more specific, such as
  [RCLabels::from_notation](https://matthewheun.github.io/RCLabels/reference/from_notation.html).

- margin:

  The margin to which row or column removal is requested. `1` indicates
  rows; `2` indicates columns. Default is `c(1, 2)`, meaning that action
  should be taken on both rows and columns.

## Value

`a` with rows and/or column retained or removed.

## Details

This function uses the `RCLabels` package to match row and column names
by pieces.

To retain rows or columns, specify `retain`. To remove rows or columns,
specify `remove`.

If `a` has row and column types, a string may be passed to `margin`, in
which case the margin will be resolved. See examples.

`notation` may be a list of notations that could apply in `a`. This
function will try to infer the notation that applies to row and column
names.

Retaining takes precedence over removing, always.

Options for `piece` are

- "all" (the default), meaning that the entire label will be matched,

- "pref", meaning that the prefix will be matched,

- "suff", meaning that the suffix will be matched,

- "noun", meaning that the first part will be matched, and

- "from" (or another preposition), meaning that the object of that
  preposition will be matched.

If retaining or removing rows or columns results in no rows or columns
remaining in the matrix, `NULL` is returned.

## Examples

``` r
m <- matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1 [to a]", "r2 [to b]"), 
                              c("c1 [from c]", "c2 [from d]"))) %>% 
  setrowtype("rows") %>% setcoltype("cols")
m
#>           c1 [from c] c2 [from d]
#> r1 [to a]           1           2
#> r2 [to b]           3           4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "r1", piece = "noun", 
                           notation = RCLabels::to_notation, 
                           margin = 1)
#>           c1 [from c] c2 [from d]
#> r1 [to a]           1           2
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "b", piece = "to", 
                           notation = RCLabels::bracket_notation, 
                           margin = 1)
#>           c1 [from c] c2 [from d]
#> r2 [to b]           3           4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "c1", piece = "noun",
                           notation = RCLabels::bracket_notation, 
                           margin = 2)
#>           c1 [from c]
#> r1 [to a]           1
#> r2 [to b]           3
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "d", piece = "from", 
                           notation = RCLabels::bracket_notation, 
                           margin = 2)
#>           c2 [from d]
#> r1 [to a]           2
#> r2 [to b]           4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "c", piece = "from", 
                           notation = RCLabels::bracket_notation, 
                           margin = 2)
#>           c1 [from c]
#> r1 [to a]           1
#> r2 [to b]           3
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "b", piece = "to", 
                           notation = RCLabels::bracket_notation, 
                           margin = "rows")
#>           c1 [from c] c2 [from d]
#> r2 [to b]           3           4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
select_rowcol_piece_byname(m, retain = "c", piece = "from", 
                           notation = RCLabels::bracket_notation, 
                           margin = "cols")
#>           c1 [from c]
#> r1 [to a]           1
#> r2 [to b]           3
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
```
