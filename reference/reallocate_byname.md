# Reallocate values from one row or column to others

There are situations where it is helpful to reallocate values from one
row or column to another, in proportion to remaining values in
corresponding columns or rows. This function performs the reallocation
operation. See examples.

## Usage

``` r
reallocate_byname(
  a,
  rownames = NULL,
  colnames = NULL,
  margin,
  .zero_behaviour = c("error", "warning", "zeroes", "allocate equally"),
  piece_rownames = "all",
  pattern_type_rownames = "exact",
  prepositions_rownames = RCLabels::prepositions_list,
  notation_rownames = RCLabels::notations_list,
  inf_notation_rownames = TRUE,
  choose_most_specific_rownames = FALSE,
  piece_colnames = "all",
  pattern_type_colnames = "exact",
  prepositions_colnames = RCLabels::prepositions_list,
  notation_colnames = RCLabels::notations_list,
  inf_notation_colnames = TRUE,
  choose_most_specific_colnames = FALSE
)
```

## Arguments

- a:

  A matrix or a list of matrices.

- rownames:

  The row names to reallocate. `NULL` (the default) means include all
  rows.

- colnames:

  The column names to reallocate. `NULL` (the default) means include all
  rows.

- margin:

  An integer vector of length 1 or a vector of integers where each entry
  has length 1. The margin of the matrix over which the reallocation
  should occur. The only valid values are `1` (reallocate to other rows)
  or `2` (reallocate to other columns). To reallocate both rows and
  columns, call the function twice.

- .zero_behaviour:

  Tells how to proceed when remaining (i.e., unallocated) rows or
  columns are all zero. Default is "error", which throws an error. See
  details for other options. If `a` is a `list`, applies to all items in
  the list.

- piece_rownames, piece_colnames:

  The piece of row or column names to be assessed. Default is "all",
  indicating that the entire label will be assessed. If `a` is a `list`,
  applies to all items in the list.

- pattern_type_rownames, pattern_type_colnames:

  The pattern type desired for row and column names. Default is "exact".
  Other options are "leading", "trailing", "anywhere", and "literal".
  See
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
  for details. If `a` is a `list`, applies to all items in the list.

- prepositions_rownames, prepositions_colnames:

  Prepositions used by
  [`select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.md)
  for row and column name matching. Default is
  [RCLabels::prepositions_list](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).
  If `a` is a `list`, applies to all items in the list.

- notation_rownames, notation_colnames:

  The row or column notation used by
  [`select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.md)
  for row and column name matching. Default is
  [RCLabels::notations_list](https://matthewheun.github.io/RCLabels/reference/notations_list.html).
  If `a` is a `list`, applies to all items in the list.

- inf_notation_rownames, inf_notation_colnames:

  A boolean used by
  [`select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.md)
  that tells whether to infer notation for rows and columns. Default is
  `TRUE`. See
  [`RCLabels::infer_notation()`](https://matthewheun.github.io/RCLabels/reference/infer_notation.html)
  for details. If `a` is a `list`, applies to all items in the list.

- choose_most_specific_rownames, choose_most_specific_colnames:

  A boolean used by
  [`select_rowcol_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rowcol_piece_byname.md)
  that tells whether to choose the most specific notation from
  `notation` when inferring notation. Default is `FALSE` so that a less
  specific notation can be inferred. In combination with
  `notations_list`s, the default value of `FALSE` means that
  [RCLabels::bracket_notation](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html)
  will be selected instead of anything more specific, such as
  [RCLabels::from_notation](https://matthewheun.github.io/RCLabels/reference/from_notation.html).
  If `a` is a `list`, applies to all items in the list.

## Value

A modified version of `a` with `rownames` or `colnames` redistributed.

## Details

This function will provide answers, but it is unlikely that the answers
will be meaningful when the remaining data (the rows or columns not
being reallocated) contain negative numbers.

The value of `margin` affects the interpretation of `rownames` and
`colnames`. If `margin = 1`, `rownames` identifies the rows to be
reallocated to other rows. `colnames` identifies the columns to be
reallocated, where `NULL` (the default) means that all columns are
reallocated. If `margin = 2`, `colnames` identifies the columns to be
reallocated to other columns. `rownames` identifies the rows to be
reallocated, where `NULL` (the default) means that all rows are
reallocated.

When the remaining rows or columns not being reallocated contain
exclusively zeroes, the result is determined by `.zero_behaviour`.
Options are one of:

- "error" (the default) to throw an error.

- "warning" to issue a warning but continue execution. Be careful with
  this option!

- "zeroes" to return zeroes in the row or column with zeroes. Note that
  "zeroes" and "warning" return the same value. "zeroes" does so without
  a warning.

- "allocate equally" to equally allocate across remaining rows or
  columns.

## Examples

``` r
m <- matrix(c(1, 2, 3,
              4, 5, 6,
              7, 8, 9), 
            nrow = 3, ncol = 3, byrow = TRUE, 
            dimnames = list(c("r1", "r2", "r3"), 
            c("c1", "c2", "c3")))
m
#>    c1 c2 c3
#> r1  1  2  3
#> r2  4  5  6
#> r3  7  8  9
# Move row 3 into the other rows (r1 and r2) proportionally
reallocate_byname(m, rownames = "r3", margin = 1)
#>     c1        c2 c3
#> r1 2.4  4.285714  6
#> r2 9.6 10.714286 12
# Move column 2 into the other columns (c1 and c3) proportionally
reallocate_byname(m, colnames = "c2", margin = 2)
#>      c1   c3
#> r1  1.5  4.5
#> r2  6.0  9.0
#> r3 10.5 13.5
# Demonstrate different options for reallocating when zeroes remain.
m2 <- matrix(c(1, 2,  0,
               4, 5,  0,
               7, 8, 10), 
             nrow = 3, ncol = 3, byrow = TRUE, 
             dimnames = list(c("r1", "r2", "r3"), 
             c("c1", "c2", "c3")))
m2
#>    c1 c2 c3
#> r1  1  2  0
#> r2  4  5  0
#> r3  7  8 10
reallocate_byname(m2, rownames = "r3", margin = 1, 
                  .zero_behaviour = "zeroes")
#>     c1        c2 c3
#> r1 2.4  4.285714  0
#> r2 9.6 10.714286  0
reallocate_byname(m2, rownames = "r3", margin = 1, 
                  .zero_behaviour = "allocate equally")
#>     c1        c2 c3
#> r1 2.4  4.285714  5
#> r2 9.6 10.714286  5
if (FALSE) { # \dontrun{
# "error" will cause an error to be emitted.
reallocate_byname(m2, rownames = "r3", margin = 1, 
                  .zero_behaviour = "error")
# "warning" will cause a warning to be emitted
# and will return a result that is the same as "zeroes".
reallocate_byname(m2, rownames = "r3", margin = 1, 
                  .zero_behaviour = "warning")
} # }
```
