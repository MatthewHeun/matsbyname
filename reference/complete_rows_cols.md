# Complete rows and columns in one matrix relative to another

"Completing" rows and columns means that `a` contains a union of rows
and columns between `a` and `mat`, with missing data represented by the
value for `fill` (`0`, by default), `fillrow`, or `fillcol`.

## Usage

``` r
complete_rows_cols(
  a = NULL,
  mat = NULL,
  fill = 0,
  fillrow = NULL,
  fillcol = NULL,
  margin = c(1, 2)
)
```

## Arguments

- a:

  A `matrix` or list of `matrix` objects to be completed. `a` can be
  `Matrix` objects, too.

- mat:

  A `matrix` or `Matrix` from which dimnames will be extracted for the
  purposes of completing `a` with respect to `mat`.

- fill:

  Rows and columns added to `a` will contain the value `fill`. (Default
  is `0`.)

- fillrow:

  A row vector of type `matrix` with same column names as `a`. Any rows
  added to `a` will be `fillrow`. If non-`NULL`, `fillrow` takes
  precedence over both `fillcol` and `fill` in the case of conflicts.

- fillcol:

  A column vector of type matrix with same row names as `a`. Any columns
  added to `a` will be `fillcol`. If non-`NULL`, `fillcol` takes
  precedence over `fill` in the case of conflicts.

- margin:

  Specifies the subscript(s) in `a` over which completion will occur
  `margin` has nearly the same semantic meaning as in
  [`base::apply()`](https://rdrr.io/r/base/apply.html) For rows only,
  give `1`; for columns only, give `2`; for both rows and columns, give
  `c(1,2)`, the default value.

## Value

A modified version of `a` possibly containing additional rows and
columns whose names are obtained from `mat` and whose values are
obtained from `fillrow`, `fillcol` or `fill` (in that order of
preference).

## Details

Note that `complete_rows_cols(mat1, mat2)` and
`complete_rows_cols(mat2, mat1)` are not guaranteed to have the same
order for rows and columns. (Nor are the values in the matrix guaranteed
to have the same positions.)

If `dimnames(mat)` is `NULL`, `a` is returned unmodified.

If either `a` or `mat` are missing names on a margin (row or column), an
error is given.

When `a` is non-`NULL`, `a` is named, and `mat` is `NULL` (the default),
`a` is completed relative to itself, meaning that `a` will be made
square, containing the union of row and column names from `a`. Under
these conditions, no warning is given.

If `mat` is non-`NULL` and dimnames of `mat` cannot be determined
(because, for example, `mat` doesn't have dimnames), `a` is completed
relative to itself and a warning is given.

All added rows and columns will be created from one of the `fill*`
arguments. When conflicts arise, precedence among the `fill*` arguments
is `fillrow` then `fillcol` then `fill`.

## Examples

``` r
m1 <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
m1
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
m2 <- matrix(c(7:12), ncol=3, dimnames = list(c("r2", "r3"), c("c2", "c3", "c4")))
m2
#>    c2 c3 c4
#> r2  7  9 11
#> r3  8 10 12
complete_rows_cols(m1, m2) # Adds empty column c4
#>    c1 c2 c3 c4
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
complete_rows_cols(m1, t(m2)) # Creates r2, r3 columns; c2, c3, c4 rows
#>    c1 c2 r2 r3
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
#> c2  0  0  0  0
#> c3  0  0  0  0
#> c4  0  0  0  0
complete_rows_cols(m1, m2, margin = 1) # No changes because r2 and r3 already present in m1
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
complete_rows_cols(m1, m2, margin = 2) # Adds empty columns c3 and c4
#>    c1 c2 c3 c4
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
complete_rows_cols(m1, t(m2), margin = 1) # Adds empty rows c2, c3, c4
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> c2  0  0
#> c3  0  0
#> c4  0  0
complete_rows_cols(m1, m2, fill = 100) # Adds columns c3 and c4 with 100's
#>    c1 c2  c3  c4
#> r1  1  4 100 100
#> r2  2  5 100 100
#> r3  3  6 100 100
complete_rows_cols(m1, m1) # Nothing added, because everything already present
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
complete_rows_cols(m1, t(m1)) # Adds empty c1, c2 rows; Adds empty r1, r2, r3 columns
#>    c1 c2 r1 r2 r3
#> r1  1  4  0  0  0
#> r2  2  5  0  0  0
#> r3  3  6  0  0  0
#> c1  0  0  0  0  0
#> c2  0  0  0  0  0
# Same as previous. With missing matrix, complete relative to transpose of m1.
complete_rows_cols(m1) 
#>    c1 c2 r1 r2 r3
#> r1  1  4  0  0  0
#> r2  2  5  0  0  0
#> r3  3  6  0  0  0
#> c1  0  0  0  0  0
#> c2  0  0  0  0  0
# Adds rows r10, r11; cols c10, c11
complete_rows_cols(m1, matrix(0, nrow = 2, ncol = 2, 
                              dimnames = list(c("r10", "r11"), c("c10", "c11")))) 
#>     c1 c2 c10 c11
#> r1   1  4   0   0
#> r2   2  5   0   0
#> r3   3  6   0   0
#> r10  0  0   0   0
#> r11  0  0   0   0
# Also works with lists
complete_rows_cols(a = list(m1,m1))
#> [[1]]
#>    c1 c2 r1 r2 r3
#> r1  1  4  0  0  0
#> r2  2  5  0  0  0
#> r3  3  6  0  0  0
#> c1  0  0  0  0  0
#> c2  0  0  0  0  0
#> 
#> [[2]]
#>    c1 c2 r1 r2 r3
#> r1  1  4  0  0  0
#> r2  2  5  0  0  0
#> r3  3  6  0  0  0
#> c1  0  0  0  0  0
#> c2  0  0  0  0  0
#> 
complete_rows_cols(a = list(m1,m1), mat = list(m2,m2))
#> [[1]]
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> 
#> [[2]]
#>    c1 c2 c3 c4
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
#> 
# No changes because r2, r3 already present in m1
complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 1) 
#> [[1]]
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> 
#> [[2]]
#>    c1 c2
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> 
complete_rows_cols(a = list(m1,m1), mat = list(m2,m2), margin = 2)
#> [[1]]
#>    c1 c2 c3 c4
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
#> 
#> [[2]]
#>    c1 c2 c3 c4
#> r1  1  4  0  0
#> r2  2  5  0  0
#> r3  3  6  0  0
#> 
complete_rows_cols(a = list(m1,m1), 
                   mat = RCLabels::make_list(matrix(0,
                                                    nrow = 2, 
                                                    ncol = 2, 
                                                    dimnames = list(c("r10", "r11"), 
                                                                    c("c10", "c11"))), 
                                             n = 2, lenx = 1))
#> [[1]]
#>     c1 c2
#> r1   1  4
#> r2   2  5
#> r3   3  6
#> r10  0  0
#> r11  0  0
#> 
#> [[2]]
#>    c1 c2 c10 c11
#> r1  1  4   0   0
#> r2  2  5   0   0
#> r3  3  6   0   0
#> 
# fillrow or fillcol can be specified
a <- matrix(c(11, 12, 21, 22), byrow = TRUE, nrow = 2, ncol = 2, 
            dimnames = list(c("r1", "r2"), c("c1", "c2")))
b <- matrix(c(1:6), byrow = TRUE, nrow = 3, ncol = 2, 
            dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
fillrow <- matrix(c(31, 32), byrow = TRUE, nrow = 1, ncol = 2, 
                  dimnames = list("r42", c("c1", "c2")))
complete_rows_cols(a = a, mat = b, fillrow = fillrow)
#>    c1 c2
#> r1 11 12
#> r2 21 22
#> r3 31 32
```
