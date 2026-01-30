# Create a constant vector from matrix `a`

This function creates a vector using `a` as a template and `k` as its
value. Row names are taken from the row names of `a`. The column name of
the output is given by `colname`. Row and column types are transferred
from `a` to the output, directly.

## Usage

``` r
kvec_from_template_byname(a, k = 1, colname = NA, column = TRUE)
```

## Arguments

- a:

  The template matrix for the column vector.

- k:

  The value of the entries in the output column vector.

- colname:

  The name of the output vector's 1-sized dimension (the only column if
  `column` is `TRUE`, the only row otherwise).

- column:

  Tells whether a column vector (if `TRUE`, the default) or a row vector
  (if `FALSE`) should be created.

## Value

A vector vector formed from `a`.

## Details

If `column` is `TRUE`, the output is a column vector with row names
taken from row names of `a` and a column named by `colname`. If `column`
is `FALSE`, the output is a row vevtor with column names taken from
column names of `a` and a row named by `colname`.

If the class of `a` is `Matrix`, the output object will be a `Matrix`.
Otherwise, the class of the output object will be a `matrix`.

## Examples

``` r
kvec_from_template_byname(matrix(42, nrow = 4, ncol = 2,
                                 dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2"))), 
                          colname = "new column")
#>    new column
#> r1          1
#> r2          1
#> r3          1
#> r4          1
kvec_from_template_byname(matrix(42, nrow = 4, ncol = 2,
                                 dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2"))), 
                          colname = "new row", column = FALSE)
#>         c1 c2
#> new row  1  1
```
