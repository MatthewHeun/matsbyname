# Sorts rows and columns of a matrix

Checks that row names are unique and that column names are unique. Then,
sorts the rows and columns in a way that ensures any other matrix with
the same row and column names will have the same order.

## Usage

``` r
sort_rows_cols(a, margin = c(1, 2), roworder = NA, colorder = NA)
```

## Arguments

- a:

  A matrix or data frame whose rows and columns are to be sorted.

- margin:

  Specifies the subscript(s) in `a` over which sorting will occur.
  `margin` has nearly the same semantic meaning as in
  [`base::apply`](https://rdrr.io/r/base/apply.html). For rows only,
  give `1`; for columns only, give `2`; for both rows and columns, give
  `c(1,2)`, the default value.

- roworder:

  Specifies the order for rows with default `sort(rownames(a))`. If `NA`
  (the default), default sort order is used. Unspecified rows are
  removed from the output, thus providing a way to delete rows from `a`.
  Extraneous row names (row names in `roworder` that do not appear in
  `a`) are ignored.

- colorder:

  Specifies the order for rows with default `sort(colnames(a))`. If `NA`
  (the default), default sort order is used. Unspecified columns are
  removed from the output, thus providing a way to delete columns from
  `a`. Extraneous column names (column names in `colorder` that do not
  appear in `a`) are ignored.

## Value

A modified version of `a` with sorted rows and columns

## Details

Default sort order is given by
[`base::sort()`](https://rdrr.io/r/base/sort.html) with
`decreasing = FALSE`.

## Examples

``` r
m <- matrix(c(1:6), nrow=3, dimnames = list(c("r3", "r5", "r1"), c("c4", "c2")))
sort_rows_cols(m)
#>    c2 c4
#> r1  6  3
#> r3  4  1
#> r5  5  2
sort_rows_cols(t(m))
#>    r1 r3 r5
#> c2  6  4  5
#> c4  3  1  2
sort_rows_cols(m, margin=1) # Sorts rows
#>    c4 c2
#> r1  3  6
#> r3  1  4
#> r5  2  5
sort_rows_cols(m, margin=2) # Sorts columns
#>    c2 c4
#> r3  4  1
#> r5  5  2
#> r1  6  3
v <- matrix(c(1:5), ncol=1, dimnames=list(rev(paste0("r", 1:5)), "c1")) # Column vector
sort_rows_cols(v)
#>    c1
#> r1  5
#> r2  4
#> r3  3
#> r4  2
#> r5  1
sort_rows_cols(v, margin = 1) # Sorts rows
#>    c1
#> r1  5
#> r2  4
#> r3  3
#> r4  2
#> r5  1
sort_rows_cols(v, margin = 2) # No effect: only one column
#>    c1
#> r5  1
#> r4  2
#> r3  3
#> r2  4
#> r1  5
r <- matrix(c(1:4), nrow=1, dimnames=list("r1", rev(paste0("c", 1:4)))) # Row vector
sort_rows_cols(r) # Sorts columns
#>    c1 c2 c3 c4
#> r1  4  3  2  1
n <- matrix(c(1,2), nrow = 1, dimnames = list(NULL, c("c2", "c1"))) # No row name
sort_rows_cols(n) # Sorts columns, because only one row.
#>      c1 c2
#> [1,]  2  1
# Also works with lists
sort_rows_cols(list(m,m)) # Sorts rows and columns for both m's.
#> [[1]]
#>    c4 c2
#> r1  3  6
#> r3  1  4
#> r5  2  5
#> 
#> [[2]]
#>    c2 c4
#> r3  4  1
#> r5  5  2
#> r1  6  3
#> 
# Sort rows only for first one, sort rows and columns for second one.  
# Row order is applied to all m's.  Column order is natural.
sort_rows_cols(a = list(m,m), margin = 1, roworder = list(c("r5", "r3", "r1")))
#> [[1]]
#>    c4 c2
#> r5  2  5
#> r3  1  4
#> r1  3  6
#> 
#> [[2]]
#>    c4 c2
#> r5  2  5
#> r3  1  4
#> r1  3  6
#> 
# Columns are sorted as default, because no colorder is given.
# roworder is ignored. 
sort_rows_cols(a = list(m,m), margin = 2, roworder = list(c("r5", "r3", "r1")))
#> [[1]]
#>    c2 c4
#> r3  4  1
#> r5  5  2
#> r1  6  3
#> 
#> [[2]]
#>    c2 c4
#> r3  4  1
#> r5  5  2
#> r1  6  3
#> 
# Both columns and rows sorted, rows by the list, columns in natural order.
sort_rows_cols(a = list(m,m), margin = c(1,2), roworder = list(c("r5", "r3", "r1")))
#> [[1]]
#>    c4 c2
#> r5  2  5
#> r3  1  4
#> r1  3  6
#> 
#> [[2]]
#>    c2 c4
#> r3  4  1
#> r5  5  2
#> r1  6  3
#> 
```
