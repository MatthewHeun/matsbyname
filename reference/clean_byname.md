# Clean (delete) rows or columns of matrices that contain exclusively `clean_value`

Cleaning is performed when all entries in a row or column or both,
depending on the value of `margin`, are within `+/- tol` of
`clean_value`. Internally, values are deemed within +/- of tol when
`abs(x - clean_value) <= tol`.

## Usage

``` r
clean_byname(a, margin = c(1, 2), clean_value = 0, tol = 0)
```

## Arguments

- a:

  The matrix to be cleaned.

- margin:

  The dimension over which cleaning should occur, `1` for rows, `2` for
  columns, or `c(1, 2)` for both rows and columns. Default is `c(1, 2)`.

- clean_value:

  The undesirable value. Default is `0`.

- tol:

  The tolerance with which any value is deemed equal to `clean_value`.
  Default is `0`.

## Value

A "cleaned" matrix, expunged of rows or columns that contain exclusively
`clean_value.`

## Details

If there is concern about machine precision, you might want to call this
function with `tol = .Machine$double.eps`.

When a row (when `margin = 1`) or a column (when `margin = 2`) contains
exclusively `clean_value` (within `tol`), the row or column is deleted
from the matrix.

## Examples

``` r
m <- matrix(c(-20, 1, -20, 2), nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
m
#>     c1  c2
#> r1 -20 -20
#> r2   1   2
m %>% clean_byname(margin = 1, clean_value = -20) # Eliminates -20, -20 row
#>    c1 c2
#> r2  1  2
# Nothing cleaned, because no columns contain all 0's (the default clean_value).
m %>% clean_byname(margin = 2) 
#>     c1  c2
#> r1 -20 -20
#> r2   1   2
# Also works with lists
list(m, m) %>% clean_byname(margin = 1, clean_value = -20)
#> [[1]]
#>    c1 c2
#> r2  1  2
#> 
#> [[2]]
#>    c1 c2
#> r2  1  2
#> 
# Also works with data frames
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
DF %>% clean_byname(margin = 1, clean_value = -20)
#> $m
#> $m[[1]]
#>    c1 c2
#> r2  1  2
#> 
#> $m[[2]]
#>    c1 c2
#> r2  1  2
#> 
#> 
m2 <- matrix(c(-20, -20, 0, -20, -20, 0, -20, -20, -20), nrow = 3,
             dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")) )
m2
#>     c1  c2  c3
#> r1 -20 -20 -20
#> r2 -20 -20 -20
#> r3   0   0 -20
clean_byname(m2, margin = c(1,2), clean_value = -20)
#>    c1 c2
#> r3  0  0
DF2 <- data.frame(m2 = I(list()))
DF2[[1, "m2"]] <- m2
DF2[[2, "m2"]] <- m2
DF2 %>% clean_byname(margin = c(1, 2), clean_value = -20)
#> $m2
#> $m2[[1]]
#>    c1 c2  c3
#> r3  0  0 -20
#> 
#> $m2[[2]]
#>     c1  c2
#> r1 -20 -20
#> r2 -20 -20
#> r3   0   0
#> 
#> 
```
