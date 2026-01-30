# Select zero columns

Matrices with columns containing all zeroes are not invertible
(singular). To diagnose this problem, it is useful to find the zero
columns of a singular matrix. This function selects (extracts) only the
zero columns of a matrix.

## Usage

``` r
selectzerocols_byname(a, tol = 1e-06)
```

## Arguments

- a:

  A matrix or a list of matrices.

- tol:

  The allowable deviation from 0 for any element.

## Value

`a` with only zero columns selected.

## Details

A column is said to be a zero column if all elements are within `tol` of
zero.

## Examples

``` r
m <- matrix(c(1, 0, 1,
              1, 0, 1),
            dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
            nrow = 2, ncol = 3, byrow = TRUE) %>% 
  setrowtype("rows") %>% setcoltype("cols")
selectzerocols_byname(m)
#>    c2
#> r1  0
#> r2  0
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
```
