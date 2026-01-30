# Select zero rows

Matrices with rows containing all zeroes are not invertible (singular).
To diagnose this problem, it is useful to find the zero rows of a
singular matrix. This function selects (extracts) only the zero rows of
a matrix.

## Usage

``` r
selectzerorows_byname(a, tol = 1e-06)
```

## Arguments

- a:

  A matrix or a list of matrices.

- tol:

  The allowable deviation from 0 for any element.

## Value

`a` with only zero rows selected.

## Details

A row is said to be a zero row if all elements are within `tol` of zero.

## Examples

``` r
m <- matrix(c(0, 0, 1,
              0, 0, 0), 
            dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")), 
            nrow = 2, ncol = 3, byrow = TRUE) %>% 
  setrowtype("rows") %>% setcoltype("cols")
m
#>    c1 c2 c3
#> r1  0  0  1
#> r2  0  0  0
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
selectzerorows_byname(m)
#>    c1 c2 c3
#> r2  0  0  0
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
```
