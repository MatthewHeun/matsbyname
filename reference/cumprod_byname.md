# Cumulative element-product that respects row and column names

Provides cumulative element-products along a list or column of a data
frame. If `a` is a single number, `a` is returned. If `a` is a list of
numbers, a list representing the cumulative product of the numbers is
returned. If `a` is a single matrix, `a` is returned. If `a` is a list
of matrices, a list representing the cumulative product of the matrices
is returned. In this case, each entry in the returned list is product
"by name," such that row and column names of the matrices are respected.

## Usage

``` r
cumprod_byname(a)
```

## Arguments

- a:

  A number, list of numbers, matrix or list of matrices for which
  cumulative element product is desired.

## Value

A single number, list of numbers, a single matrix, or a list of
matrices, depending on the nature of `a`.

## Details

This function respects groups if `a` is a variable in a data frame.

## Examples

``` r
cumprod_byname(list(1, 2, 3, 4, 5))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 24
#> 
#> [[5]]
#> [1] 120
#> 
m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>%
  setrowtype("row") %>% setcoltype("col")
m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>%
  setrowtype("row") %>% setcoltype("col")
m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>%
  setrowtype("row") %>% setcoltype("col")
cumprod_byname(list(m1, m2, m3))
#> [[1]]
#>    c1
#> r1  1
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[2]]
#>    c1 c2
#> r1  0  0
#> r2  0  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[3]]
#>    c1 c2 c3
#> r1  0  0  0
#> r2  0  0  0
#> r3  0  0  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
```
