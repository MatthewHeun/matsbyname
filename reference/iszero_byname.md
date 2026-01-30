# Test whether this is the zero matrix

Note that this function tests whether the elements of `abs(a)` are
`<= tol`. The default value for `tol` is `1e-6`. So, you can set
`tol = 0` to discover if `a` is EXACTLY the zero matrix.

## Usage

``` r
iszero_byname(a, tol = 1e-06)
```

## Arguments

- a:

  A matrix or list of matrices.

- tol:

  The allowable deviation from 0 for any element. Interpreted as an
  absolute value.

## Value

`TRUE` Iff this is the zero matrix within `tol`.

## Details

If `a` contains any `NA` values, `NA` is returned.

## Examples

``` r
zero <- matrix(0, nrow = 50, ncol = 50)
iszero_byname(zero)
#> [1] TRUE
nonzero <- matrix(1:4, nrow = 2)
iszero_byname(nonzero)
#> [1] FALSE
# Also works for lists
iszero_byname(list(zero, nonzero))
#> [[1]]
#> [1] TRUE
#> 
#> [[2]]
#> [1] FALSE
#> 
# And it works for data frames
DF <- data.frame(A = I(list()), B = I(list()))
DF[[1,"A"]] <- zero
DF[[2,"A"]] <- nonzero
DF[[1,"B"]] <- nonzero
DF[[2,"B"]] <- zero
iszero_byname(DF$A)
#> [[1]]
#> [1] TRUE
#> 
#> [[2]]
#> [1] FALSE
#> 
iszero_byname(DF$B)
#> [[1]]
#> [1] FALSE
#> 
#> [[2]]
#> [1] TRUE
#> 
iszero_byname(matrix(1e-10, nrow = 2))
#> [1] TRUE
iszero_byname(matrix(1e-10, nrow = 2), tol = 1e-11)
#> [1] FALSE
# When any NA value is present, NA is returned
iszero_byname(NA)
#> [1] NA
iszero_byname(matrix(c(0, NA), ncol = 2))
#> [1] NA
iszero_byname(list(matrix(c(0, NA)), zero, nonzero))
#> [[1]]
#> [1] NA
#> 
#> [[2]]
#> [1] TRUE
#> 
#> [[3]]
#> [1] FALSE
#> 
```
