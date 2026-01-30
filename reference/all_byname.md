# Are all matrix elements `TRUE`?

Tells whether all elements in matrix `a` are true.

## Usage

``` r
all_byname(a)
```

## Arguments

- a:

  a matrix or list of matrices

## Value

`TRUE` if all elements of `a` are `TRUE`, `FALSE` otherwise

## Details

`a` can be a matrix or a list of matrices.

## Examples

``` r
all_byname(matrix(rep(TRUE, times = 4), nrow = 2, ncol = 2))
#> [1] TRUE
all_byname(matrix(c(TRUE, FALSE), nrow = 2, ncol = 1))
#> [1] FALSE
```
