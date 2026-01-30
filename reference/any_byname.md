# Are any matrix elements `TRUE`?

Tells whether any elements in matrix `a` are true.

## Usage

``` r
any_byname(a)
```

## Arguments

- a:

  a matrix or list of matrices

## Value

`TRUE` if any elements of `a` are `TRUE`, `FALSE` otherwise

## Details

`a` can be a matrix or a list of matrices.

## Examples

``` r
any_byname(matrix(c(TRUE, FALSE), nrow = 2, ncol = 1))
#> [1] TRUE
any_byname(matrix(rep(FALSE, times = 4), nrow = 2, ncol = 2))
#> [1] FALSE
```
