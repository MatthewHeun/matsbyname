# Is an object a Matrix?

Arguably, this function should be in the `Matrix` package, but it is
not. We include it here for convenience.

## Usage

``` r
is.Matrix(a)
```

## Arguments

- a:

  The object to be queried if it is Matrix.

## Value

A boolean. `TRUE` if `a` is a `Matrix`, `FALSE` otherwise.

## Details

This function is not vectorized.

`is.Matrix()` is a wrapper for `inherits(a, "Matrix)`.

## Examples

``` r
is.Matrix(matrix(42))
#> [1] FALSE
is.Matrix(Matrix::Matrix(42))
#> [1] TRUE
```
