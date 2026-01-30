# Tells whether an object is one of a matrix or a Matrix

Often, it helps to know whether an object is a `matrix` or a `Matrix`,
and you don't care which. This function helps in those situations.

## Usage

``` r
is_matrix_or_Matrix(a)
```

## Arguments

- a:

  The object about which we want to know if it is a `matrix` or a
  `Matrix`.

## Value

`TRUE` when `a` is a `matrix` or a `Matrix`. `FALSE` otherwise.

## Examples

``` r
is_matrix_or_Matrix(42)
#> [1] FALSE
is_matrix_or_Matrix(matrix(42))
#> [1] TRUE
is_matrix_or_Matrix(Matrix::Matrix(42))
#> [1] TRUE
is_matrix_or_Matrix(matsbyname::Matrix(42))
#> [1] TRUE
```
