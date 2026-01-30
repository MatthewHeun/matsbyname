# Replace `NaN` values with a value

In a matrix or within matrices in a list, replace all `NaN` matrix
values with `val.`

## Usage

``` r
replaceNaN_byname(a, val = 0)
```

## Arguments

- a:

  A matrix of list of matrices in which `NaN` will be replaced by `val`.

- val:

  `NaN`s are replace by `val.`

## Value

A matrix or list of matrices in which all `NaN` are replaced by `val`.

## Examples

``` r
suppressWarnings(a <- matrix(c(1, sqrt(-1))))
replaceNaN_byname(a)
#>      [,1]
#> [1,]    1
#> [2,]    0
replaceNaN_byname(a, 42)
#>      [,1]
#> [1,]    1
#> [2,]   42
```
