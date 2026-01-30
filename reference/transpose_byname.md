# Transpose a matrix by name

Gives the transpose of a matrix or list of matrices.

## Usage

``` r
transpose_byname(a)
```

## Arguments

- a:

  The matrix to be transposed.

## Value

The transposed matrix.

## Examples

``` r
m <- matrix(c(11,21,31,12,22,32), ncol = 2, dimnames = list(paste0("i", 1:3), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>    c1 c2
#> i1 11 12
#> i2 21 22
#> i3 31 32
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
transpose_byname(m)
#>    i1 i2 i3
#> c1 11 21 31
#> c2 12 22 32
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
transpose_byname(list(m,m))
#> [[1]]
#>    i1 i2 i3
#> c1 11 21 31
#> c2 12 22 32
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>    i1 i2 i3
#> c1 11 21 31
#> c2 12 22 32
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
#> 
```
