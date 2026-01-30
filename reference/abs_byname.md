# Absolute value of matrix elements

Absolute value of matrix elements

## Usage

``` r
abs_byname(a)
```

## Arguments

- a:

  A matrix or list of matrices.

## Value

`a` with each element replaced by its absolute value.

## Examples

``` r
abs_byname(1)
#> [1] 1
abs_byname(-1)
#> [1] 1
m <- matrix(c(-10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>     c1  c2
#> i1 -10   1
#> i2   1 100
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
abs_byname(m)
#>    c1  c2
#> i1 10   1
#> i2  1 100
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
```
