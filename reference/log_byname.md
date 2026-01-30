# Logarithm of matrix elements

Specify the base of the log with `base` argument.

## Usage

``` r
log_byname(a, base = exp(1))
```

## Arguments

- a:

  A matrix or list of matrices.

- base:

  The base of the logarithm (default is `exp(1)`, giving the natural
  logarithm).

## Value

M with each element replaced by its base `base` logarithm

## Examples

``` r
log_byname(exp(1))
#> [1] 1
m <- matrix(c(10,1,1,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>    c1  c2
#> i1 10   1
#> i2  1 100
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
log_byname(m)
#>          c1      c2
#> i1 2.302585 0.00000
#> i2 0.000000 4.60517
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
log_byname(m, base = 10)
#>    c1 c2
#> i1  1  0
#> i2  0  2
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
```
