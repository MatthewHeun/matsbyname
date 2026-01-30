# Exponential of matrix elements

Gives the exponential of all elements of a matrix or list of matrices

## Usage

``` r
exp_byname(a)
```

## Arguments

- a:

  a matrix of list of matrices

## Value

`M` with each element replaced by its exponential

## Examples

``` r
exp_byname(1)
#> [1] 2.718282
m <- matrix(c(log(10),log(1),
              log(1),log(100)), 
              byrow = TRUE, nrow = 2, ncol = 2,
              dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>          c1      c2
#> i1 2.302585 0.00000
#> i2 0.000000 4.60517
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
exp_byname(m)
#>    c1  c2
#> i1 10   1
#> i2  1 100
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
```
