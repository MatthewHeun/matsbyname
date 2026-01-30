# Name-wise matrix element division

Element-wise division of two matrices.

## Usage

``` r
quotient_byname(dividend, divisor)
```

## Arguments

- dividend:

  Dividend matrix or constant

- divisor:

  Divisor matrix or constant

## Value

A matrix representing the name-wise element quotient of `dividend` and
`divisor`

## Details

Performs a union and sorting of names of rows and columns for both
`dividend` and `divisor` prior to element division. Zeroes are inserted
for missing matrix elements. Doing so ensures that the dimensions of the
`dividend` and `divisor` will be conformable.

## Examples

``` r
library(dplyr)
quotient_byname(100, 50)
#> [1] 2
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
U / G # Non-sensical.  Names aren't aligned
#>           i1  i2
#> c1 0.2500000 1.5
#> c2 0.6666667 4.0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
quotient_byname(U, G)
#>    i1 i2
#> c1  1  1
#> c2  1  1
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
quotient_byname(U, 10)
#>     i1  i2
#> c1 0.1 0.3
#> c2 0.2 0.4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
quotient_byname(10, G)
#>    i1       i2
#> c1 10 3.333333
#> c2  5 2.500000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# This also works with lists
quotient_byname(10, list(G,G))
#> [[1]]
#>    i1       i2
#> c1 10 3.333333
#> c2  5 2.500000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1       i2
#> c1 10 3.333333
#> c2  5 2.500000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
quotient_byname(list(G,G), 10)
#> [[1]]
#>     i1  i2
#> c1 0.1 0.3
#> c2 0.2 0.4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>     i1  i2
#> c1 0.1 0.3
#> c2 0.2 0.4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
quotient_byname(list(U, U), list(G, G))
#> [[1]]
#>    i1 i2
#> c1  1  1
#> c2  1  1
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  1
#> c2  1  1
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF <- data.frame(U = I(list()), G = I(list()))
DF[[1,"U"]] <- U
DF[[2,"U"]] <- U
DF[[1,"G"]] <- G
DF[[2,"G"]] <- G
quotient_byname(DF$U, DF$G)
#> [[1]]
#>    i1 i2
#> c1  1  1
#> c2  1  1
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  1
#> c2  1  1
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(elementquotients = quotient_byname(U, G))
#>            U          G elementquotients
#> 1 1, 2, 3, 4 4, 3, 2, 1       1, 1, 1, 1
#> 2 1, 2, 3, 4 4, 3, 2, 1       1, 1, 1, 1
```
