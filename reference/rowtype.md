# Row type

Extracts row type of `a`.

## Usage

``` r
rowtype(a)
```

## Arguments

- a:

  The object from which you want to extract row types.

## Value

The row type of `a`.

## Examples

``` r
library(dplyr)
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
rowtype(U)
#> [1] "Commodities"
# This also works for lists
rowtype(list(U,U))
#> [[1]]
#> [1] "Commodities"
#> 
#> [[2]]
#> [1] "Commodities"
#> 
```
