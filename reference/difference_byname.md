# Name-wise subtraction of matrices

Name-wise subtraction of matrices

## Usage

``` r
difference_byname(minuend, subtrahend)
```

## Arguments

- minuend:

  matrix or constant

- subtrahend:

  matrix or constant

  Performs a union and sorting of row and column names prior to
  differencing. Zeroes are inserted for missing matrix elements.

## Value

A matrix representing the name-wise difference between `minuend` and
`subtrahend`

## Examples

``` r
library(dplyr)
difference_byname(100, 50)
#> [1] 50
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
U - G # Non-sensical. Row and column names not respected.
#>    i1 i2
#> c1 -3  1
#> c2 -1  3
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
difference_byname(U, G) # Row and column names respected! Should be all zeroes.
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
difference_byname(100, U)
#>    i1 i2
#> c1 99 97
#> c2 98 96
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
difference_byname(10, G)
#>    i1 i2
#> c1  9  7
#> c2  8  6
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
difference_byname(G) # When subtrahend is missing, return minuend (in this case, G).
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
difference_byname(subtrahend = G) # When minuend is missing, return - subtrahend (in this case, -G)
#>    i1 i2
#> c1 -1 -3
#> c2 -2 -4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# This also works with lists
difference_byname(list(100, 100), list(50, 50))
#> [[1]]
#> [1] 50
#> 
#> [[2]]
#> [1] 50
#> 
difference_byname(list(U,U), list(G,G))
#> [[1]]
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  0  0
#> c2  0  0
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
difference_byname(DF$U, DF$G)
#> [[1]]
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(diffs = difference_byname(U, G))
#>            U          G      diffs
#> 1 1, 2, 3, 4 4, 3, 2, 1 0, 0, 0, 0
#> 2 1, 2, 3, 4 4, 3, 2, 1 0, 0, 0, 0
```
