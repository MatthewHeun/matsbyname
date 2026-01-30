# Name- and element-wise arithmetic mean of matrices

Gives the arithmetic mean of operands in `...`.

## Usage

``` r
mean_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands: constants, matrices, or lists of matrices.

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`).

## Value

name-wise arithmetic mean of operands.

## Details

This function performs a union and sorting of row and column names prior
to performing arithmetic mean. Zeroes are inserted for missing matrix
elements.

## Examples

``` r
library(dplyr)
mean_byname(100, 50)
#> [1] 75
mean_byname(10, 20, 30)
#> [1] 20
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
G <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
(U + G) / 2 # Non-sensical. Row and column names not respected.
#>     i1  i2
#> c1 2.5 2.5
#> c2 2.5 2.5
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mean_byname(U, G) # Row and column names respected! Should be 1, 2, 3, and 4. 
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mean_byname(U, G, G)
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mean_byname(100, U)
#>      i1   i2
#> c1 50.5 51.5
#> c2 51.0 52.0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mean_byname(100, 50, U)
#>          i1       i2
#> c1 50.33333 51.00000
#> c2 50.66667 51.33333
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
mean_byname(10, G)
#>     i1  i2
#> c1 5.5 6.5
#> c2 6.0 7.0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# This also works with lists
mean_byname(list(100, 100), list(50, 50))
#> [[1]]
#> [1] 75
#> 
#> [[2]]
#> [1] 75
#> 
mean_byname(list(U,U), list(G,G))
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
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
mean_byname(DF$U, DF$G)
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(means = mean_byname(U, G))
#>            U          G      means
#> 1 1, 2, 3, 4 4, 3, 2, 1 1, 2, 3, 4
#> 2 1, 2, 3, 4 4, 3, 2, 1 1, 2, 3, 4
```
