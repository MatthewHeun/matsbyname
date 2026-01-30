# Name- and element-wise geometric mean of two matrices.

Gives the geometric mean of corresponding entries of `a` and `b`.

## Usage

``` r
geometricmean_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  operands; constants, matrices, or lists of matrices

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`).

## Value

name-wise geometric mean of operands

## Details

This function performs a union and sorting of row and column names prior
to performing geometric mean. Zeroes are inserted for missing matrix
elements.

## Examples

``` r
library(dplyr)
geometricmean_byname(10, 1000)
#> [1] 100
geometricmean_byname(10, 1000, 100000)
#> [1] 1000
commoditynames <- c("c1", "c2")
industrynames <- "i1"
U <- matrix(c(10, 1000), ncol = 1, nrow = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
G <- matrix(c(1e3, 1e5), ncol = 1, nrow = 2, 
            dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
# Non-sensical. Row and column names not respected.
sqrt(U*G)
#>       i1
#> c1   100
#> c2 10000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# Row and column names respected!
geometricmean_byname(U, G)
#>      i1
#> c1 1000
#> c2 1000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
geometricmean_byname(1000, U)
#>      i1
#> c1  100
#> c2 1000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
geometricmean_byname(10, G)
#>      i1
#> c1 1000
#> c2  100
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# This also works with lists
geometricmean_byname(list(10, 1000), list(1000, 10))
#> [[1]]
#> [1] 100
#> 
#> [[2]]
#> [1] 100
#> 
geometricmean_byname(list(U,U), list(G,G))
#> [[1]]
#>      i1
#> c1 1000
#> c2 1000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>      i1
#> c1 1000
#> c2 1000
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
geometricmean_byname(DF$U, DF$G)
#> [[1]]
#>      i1
#> c1 1000
#> c2 1000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>      i1
#> c1 1000
#> c2 1000
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(geomeans = geometricmean_byname(U, G))
#>          U           G   geomeans
#> 1 10, 1000 1000, 1e+05 1000, 1000
#> 2 10, 1000 1000, 1e+05 1000, 1000
```
