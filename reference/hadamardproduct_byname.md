# Name-wise matrix Hadamard multiplication

Performs a union and sorting of names of rows and columns for both
multiplicand and multiplier for each sequential multiplication step.
Zeroes are inserted for missing matrix elements. Doing so ensures that
the dimensions of the multiplicand and multiplier are be conformable for
each sequential multiplication.

## Usage

``` r
hadamardproduct_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands; constants, matrices, or lists of matrices.

- .summarise:

  When `TRUE`, operands are multiplied down lists. When `FALSE` (the
  default), items multiplied across lists.

## Value

Name-wise element product of operands.

## Details

The Hadamard product is also known as the `entrywise` product.

## Examples

``` r
library(dplyr)
hadamardproduct_byname(2, 2)
#> [1] 4
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
G <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
U * G # Not what is desired, because names aren't aligned
#>    i1 i2
#> c1  1  9
#> c2  4 16
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
hadamardproduct_byname(U, G)
#>    i1 i2
#> c1  4  6
#> c2  6  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
hadamardproduct_byname(U, G, G)
#>    i1 i2
#> c1 16 12
#> c2 18  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
hadamardproduct_byname(U, 0)
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
hadamardproduct_byname(0, G)
#>    i1 i2
#> c1  0  0
#> c2  0  0
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
# This also works with lists
hadamardproduct_byname(list(U, U), list(G, G))
#> [[1]]
#>    i1 i2
#> c1  4  6
#> c2  6  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  4  6
#> c2  6  4
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
hadamardproduct_byname(DF$U, DF$G)
#> [[1]]
#>    i1 i2
#> c1  4  6
#> c2  6  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  4  6
#> c2  6  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(entrywiseprods = hadamardproduct_byname(U, G))
#>            U          G entrywiseprods
#> 1 1, 2, 3, 4 1, 2, 3, 4     4, 6, 6, 4
#> 2 1, 2, 3, 4 1, 2, 3, 4     4, 6, 6, 4
# Also works down lists with `.summarise = TRUE`.
hadamardproduct_byname(list(U, G), .summarise = TRUE)
#> [[1]]
#>    i1 i2
#> c1  4  6
#> c2  6  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Industries"
#> 
```
