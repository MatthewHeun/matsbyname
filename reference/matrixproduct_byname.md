# Name-wise matrix multiplication

Multiplies operands from left to right (when `.summarise = FALSE`). If
`.summarise = TRUE`, operands are multiplied from first to last.

## Usage

``` r
matrixproduct_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands; constants, matrices, or lists of matrices.

- .summarise:

  When `TRUE`, a matrix multiplication proceeds down lists of arguments.
  When `FALSE` (the default), items are multiplied across lists.

## Value

A matrix representing the name-wise product of operands.

## Details

Performs a union and sorting of multiplicand rows and multiplier columns
by name prior to multiplication. Zeroes are inserted for missing matrix
elements. Doing so ensures that the dimensions of multiplicand and
multiplier matrices will be conformable. I.e., the number of columns in
multiplicand will equal the number of rows in multiplier, so long as the
column names of multiplicand are unique and the row names of multiplier
are unique. If column type of the multiplicand is not same as row type
of the multiplier on any step of the multiplication, the function will
fail. The result is matrix product with row names from the first
multiplicand and column names from the last multiplier.

## Examples

``` r
library(dplyr)
V <- matrix(1:6, ncol = 3, dimnames = list(c("i1", "i2"), c("c1", "c2", "c3"))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
G <- matrix(1:4, ncol = 2, dimnames = list(c("c2", "c1"), c("i2", "i1"))) %>%
  setrowtype("Commodities") %>% setcoltype("Industries")
Z <- matrix(11:14, ncol = 2, dimnames = list(c("i1", "i2"), c("s1", "s2"))) %>% 
  setrowtype("Industries") %>% setcoltype("Sectors")
# Succeeds because G is completed to include a row named c3 (that contains zeroes).
matrixproduct_byname(V, G)
#>    i1 i2
#> i1 13  5
#> i2 20  8
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
if (FALSE) V %*% G # \dontrun{} # Fails because E lacks a row named c3.
matrixproduct_byname(V, G, Z)
#>     s1  s2
#> i1 203 239
#> i2 316 372
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Sectors"
# This also works with lists
matrixproduct_byname(list(V,V), list(G,G))
#> [[1]]
#>    i1 i2
#> i1 13  5
#> i2 20  8
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> i1 13  5
#> i2 20  8
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF <- data.frame(V = I(list()), G = I(list()))
DF[[1,"V"]] <- V
DF[[2,"V"]] <- V
DF[[1,"G"]] <- G
DF[[2,"G"]] <- G
matrixproduct_byname(DF$V, DF$G)
#> [[1]]
#>    i1 i2
#> i1 13  5
#> i2 20  8
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> i1 13  5
#> i2 20  8
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(matprods = matrixproduct_byname(V, G))
#>              V          G     matprods
#> 1 1, 2, 3,.... 1, 2, 3, 4 13, 20, 5, 8
#> 2 1, 2, 3,.... 1, 2, 3, 4 13, 20, 5, 8
# Also works with lists, multiplying down the lists if `.summarise = TRUE`.
matrixproduct_byname(list(V, G, Z), .summarise = TRUE)
#> [[1]]
#>     s1  s2
#> i1 203 239
#> i2 316 372
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Sectors"
#> 
```
