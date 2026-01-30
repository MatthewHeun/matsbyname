# Product of all elements in a matrix

This function is equivalent to
`a \%>\% rowprods_byname() \%>\% colprods_byname()`, but returns a
single numeric value instead of a 1x1 matrix.

## Usage

``` r
prodall_byname(a)
```

## Arguments

- a:

  The matrix whose elements are to be multiplied.

## Value

The product of all elements in `a` as a numeric.

## Examples

``` r
library(dplyr)
M <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Product")
prodall_byname(M)
#> [1] 16
rowprods_byname(M) %>% colprods_byname
#>          Product
#> Industry      16
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
# Also works for lists
prodall_byname(list(M,M))
#> [[1]]
#> [1] 16
#> 
#> [[2]]
#> [1] 16
#> 
DF <- data.frame(M = I(list()))
DF[[1,"M"]] <- M
DF[[2,"M"]] <- M
prodall_byname(DF$M[[1]])
#> [1] 16
prodall_byname(DF$M)
#> [[1]]
#> [1] 16
#> 
#> [[2]]
#> [1] 16
#> 
res <- DF %>% mutate(
  prods = prodall_byname(M)
)
res$prods
#> [[1]]
#> [1] 16
#> 
#> [[2]]
#> [1] 16
#> 
```
