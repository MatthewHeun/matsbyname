# Sum of all elements in a matrix

This function is equivalent to
`a \%>\% rowsums_byname() \%>\% colsums_byname()`, but returns a single
numeric value instead of a 1x1 matrix.

## Usage

``` r
sumall_byname(a)
```

## Arguments

- a:

  The matrix whose elements are to be summed.

## Value

The sum of all elements in `a` as a numeric.

## Examples

``` r
library(dplyr)
sumall_byname(42)
#> [1] 42
m <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>    c1 c2
#> i1  2  2
#> i2  2  2
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
sumall_byname(m)
#> [1] 8
rowsums_byname(m) %>% colsums_byname
#>          Commodity
#> Industry         8
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
# Also works for lists
sumall_byname(list(m,m))
#> [[1]]
#> [1] 8
#> 
#> [[2]]
#> [1] 8
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
sumall_byname(DF$m[[1]])
#> [1] 8
sumall_byname(DF$m)
#> [[1]]
#> [1] 8
#> 
#> [[2]]
#> [1] 8
#> 
res <- DF %>% mutate(
  sums = sumall_byname(m)
)
res$sums
#> [[1]]
#> [1] 8
#> 
#> [[2]]
#> [1] 8
#> 
sumall_byname(list(m, NULL))
#> [[1]]
#> [1] 8
#> 
#> [[2]]
#> NULL
#> 
```
