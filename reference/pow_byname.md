# Powers of matrix elements

Gives the result of raising all elements of a matrix or list of matrices
to a power.

## Usage

``` r
pow_byname(a, pow)
```

## Arguments

- a:

  a matrix of list of matrices

- pow:

  the power to which elements of `a` will be raised

## Value

`a` with each element raised to `pow`

## Examples

``` r
library(dplyr)
pow_byname(2, 3)
#> [1] 8
m <- matrix(2, nrow = 2, ncol = 3, dimnames = list(paste0("r", 1:2), paste0("c", 1:3))) %>% 
  setrowtype("rows") %>% setcoltype("cols")
pow_byname(m, 2)
#>    c1 c2 c3
#> r1  4  4  4
#> r2  4  4  4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
DF <- data.frame(m = I(list()), pow = I(list()))
DF[[1, "m"]] <- m
DF[[2, "m"]] <- m
DF[[1, "pow"]] <- 0.5
DF[[2, "pow"]] <- -1
DF %>% mutate(
  sqrtm = pow_byname(m, 0.5),
  mtopow = pow_byname(m, pow)
)
#>              m pow                                                      sqrtm
#> 1 2, 2, 2,.... 0.5 1.414214, 1.414214, 1.414214, 1.414214, 1.414214, 1.414214
#> 2 2, 2, 2,....  -1 1.414214, 1.414214, 1.414214, 1.414214, 1.414214, 1.414214
#>                                                       mtopow
#> 1 1.414214, 1.414214, 1.414214, 1.414214, 1.414214, 1.414214
#> 2                               0.5, 0.5, 0.5, 0.5, 0.5, 0.5
```
