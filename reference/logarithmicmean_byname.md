# Name- and element-wise logarithmic mean of matrices

The logarithmic mean of corresponding entries of `a` and `b` is `0` if
`a = 0` or `b = 0`, `a` if `a = b`, or `(b - a) / (log(b) - log(a))`
otherwise.

## Usage

``` r
logarithmicmean_byname(a, b, base = exp(1))
```

## Arguments

- a:

  first operand (a matrix or constant value or lists of same).

- b:

  second operand (a matrix or constant value or lists of same).

- base:

  the base of the logarithm used when computing the logarithmic mean.
  (Default is `base = exp(1)`.)

## Value

A matrix representing the name-wise logarithmic mean of `a` and `b`.

## Details

This function performs a union and sorting of row and column names prior
to performing logarithmic mean. Zeroes are inserted for missing matrix
elements.

Internally, the third condition is implemented as `(b - a) / log(b/a)`.

Note that `(b - a) / log(b/a) = (a - b) / log(a/b)`, so logarithmic mean
is commutative; the order of arguments **`a`** and **`b`** does not
change the result.

## Examples

``` r
library(dplyr)
m1 <- matrix(c(1:6), nrow = 3, ncol = 2) %>% 
  setrownames_byname(c("r1", "r2", "r3")) %>% setcolnames_byname(c("c1", "c2")) %>% 
  setrowtype("row") %>% setcoltype("col")
m2 <- matrix(c(7:12), nrow = 3, ncol = 2) %>% 
  setrownames_byname(c("r2", "r3", "r4")) %>% setcolnames_byname(c("c2", "c3")) %>% 
  setrowtype("row") %>% setcoltype("col")
logarithmicmean_byname(m1, m2)
#>    c1       c2 c3
#> r1  0 0.000000  0
#> r2  0 5.944027  0
#> r3  0 6.952119  0
#> r4  0 0.000000  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
# This also works with lists
logarithmicmean_byname(list(m1, m1), list(m2, m2))
#> [[1]]
#>    c1       c2 c3
#> r1  0 0.000000  0
#> r2  0 5.944027  0
#> r3  0 6.952119  0
#> r4  0 0.000000  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[2]]
#>    c1       c2 c3
#> r1  0 0.000000  0
#> r2  0 5.944027  0
#> r3  0 6.952119  0
#> r4  0 0.000000  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
DF <- data.frame(m1 = I(list()), m2 = I(list()))
DF[[1,"m1"]] <- m1
DF[[2,"m1"]] <- m1
DF[[1,"m2"]] <- m2
DF[[2,"m2"]] <- m2
logarithmicmean_byname(DF$m1, DF$m2)
#> [[1]]
#>    c1       c2 c3
#> r1  0 0.000000  0
#> r2  0 5.944027  0
#> r3  0 6.952119  0
#> r4  0 0.000000  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[2]]
#>    c1       c2 c3
#> r1  0 0.000000  0
#> r2  0 5.944027  0
#> r3  0 6.952119  0
#> r4  0 0.000000  0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
DF %>% mutate(logmeans = logarithmicmean_byname(m1, m2))
#>             m1           m2
#> 1 1, 2, 3,.... 7, 8, 9,....
#> 2 1, 2, 3,.... 7, 8, 9,....
#>                                                                                                                 logmeans
#> 1 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 5.944027, 6.952119, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000
#> 2 0.000000, 0.000000, 0.000000, 0.000000, 0.000000, 5.944027, 6.952119, 0.000000, 0.000000, 0.000000, 0.000000, 0.000000
```
