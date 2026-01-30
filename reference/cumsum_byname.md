# Cumulative sum that respects row and column names

Provides cumulative sums along a list or column of a data frame. If `a`
is a single number, `a` is returned. If `a` is a list of numbers, a list
representing the cumulative sum of the numbers is returned. If `a` is a
single matrix, `a` is returned. If `a` is a list of matrices, a list
representing the cumulative sum of the matrices is returned. In this
case, each entry in the returned list is sum "by name," such that row
and column names of the matrices are respected.

## Usage

``` r
cumsum_byname(a)
```

## Arguments

- a:

  A number, list of numbers, matrix or list of matrices for which
  cumulative sum is desired.

## Value

A single number, list of numbers, a single matrix, or a list of
matrices, depending on the nature of `a`.

## Details

If cumulative sums are desired in the context of a data frame, groups in
the data frame are respected if `mutate` is used. See examples.

## Examples

``` r
library(dplyr)
library(tibble)
m1 <- matrix(c(1), nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
  setrowtype("row") %>% setcoltype("col")
m2 <- matrix(c(2), nrow = 1, ncol = 1, dimnames = list("r2", "c2")) %>% 
  setrowtype("row") %>% setcoltype("col")
m3 <- matrix(c(3), nrow = 1, ncol = 1, dimnames = list("r3", "c3")) %>% 
  setrowtype("row") %>% setcoltype("col")
cumsum_byname(list(m1, m2, m3))
#> [[1]]
#>    c1
#> r1  1
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[2]]
#>    c1 c2
#> r1  1  0
#> r2  0  2
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
#> [[3]]
#>    c1 c2 c3
#> r1  1  0  0
#> r2  0  2  0
#> r3  0  0  3
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
#> 
# Groups are respected in the context of mutate.
tibble(grp = c("A", "A", "B"), m = list(m1, m2, m3)) %>% group_by(grp) %>% 
  mutate(m2 = cumsum_byname(m))
#> # A tibble: 3 × 3
#> # Groups:   grp [2]
#>   grp   m             m2           
#>   <chr> <list>        <list>       
#> 1 A     <dbl [1 × 1]> <dbl [1 × 1]>
#> 2 A     <dbl [1 × 1]> <dbl [2 × 2]>
#> 3 B     <dbl [1 × 1]> <dbl [1 × 1]>
```
