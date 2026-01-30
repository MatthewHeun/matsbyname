# Sets row names

Sets row names in a way that is amenable to use in piping operations in
a functional programming way. If `a` is `NULL`, `NULL` is returned. If
`a` is a constant, it is converted to a matrix and `rownames` are
applied. If `a` is a matrix, `rownames` should be a vector of new row
names that is as long as the number of rows in `a`. If `a` is a list of
matrices, `rownames` can also be a list, and it should be as long `a`.
Or `rownames` can be a vector of row names which will be applied to
every matrix in the list of `a`. Each item in the list should be a
vector containing row names for the corresponding matrix in `a`.

## Usage

``` r
setrownames_byname(a, rownames)
```

## Arguments

- a:

  A matrix or a list of matrices in which row names are to be set

- rownames:

  A vector of new row names or a list of vectors of new row names

## Value

a copy of `m` with new row names

## Examples

``` r
library(dplyr)
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
setrownames_byname(m, c("a", "b"))
#>   c1 c2 c3
#> a  1  3  5
#> b  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
setrownames_byname(m %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
#>   c1 c2 c3
#> c  1  3  5
#> d  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
m %>% setrownames_byname(NULL)
#>      c1 c2 c3
#> [1,]  1  3  5
#> [2,]  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
m %>% setrownames_byname(c(NA, NA))
#>      c1 c2 c3
#> <NA>  1  3  5
#> <NA>  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
2 %>% setrownames_byname("row")
#>     [,1]
#> row    2
# This also works for lists
setrownames_byname(list(m,m), list(c("a", "b")))
#> [[1]]
#>   c1 c2 c3
#> a  1  3  5
#> b  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>   c1 c2 c3
#> a  1  3  5
#> b  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
setrownames_byname(DF$m, list(c("r1", "r2")))
#> [[1]]
#>    c1 c2 c3
#> r1  1  3  5
#> r2  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    c1 c2 c3
#> r1  1  3  5
#> r2  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
setrownames_byname(DF$m, list(c("c", "d")))
#> [[1]]
#>   c1 c2 c3
#> c  1  3  5
#> d  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>   c1 c2 c3
#> c  1  3  5
#> d  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
DF <- DF %>% mutate(m = setrownames_byname(m, list(c("r1", "r2"))))
DF$m[[1]]
#>    c1 c2 c3
#> r1  1  3  5
#> r2  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
```
