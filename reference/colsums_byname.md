# Column sums, sorted by name

Calculates column sums for a matrix by premultiplying by an identity
vector (containing all 1's). In contrast to `colSums` (which returns a
`numeric` result), the return value from `colsums_byname` is a matrix.
An optional `rowname` for the resulting row vector can be supplied. If
`rowname` is `NA` (the default), the row name is set to the row type as
given by `rowtype(a)`. If `rowname` is set to `NULL`, the row name is
returned empty.

## Usage

``` r
colsums_byname(a, rowname = NA)
```

## Arguments

- a:

  A matrix or list of matrices from which column sums are desired.

- rowname:

  The name of the output row containing column sums.

## Value

A row vector of type `matrix` containing the column sums of `a`.

## Examples

``` r
library(dplyr)
colsums_byname(42)
#> [1] 42
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 3:1))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
m
#>    c3 c2 c1
#> i1  1  3  5
#> i2  2  4  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
colsums_byname(m)
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
colsums_byname(m, rowname = "E.ktoe")
#>        c1 c2 c3
#> E.ktoe 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
m %>% 
  colsums_byname() %>% 
  rowsums_byname()
#>            Commodities
#> Industries          21
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists
colsums_byname(list(m, m))
#> [[1]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colsums_byname(list(m, m), rowname = "E.ktoe")
#> [[1]]
#>        c1 c2 c3
#> E.ktoe 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>        c1 c2 c3
#> E.ktoe 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colsums_byname(list(m, m), rowname = NA)
#> [[1]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colsums_byname(list(m, m), rowname = NULL)
#> [[1]]
#>      c1 c2 c3
#> [1,] 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>      c1 c2 c3
#> [1,] 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
colsums_byname(DF$m[[1]])
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
colsums_byname(DF$m)
#> [[1]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colsums_byname(DF$m, "sums")
#> [[1]]
#>      c1 c2 c3
#> sums 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>      c1 c2 c3
#> sums 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
res <- DF %>% mutate(
  cs = colsums_byname(m),
  cs2 = colsums_byname(m, rowname = "sum")
)
res$cs2
#> [[1]]
#>     c1 c2 c3
#> sum 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>     c1 c2 c3
#> sum 11  7  3
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
```
