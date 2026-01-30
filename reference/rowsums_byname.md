# Row sums, sorted by name

Calculates row sums for a matrix by post-multiplying by an identity
vector (containing all 1's). In contrast to `rowSums` (which returns a
`numeric` result), the return value from `rowsums_byname` is a matrix.
An optional `colname` for the resulting column vector can be supplied.
If `colname` is `NULL` or `NA` (the default), the column name is set to
the column type as given by `coltype(a)`. If `colname` is set to `NULL`,
the column name is returned empty.

## Usage

``` r
rowsums_byname(a, colname = NA)
```

## Arguments

- a:

  A matrix or list of matrices from which row sums are desired.

- colname:

  The name of the output column containing row sums.

## Value

A column vector of type `matrix` containing the row sums of `m`.

## Examples

``` r
library(dplyr)
rowsums_byname(42)
#> [1] 42
m <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("c", 1:2))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
m
#>    c1 c2
#> i3  1  4
#> i2  2  5
#> i1  3  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
rowsums_byname(m)
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
rowsums_byname(m, "E.ktoe")
#>    E.ktoe
#> i1      9
#> i2      7
#> i3      5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists
rowsums_byname(list(m, m))
#> [[1]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
rowsums_byname(list(m, m), "E.ktoe")
#> [[1]]
#>    E.ktoe
#> i1      9
#> i2      7
#> i3      5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    E.ktoe
#> i1      9
#> i2      7
#> i3      5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
rowsums_byname(list(m, m), NA)
#> [[1]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
rowsums_byname(list(m, m), NULL)
#> [[1]]
#>    [,1]
#> i1    9
#> i2    7
#> i3    5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    [,1]
#> i1    9
#> i2    7
#> i3    5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
rowsums_byname(DF$m[[1]])
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
rowsums_byname(DF$m)
#> [[1]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
ans <- DF %>% mutate(rs = rowsums_byname(m))
ans
#>              m      rs
#> 1 1, 2, 3,.... 9, 7, 5
#> 2 1, 2, 3,.... 9, 7, 5
ans$rs[[1]]
#>    Commodities
#> i1           9
#> i2           7
#> i3           5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# Nonsensical
if (FALSE) rowsums_byname(NULL) # \dontrun{}
```
