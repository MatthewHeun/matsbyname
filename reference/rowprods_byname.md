# Row products, sorted by name

Calculates row products (the product of all elements in a row) for a
matrix. An optional `colname` for the resulting column vector can be
supplied. If `colname` is `NULL` or `NA` (the default), the column name
is set to the column type as given by `coltype(a)`.

## Usage

``` r
rowprods_byname(a, colname = NA)
```

## Arguments

- a:

  A matrix or list of matrices from which row products are desired.

- colname:

  The Name of the output column containing row products.

## Value

A column vector of type `matrix` containing the row products of `a`

## Examples

``` r
library(dplyr)
M <- matrix(c(1:6), ncol = 2, dimnames = list(paste0("i", 3:1), paste0("c", 1:2))) %>%
  setrowtype("Industries") %>% setcoltype("Products")
rowprods_byname(M)
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
rowprods_byname(M, "E.ktoe")
#>    E.ktoe
#> i1     18
#> i2     10
#> i3      4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
# This also works with lists
rowprods_byname(list(M, M))
#> [[1]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
rowprods_byname(list(M, M), "E.ktoe")
#> [[1]]
#>    E.ktoe
#> i1     18
#> i2     10
#> i3      4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    E.ktoe
#> i1     18
#> i2     10
#> i3      4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
rowprods_byname(list(M, M), NA)
#> [[1]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
rowprods_byname(list(M, M), NULL)
#> [[1]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
DF <- data.frame(M = I(list()))
DF[[1,"M"]] <- M
DF[[2,"M"]] <- M
rowprods_byname(DF$M[[1]])
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
rowprods_byname(DF$M)
#> [[1]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
ans <- DF %>% mutate(rs = rowprods_byname(M))
ans
#>              M        rs
#> 1 1, 2, 3,.... 18, 10, 4
#> 2 1, 2, 3,.... 18, 10, 4
ans$rs[[1]]
#>    Products
#> i1       18
#> i2       10
#> i3        4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
# Nonsensical
if (FALSE) rowprods_byname(NULL) # \dontrun{}
```
