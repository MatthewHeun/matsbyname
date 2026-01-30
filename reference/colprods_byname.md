# Column products, sorted by name

Calculates column products (the product of all elements in a column) for
a matrix. An optional `rowname` for the resulting row vector can be
supplied. If `rowname` is `NULL` or `NA` (the default), the row name is
set to the row type as given by `rowtype(a)`.

## Usage

``` r
colprods_byname(a, rowname = NA)
```

## Arguments

- a:

  A matrix or data frame from which column products are desired.

- rowname:

  The Name of the output row containing column products.

## Value

a row vector of type `matrix` containing the column products of `a`.

## Examples

``` r
library(dplyr)
M <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 3:1))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
colprods_byname(M)
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
colprods_byname(M, rowname = "E.ktoe")
#>        c1 c2 c3
#> E.ktoe 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
M %>% colprods_byname %>% rowprods_byname
#>            Commodities
#> Industries         720
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists
colprods_byname(list(M, M))
#> [[1]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colprods_byname(list(M, M), rowname = "E.ktoe")
#> [[1]]
#>        c1 c2 c3
#> E.ktoe 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>        c1 c2 c3
#> E.ktoe 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colprods_byname(list(M, M), rowname = NA)
#> [[1]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colprods_byname(list(M, M), rowname = NULL)
#> [[1]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
DF <- data.frame(M = I(list()))
DF[[1,"M"]] <- M
DF[[2,"M"]] <- M
colprods_byname(DF$M[[1]])
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
colprods_byname(DF$M)
#> [[1]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3
#> Industries 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
colprods_byname(DF$M, "prods")
#> [[1]]
#>       c1 c2 c3
#> prods 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>       c1 c2 c3
#> prods 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
res <- DF %>% mutate(
  cs = colprods_byname(M),
  cs2 = colprods_byname(M, rowname = "prod")
)
res$cs2
#> [[1]]
#>      c1 c2 c3
#> prod 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>      c1 c2 c3
#> prod 30 12  2
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
```
