# Sets row type for a matrix or a list of matrices

This function is a wrapper for
[`attr()`](https://rdrr.io/r/base/attr.html) so that setting can be
accomplished by the pipe operator (`%>%`). Row types are strings stored
in the `rowtype` attribute.

## Usage

``` r
setrowtype(a, rowtype)
```

## Arguments

- a:

  The matrix on which row type is to be set.

- rowtype:

  The type of item stored in rows.

## Value

`a` with rowtype attribute set to `rowtype.`

## Details

If `is.null(rowtype)`, the rowtype attribute is deleted and subsequent
calls to `rowtype` will return `NULL`.

## Examples

``` r
library(dplyr)
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
U %>% setrowtype("Commodities")
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
# This also works for lists
setrowtype(list(U,U), rowtype = "Commodities")
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
setrowtype(list(U,U), rowtype = list("Commodities", "Commodities"))
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
DF <- data.frame(U = I(list()))
DF[[1,"U"]] <- U
DF[[2,"U"]] <- U
setrowtype(DF$U, "Commodities")
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
#> 
DF <- DF %>% mutate(newcol = setrowtype(U, "Commodities"))
DF$newcol[[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
DF$newcol[[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"rowtype")
#> [1] "Commodities"
```
