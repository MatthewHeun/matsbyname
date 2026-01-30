# Sets column type for a matrix or a list of matrices

This function is a wrapper for
[`attr()`](https://rdrr.io/r/base/attr.html) so that setting can be
accomplished by the pipe operator (`%>%`). Column types are strings
stored in the `coltype` attribute.

## Usage

``` r
setcoltype(a, coltype)
```

## Arguments

- a:

  The matrix on which column type is to be set.

- coltype:

  The type of item stored in columns.

## Value

`a` with `coltype` attribute set.

## Details

If `is.null(coltype)`, the coltype attribute is deleted and subsequent
calls to `coltype` will return `NULL`.

## Examples

``` r
library(dplyr)
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames))
U %>% setcoltype("Industries")
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
# This also works for lists
setcoltype(list(U,U), coltype = "Industries")
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
setcoltype(list(U,U), coltype = list("Industries", "Industries"))
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF <- data.frame(U = I(list()))
DF[[1,"U"]] <- U
DF[[2,"U"]] <- U
setcoltype(DF$U, "Industries")
#> [[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF <- DF %>% mutate(newcol = setcoltype(U, "Industries"))
DF$newcol[[1]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
DF$newcol[[2]]
#>    i1 i2
#> c1  1  3
#> c2  2  4
#> attr(,"coltype")
#> [1] "Industries"
```
