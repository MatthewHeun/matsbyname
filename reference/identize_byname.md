# Named identity matrix or vector

Creates an identity matrix (**I**) or vector (**i**) of same size and
with same names and same row and column types as `a`.

## Usage

``` r
identize_byname(a, margin = c(1, 2))
```

## Arguments

- a:

  the matrix whose names and dimensions are to be preserved in an
  identity matrix or vector

- margin:

  determines whether an identity vector or matrix is returned. See
  details.

## Value

An identity matrix or vector.

## Details

Behaviour for different values of `margin` are as follows:

- If `margin = 1`, makes a column matrix filled with `1`s. Row names and
  type are taken from row names and type of `a`. Column name and type
  are same as column type of `a`.

- If `margin = 2`, make a row matrix filled with `1`s. Column names and
  type are taken from column name and type of `a`. Row name and type are
  same as row type of `a`.

- If `list(c(1,2))` (the default), make an identity matrix with `1`s on
  the diagonal. Row and column names are sorted on output.

## Examples

``` r
M <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("c", 1:4))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
identize_byname(M)
#>    c1 c2 c3 c4
#> i1  1  0  0  0
#> i2  0  1  0  0
#> i3  0  0  1  0
#> i4  0  0  0  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
identize_byname(M, margin = c(1,2))
#>    c1 c2 c3 c4
#> i1  1  0  0  0
#> i2  0  1  0  0
#> i3  0  0  1  0
#> i4  0  0  0  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
identize_byname(M, margin = 1)
#>    Commodities
#> i1           1
#> i2           1
#> i3           1
#> i4           1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
identize_byname(M, margin = 2)
#>            c1 c2 c3 c4
#> Industries  1  1  1  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
N <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
identize_byname(N)
#>   b a
#> b 1 0
#> a 0 1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists
identize_byname(list(M, M))
#> [[1]]
#>    Commodities
#> i1           1
#> i2           1
#> i3           1
#> i4           1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>            c1 c2 c3 c4
#> Industries  1  1  1  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
```
