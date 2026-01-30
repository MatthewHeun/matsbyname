# Column type

Extracts column type of `a`.

## Usage

``` r
coltype(a)
```

## Arguments

- a:

  The object from which you want to extract column types.

## Value

The column type of `a`.

## Examples

``` r
commoditynames <- c("c1", "c2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
  setrowtype(rowtype = "Commodities") %>% setcoltype("Industries")
coltype(U)
#> [1] "Industries"
# This also works for lists
coltype(list(U,U))
#> [[1]]
#> [1] "Industries"
#> 
#> [[2]]
#> [1] "Industries"
#> 
```
