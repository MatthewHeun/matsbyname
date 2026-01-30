# Subtract a matrix with named rows and columns from a suitably named and sized identity matrix (`I`)

The order of rows and columns of `m` may change before subtracting from
`I`, because the rows and columns are sorted by name prior to
subtracting from `I`. Furthermore, if `m` is not square, it will be made
square before subtracting from `I` by calling
[`complete_and_sort()`](https://matthewheun.github.io/matsbyname/reference/complete_and_sort.md).

## Usage

``` r
Iminus_byname(a)
```

## Arguments

- a:

  The matrix to be subtracted from `I`.

## Value

The difference between an identity matrix (`I`) and `m`. (whose rows and
columns have been completed and sorted)

## Examples

``` r
m <- matrix(c(-21, -12, -21, -10), ncol = 2, dimnames = list(c("b", "a"), c("b", "a"))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
# Rows and columns are unsorted
diag(1, nrow = 2) - m 
#>    b  a
#> b 22 21
#> a 12 11
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# Rows and columns are sorted prior to subtracting from the identity matrix
Iminus_byname(m) 
#>    a  b
#> a 11 12
#> b 21 22
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists
Iminus_byname(list(m,m))
#> [[1]]
#>    a  b
#> a 11 12
#> b 21 22
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    a  b
#> a 11 12
#> b 21 22
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
# If the m is not square before subtracting from I,
# it will be made square by the function complete_and_sort.
m2 <- matrix(c(1,2,3,4,5,6), ncol = 2, dimnames = list(c("a", "b", "c"), c("a", "b"))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
Iminus_byname(m2)
#>    a  b c
#> a  0 -4 0
#> b -2 -4 0
#> c -3 -6 1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
```
