# Compute fractions of matrix entries

This function divides all entries in `a` by the specified sum, thereby
"fractionizing" the matrix.

## Usage

``` r
fractionize_byname(a, margin, inf_becomes = .Machine$double.xmax)
```

## Arguments

- a:

  The matrix to be fractionized.

- margin:

  If `1` (rows), each entry in `a` is divided by its row's sum. If `2`
  (columns), each entry in `a` is divided by its column's sum. If
  `c(1,2)` (both rows and columns), each entry in `a` is divided by the
  sum of all entries in `a`.

- inf_becomes:

  A value to be substitute for any `Inf` produced by division. Default
  is `.Machine$double.xmax`. Another reasonable value is `Inf`. Set to
  `NULL` to disable substitution. `inf_becomes` is passed to
  [`hatinv_byname()`](https://matthewheun.github.io/matsbyname/reference/hatinv_byname.md).

## Value

A fractionized matrix of same dimensions and same row and column types
as `a`.

## Examples

``` r
M <- matrix(c(1, 5,
              4, 5),
            nrow = 2, ncol = 2, byrow = TRUE, 
            dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
            setcoltype("Products") %>% setrowtype("Industries")
fractionize_byname(M, margin = c(1,2))
#>            i1        i2
#> p1 0.06666667 0.3333333
#> p2 0.26666667 0.3333333
#> attr(,"coltype")
#> [1] "Products"
#> attr(,"rowtype")
#> [1] "Industries"
fractionize_byname(M, margin = 1)
#>           i1        i2
#> p1 0.1666667 0.8333333
#> p2 0.4444444 0.5555556
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
fractionize_byname(M, margin = 2)
#>     i1  i2
#> p1 0.2 0.5
#> p2 0.8 0.5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
```
