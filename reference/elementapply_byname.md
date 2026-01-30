# Apply a function to an element of a matrix specified by rows and columns

`FUN` is applied to the element of `a` that is specified by `row` and
`col`.

## Usage

``` r
elementapply_byname(FUN, a, row, col, .FUNdots = NULL)
```

## Arguments

- FUN:

  a unary function to be applied to specified rows and columns of `a`

- a:

  the argument to `FUN`

- row:

  the row name of the element to which `FUN` will be applied

- col:

  the column name of the element to which `FUN` will be applied

- .FUNdots:

  a list of additional arguments to `FUN`. (Default is `NULL`.)

## Value

`a`, after `FUN` has been applied to the element at `row` and `col`

## Details

`row` and `col` can be any of row or column names or integer indices or
a mix of both.

## Examples

``` r
divide <- function(x, divisor){
  x/divisor
}
m <- matrix(c(1:4), nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
  setrowtype("row") %>% setcoltype("col")
elementapply_byname(divide, a = m, row = 1, col = 1, .FUNdots = list(divisor = 2))
#>     c1 c2
#> r1 0.5  3
#> r2 2.0  4
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
elementapply_byname(divide, a = m, row = 1, col = 2, .FUNdots = list(divisor = 10))
#>    c1  c2
#> r1  1 0.3
#> r2  2 4.0
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
elementapply_byname(divide, a = m, row = "r2", col = "c2", .FUNdots = list(divisor = 100))
#>    c1   c2
#> r1  1 3.00
#> r2  2 0.04
#> attr(,"rowtype")
#> [1] "row"
#> attr(,"coltype")
#> [1] "col"
```
