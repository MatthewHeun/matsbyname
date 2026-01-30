# And "by name"

Operands should be logical, although numerical operands are accepted.
Numerical operands are interpreted as `FALSE` when `0` and `TRUE` for
any other number.

## Usage

``` r
and_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands to the logical `and` function.

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`).

## Value

Logical `and` applied to the operands.

## Examples

``` r
and_byname(TRUE)
#> [1] TRUE
and_byname(FALSE)
#> [1] FALSE
and_byname(list(TRUE, FALSE), list(TRUE, TRUE), list(TRUE, TRUE), list(TRUE, TRUE))
#> [[1]]
#> [1] TRUE
#> 
#> [[2]]
#> [1] FALSE
#> 
m1 <- matrix(c(TRUE, TRUE, TRUE, FALSE), nrow = 2, ncol = 2, 
  dimnames = list(c("r1", "r2"), c("c1", "c2")))
m2 <- matrix(c(TRUE, FALSE, TRUE, TRUE), nrow = 2, ncol = 2,
  dimnames = list(c("r1", "r2"), c("c1", "c2")))
and_byname(m1, m1)
#>      c1    c2
#> r1 TRUE  TRUE
#> r2 TRUE FALSE
and_byname(m1, m2)
#>       c1    c2
#> r1  TRUE  TRUE
#> r2 FALSE FALSE
and_byname(list(m1, m1), list(m1, m1), list(m2, m2))
#> [[1]]
#>       c1    c2
#> r1  TRUE  TRUE
#> r2 FALSE FALSE
#> 
#> [[2]]
#>       c1    c2
#> r1  TRUE  TRUE
#> r2 FALSE FALSE
#> 
and_byname(list(m1, m1), list(m1, m1), list(m2, m2), .summarise = TRUE)
#> [[1]]
#>      c1    c2
#> r1 TRUE  TRUE
#> r2 TRUE FALSE
#> 
#> [[2]]
#>      c1    c2
#> r1 TRUE  TRUE
#> r2 TRUE FALSE
#> 
#> [[3]]
#>       c1   c2
#> r1  TRUE TRUE
#> r2 FALSE TRUE
#> 
```
