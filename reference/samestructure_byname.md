# Test whether matrices or lists of matrices have same structure

Matrices are said to have the same structure if row and column types are
identical and if row and column names are identical. Values can be
different.

## Usage

``` r
samestructure_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands to be compared.

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`).

## Value

`TRUE` if all operands have the same structure, `FALSE` otherwise.

## Examples

``` r
samestructure_byname(2, 2)
#> [1] TRUE
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
  setrowtype("Products") %>% setcoltype("Industries")
samestructure_byname(U, U)
#> [1] TRUE
samestructure_byname(U, U %>% setrowtype("row"))
#> [1] FALSE
samestructure_byname(U %>% setcoltype("col"), U)
#> [1] FALSE
# Also works with lists
samestructure_byname(list(U, U), list(U, U))
#> [[1]]
#> [1] TRUE
#> 
#> [[2]]
#> [1] TRUE
#> 
```
