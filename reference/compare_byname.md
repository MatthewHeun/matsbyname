# Compare matrix entries to a value

Compares matrix entries to a value, returning a matrix of same size as
`a` containing `TRUE` or `FALSE` values as the result of applying
`compare_fun` and `val` to all entries in `a`.

## Usage

``` r
compare_byname(a, compare_fun = c("==", "!=", "<", "<=", ">=", ">"), val = 0)
```

## Arguments

- a:

  a matrix or list of matrices whose values are to be counted according
  to `compare_fun`

- compare_fun:

  the comparison function, one of "`==`", "`!=`", "`<`", "`<=`", "`>=`",
  or "`>`". Default is "`==`".

- val:

  a single value against which entries in matrix `a` are compared.
  Default is `0`.

## Value

a logical matrix of same size as `a` containing `TRUE` where the
criterion is met, `FALSE` otherwise

## Examples

``` r
m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
compare_byname(m, "<", 3)
#>      [,1]  [,2]
#> [1,] TRUE FALSE
#> [2,] TRUE FALSE
#> [3,] TRUE  TRUE
compare_byname(list(m,m), "<", 3)
#> [[1]]
#>      [,1]  [,2]
#> [1,] TRUE FALSE
#> [2,] TRUE FALSE
#> [3,] TRUE  TRUE
#> 
#> [[2]]
#>      [,1]  [,2]
#> [1,] TRUE FALSE
#> [2,] TRUE FALSE
#> [3,] TRUE  TRUE
#> 
```
