# Count the number of matrix entries in columns that meet a criterion

Expressions can be written in a natural way such as
`count_vals_incols_byname(m, "<=", 1)`.

## Usage

``` r
count_vals_incols_byname(
  a,
  compare_fun = c("==", "!=", "<", "<=", ">=", ">"),
  val = 0
)
```

## Arguments

- a:

  a matrix or list of matrices whose values are to be counted by columns
  according to `compare_fun`

- compare_fun:

  the comparison function, one of "`==`", "`!=`", "`<`", "`<=`", "`>`",
  or "`>=`". Default is "`==`"

- val:

  the value against which matrix entries are compared. Default is `0`.

## Value

an `matrix` with a single row indicating the number of entries in `a`
that meet the specified criterion in each column of `a`

## Details

Either a single matrix or a list of matrices can be given as the `a`
argument. `compare_fun` can be specified as a string (`"!="`) or as a
back-quoted function (`` `!=` ``).

## Examples

``` r
m <- matrix(c(0, 1, 2, 3, 4, 0), nrow = 3, ncol = 2)
count_vals_incols_byname(m) # uses defaults: compare_fun = "==" and val = 0
#>      [,1] [,2]
#> [1,]    1    1
count_vals_incols_byname(m, compare_fun = "!=")
#>      [,1] [,2]
#> [1,]    2    2
count_vals_incols_byname(m, compare_fun = `!=`)
#>      [,1] [,2]
#> [1,]    2    2
# Write expressions in a natural way
count_vals_incols_byname(m, "<=", 1)
#>      [,1] [,2]
#> [1,]    2    1
# Also works for lists
count_vals_incols_byname(list(m,m), "<=", 1)
#> [[1]]
#>      [,1] [,2]
#> [1,]    2    1
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    2    1
#> 
```
