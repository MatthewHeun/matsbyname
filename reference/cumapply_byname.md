# Apply a function cumulatively to a list of matrices or numbers

`FUN` must be a binary function that also accepts a single argument. The
result is a list with first element `FUN(a[[1]])`. For `i >= 2`,
elements are `FUN(a[[i]], out[[i-1]])`, where `out` is the result list.

## Usage

``` r
cumapply_byname(FUN, a)
```

## Arguments

- FUN:

  the function to be applied

- a:

  the list of matrices or numbers to which `FUN` will be applied
  cumulatively

## Value

a list of same length as `a` containing the cumulative application of
`FUN` to `a`

## Details

[`naryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/naryapply_byname.md)
and `cumapply_byname()` are similar. Their differences can be described
by considering a data frame.
[`naryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/naryapply_byname.md)
applies `FUN` to several columns (variables) of the data frame. For
example,
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
applied to several variables gives another column containing the sums
across each row of the data frame. `cumapply_byname()` applies `FUN` to
successive entries in a single column. For example
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
applied to a single column gives the sum of all numbers in that column.

## Examples

``` r
cumapply_byname(sum, list(1, 2, 3, 4))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 3
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 10
#> 
cumapply_byname(sum_byname, list(1, 2, 3, 4))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 3
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 10
#> 
cumapply_byname(prod, list(1, 2, 3, 4))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 24
#> 
cumapply_byname(hadamardproduct_byname, list(1, 2, 3, 4))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
#> [[3]]
#> [1] 6
#> 
#> [[4]]
#> [1] 24
#> 
```
