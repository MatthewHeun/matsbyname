# Names of zero rows and columns

When a matrix has rows or columns full of zeroes, it is singular, and
can't be inverted. This function returns the names of rows or columns
that are full with zeroes.

## Usage

``` r
getzerorowcolnames_byname(a, tol = 1e-06)
```

## Arguments

- a:

  A matrix or list of matrices.

- tol:

  The allowable deviation from 0 for any element.

## Value

A vector of names of zero rows or columns.

## Examples

``` r
m <- matrix(c(1, 0, 1,
              1, 0, 0, 
              0, 0, 0),
            dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")), 
            nrow = 3, ncol = 3, byrow = TRUE)
m
#>    c1 c2 c3
#> r1  1  0  1
#> r2  1  0  0
#> r3  0  0  0
getzerorowcolnames_byname(m)
#> [1] "r3" "c2"
```
