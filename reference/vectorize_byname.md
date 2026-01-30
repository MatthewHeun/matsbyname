# Vectorize a matrix

Converts a matrix into a column vector. Each element of the matrix
becomes an entry in the column vector, with rows named via the
`notation` argument. Callers may want to transpose the matrix first with
[`transpose_byname()`](https://matthewheun.github.io/matsbyname/reference/transpose_byname.md).

## Usage

``` r
vectorize_byname(a, notation)
```

## Arguments

- a:

  The matrix to be vectorized.

- notation:

  A string vector created by `notation_vec()`.

## Value

A column vector containing all elements of `a`, with row names assigned
as "rowname `sep` colname".

## Details

The `notation` is also applied to `rowtype` and `coltype` attributes.

## Examples

``` r
m <- matrix(c(1, 5,
              4, 5),
            nrow = 2, ncol = 2, byrow = TRUE, 
            dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>% 
  setrowtype("Products") %>% setcoltype("Industries")
m
#>    i1 i2
#> p1  1  5
#> p2  4  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
vectorize_byname(m, notation = RCLabels::arrow_notation)
#>          [,1]
#> p1 -> i1    1
#> p2 -> i1    4
#> p1 -> i2    5
#> p2 -> i2    5
#> attr(,"rowtype")
#> [1] "Products -> Industries"
# If a single number is provided, the number will be returned as a 1x1 column vector 
# with some additional attributes.
vectorize_byname(42, notation = RCLabels::arrow_notation)
#>      [,1]
#> [1,]   42
attributes(vectorize_byname(42, notation = RCLabels::arrow_notation))
#> $dim
#> [1] 1 1
#> 
```
