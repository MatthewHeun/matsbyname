# Matricize a vector

Converts a vector with rows or columns named according to `notation`
into a `matrix` or a `Matrix`, depending on the type of `a`. Row and
column types of the output are taken from the row or column type of the
long dimension of the incoming vector. If the row or column type of the
long dimension of the incoming vector is `NULL`, the outgoing matrix
will have `NULL` rowtype and `NULL` coltype.

## Usage

``` r
matricize_byname(a, notation)
```

## Arguments

- a:

  A row (column) vector to be converted to a matrix based on its row
  (column) names.

- notation:

  A string vector created by
  [`RCLabels::notation_vec()`](https://matthewheun.github.io/RCLabels/reference/row-col-notation.html)
  that identifies the notation for row or column names.

## Value

A matrix created from vector `a`.

## Examples

``` r
v <- matrix(c(1,
              2,
              3, 
              4), 
            nrow = 4, ncol = 1, dimnames = list(c("p1 -> i1", 
                                                  "p2 -> i1", 
                                                  "p1 -> i2", 
                                                  "p2 -> i2"))) %>% 
  setrowtype("Products -> Industries")
# Default separator is " -> ".
matricize_byname(v, notation = RCLabels::arrow_notation)
#>    i1 i2
#> p1  1  3
#> p2  2  4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```
