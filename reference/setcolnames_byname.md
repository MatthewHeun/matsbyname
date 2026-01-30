# Sets column names

Sets column names in a way that is amenable to use in piping operations
in a functional programming way. if `a` is `NULL`, `NULL` is returned.
If `a` is a constant, it is converted to a matrix and `colnames` are
applied. If `a` is a matrix, `colnames` should be a vector of new column
names that is as long as the number of columns in `a`. If `a` is a list
of matrices, `colnames` can also be a list, and it should be as long as
`a`. Or `colnames` can be a vector of column names which will be applied
to every matrix in the list of `a`. Each item in the list should be a
vector containing column names for the corresponding matrix in `a`.

## Usage

``` r
setcolnames_byname(a, colnames)
```

## Arguments

- a:

  A matrix or a list of matrices in which column names are to be set

- colnames:

  A vector of new column names or a list of vectors of new column names

## Value

a copy of `a` with new column names

## Examples

``` r
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
setcolnames_byname(m, c("a", "b", "c"))
#>    a b c
#> i1 1 3 5
#> i2 2 4 6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
```
