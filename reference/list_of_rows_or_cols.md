# Named list of rows or columns of matrices

This function takes matrix `m` and converts it to a list of single-row
(if `margin == 1`) or single-column(if `margin == 2`) matrices. Each
item in the list is named for its row (if `margin == 1`) or column (if
`margin == 2`).

## Usage

``` r
list_of_rows_or_cols(a, margin)
```

## Arguments

- a:

  a matrix or list of matrices (say, from a column of a data frame)

- margin:

  the margin of the matrices to be extracted (`1` for rows, `2` for
  columns)

## Value

a named list of rows or columns extracted from `m`

## Details

Note that the result provides column vectors, regardless of the value of
`margin`.

## Examples

``` r
m <- matrix(data = c(1:6), 
            nrow = 2, ncol = 3, 
            dimnames = list(c("p1", "p2"), c("i1", "i2", "i3"))) %>%
  setrowtype(rowtype = "Products") %>% setcoltype(coltype = "Industries")
list_of_rows_or_cols(m, margin = 1)
#> $p1
#>    p1
#> i1  1
#> i2  3
#> i3  5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> $p2
#>    p2
#> i1  2
#> i2  4
#> i3  6
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
#> 
list_of_rows_or_cols(m, margin = 2)
#> $i1
#>    i1
#> p1  1
#> p2  2
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> $i2
#>    i2
#> p1  3
#> p2  4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> $i3
#>    i3
#> p1  5
#> p2  6
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
```
