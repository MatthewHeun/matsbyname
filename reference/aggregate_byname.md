# Aggregate rows and columns in a matrix

Rows (`margin = 1`), columns (`margin = 2`), or both
(`margin = c(1, 2)`, the default) are aggregated according to
`aggregation_map`.

## Usage

``` r
aggregate_byname(
  a,
  aggregation_map = NULL,
  margin = c(1, 2),
  pattern_type = "exact"
)
```

## Arguments

- a:

  A matrix or list of matrices whose rows or columns are to be
  aggregated.

- aggregation_map:

  A named list of rows or columns to be aggregated (or `NULL`). See
  `details`.

- margin:

  `1`, `2`, or `c(1, 2)` for row aggregation, column aggregation, or
  both. As a string, `margin` can be a row or column type. Default is
  `c(1, 2)`.

- pattern_type:

  See
  [`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
  Default is "exact".

## Value

A version of `a` with aggregated rows and/or columns

## Details

When `aggregation_map` is `NULL` (the default), rows (or columns or
both) of same name are aggregated together.

If `aggregation_map` is not `NULL`, it must be a named list. The name of
each `aggregation_map` item is the name of a row or column in output
that will contain the specified aggregation. The value of each item in
`aggregation_map` must be a vector of names of rows or columns in `a`.
The names in the value are aggregated and inserted into the output with
the name of the value. For example
`aggregation_map = list(new_row = c("r1", "r2"))` will aggregate rows
"r1" and "r2", delete rows "r1" and "r2", and insert a new row whose
name is "new_row" and whose value is the sum of rows "r1" and "r2'.

The values in the `aggregation_map` are interpreted as regular
expressions, and they are escaped using
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html)
prior to use.

`margin` can be a string, in which case it is interpreted as a row or
column type. If a string `margin` does not match a row or column type,
`a` is returned unmodified.

Note that aggregation on one margin only will sort only the aggregated
margin, because the other margin is not guaranteed to have unique names.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
library(tibble)
m <- matrix(1:9, byrow = TRUE, nrow = 3, 
            dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1"))) %>% 
  setrowtype("rows") %>% setcoltype("cols")
# Aggregate all rows by establishing an aggregation map (`am`)
am <- list(new_row = c("r1", "r2"))
aggregate_byname(m, aggregation_map = am, margin = 1)
#>         c2 c1 c1
#> new_row 12 15 18
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
# aggregate_byname() also works with lists and in data frames
m1 <- matrix(42, nrow = 1, dimnames = list(c("r1"), c("c1")))
m2 <- matrix(1:4, byrow = TRUE, nrow = 2, 
             dimnames = list(c("a", "a"), c("a", "a")))
m3 <- matrix(1:9, byrow = TRUE, nrow = 3, 
             dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1")))
DF <- tibble(m = list(m1, m1, m1, m2, m2, m2, m3, m3, m3), 
             margin = list(1, 2, c(1,2), 1, 2, c(1, 2), 1, 2, c(1, 2))) %>% 
  mutate(
    aggregated = aggregate_byname(m, margin = margin), 
  )
m1
#>    c1
#> r1 42
DF$aggregated[[1]] # by rows
#>    c1
#> r1 42
DF$aggregated[[2]] # by cols
#>    c1
#> r1 42
DF$aggregated[[3]] # by rows and cols
#>    c1
#> r1 42
m2
#>   a a
#> a 1 2
#> a 3 4
DF$aggregated[[4]] # by rows
#>   a a
#> a 4 6
DF$aggregated[[5]] # by cols
#>   a
#> a 3
#> a 7
DF$aggregated[[6]] # by rows and cols
#>    a
#> a 10
m3
#>    c2 c1 c1
#> r2  1  2  3
#> r1  4  5  6
#> r1  7  8  9
DF$aggregated[[7]] # by rows
#>    c2 c1 c1
#> r1 11 13 15
#> r2  1  2  3
DF$aggregated[[8]] # by cols
#>    c1 c2
#> r2  5  1
#> r1 11  4
#> r1 17  7
DF$aggregated[[9]] # by rows and cols
#>    c1 c2
#> r1 28 11
#> r2  5  1
```
