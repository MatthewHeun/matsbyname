# Change row and column name notation

This function switches matrix row and/or column names from one type of
notation to another based on the `from` and `to` arguments. Optionally,
prefix and suffix can be `flip`ped.

## Usage

``` r
switch_notation_byname(a, margin = c(1, 2), from, to, flip = FALSE)
```

## Arguments

- a:

  A matrix or list of matrices whose row and/or column notation is to be
  changed.

- margin:

  `1` For rows, `2` for columns, or `c(1, 2)` for both rows and columns.
  Default is `c(1, 2)`.

- from:

  The `notation` to switch *away from*.

- to:

  The `notation` to switch *to*.

- flip:

  A boolean that tells whether to also flip the notation. Default is
  `FALSE`.

## Value

Matrices with row and column names with switched notation, per
arguments.

## Examples

``` r
m <- matrix(c(1, 2, 
              3, 4), nrow = 2, ncol = 2, byrow = TRUE, 
            dimnames = list(c("b [a]", "d [c]"), c("f [e]", "h [g]"))) %>% 
  setrowtype("Products [Industries]") %>% setcoltype("Industries [Products]")
m
#>       f [e] h [g]
#> b [a]     1     2
#> d [c]     3     4
#> attr(,"rowtype")
#> [1] "Products [Industries]"
#> attr(,"coltype")
#> [1] "Industries [Products]"
switch_notation_byname(m, from = RCLabels::bracket_notation, to = RCLabels::arrow_notation, 
                       flip = TRUE)
#>        e -> f g -> h
#> a -> b      1      2
#> c -> d      3      4
#> attr(,"rowtype")
#> [1] "Industries -> Products"
#> attr(,"coltype")
#> [1] "Products -> Industries"
# Also works for lists.
# Note that margin must be specified as a list here.
switch_notation_byname(list(m, m), margin = list(c(1, 2)), 
                       from = RCLabels::bracket_notation, 
                       to = RCLabels::arrow_notation, flip = TRUE)
#> [[1]]
#>        e -> f g -> h
#> a -> b      1      2
#> c -> d      3      4
#> attr(,"rowtype")
#> [1] "Industries -> Products"
#> attr(,"coltype")
#> [1] "Products -> Industries"
#> 
#> [[2]]
#>        e -> f g -> h
#> a -> b      1      2
#> c -> d      3      4
#> attr(,"rowtype")
#> [1] "Industries -> Products"
#> attr(,"coltype")
#> [1] "Products -> Industries"
#> 
```
