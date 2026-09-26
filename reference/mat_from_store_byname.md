# Create a matrix (or vector) with labels from a matrix and values from a "store"

When a matrix (`a`) is multiplied by another matrix or vector (`v`)
byname, naming can be tricky. There are times when pieces of the labels
in `v` should be matched to pieces of the `a` labels. This function
helps by performing the matching byname. For this function, matrix or
vector `v` is considered a store of values, rows, or columns from which
the output vector or matrix is constructed using special matching rules
between matrix `a` and matrix or vector `v`.

## Usage

``` r
mat_from_store_byname(
  a,
  v,
  a_piece = "all",
  v_piece = "all",
  margin = 1,
  margin_v = 1,
  notation = if (is.list(a)) {
     list(RCLabels::bracket_notation)
 } else {
    
RCLabels::bracket_notation
 },
  prepositions = if (is.list(a)) {
     list(RCLabels::prepositions_list)
 } else {
    
RCLabels::prepositions_list
 },
  missing = NA_real_
)
```

## Arguments

- a:

  A matrix from which row or column labels are taken. Can also be a list
  or the name of a column in a data frame.

- v:

  A matrix or vector from which values are taken, when `a_piece` of
  names on `a`'s `margin` matches `v_piece` of names on `v`'s
  `margin_v`. Can also be a list or the name of a column in a data
  frame.

- a_piece:

  The piece of labels on `a`'s `margin` that is to be matched. Default
  is "all".

- v_piece:

  The piece of labels on `v`'s `margin_v` that is to be matched. Default
  is "all".

- margin:

  Tells whether to assess the rows (`1`) or columns (`2`) of `a` when
  creating the outgoing vector. Default is `1`.

- margin_v:

  Tells whether to assess the rows (`1`) or columns (`2`) of `v` when
  creating the outgoing vector. Default is `1`.

- notation:

  The notation for the row and column labels. Default is
  [`RCLabels::bracket_notation`](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html),
  wrapped as a list if `a` is a list.

- prepositions:

  The strings that will count for prepositions. Default is
  [`RCLabels::prepositions`](https://matthewheun.github.io/RCLabels/reference/prepositions.html),
  wrapped as a list if `a` is a list.

- missing:

  The value used when the desired value is not found in `v`. Default is
  `NA_real_`.

## Value

A column vector with names from `a` and values from `v`.

## Details

The output of this function is a matrix or vector. The names of the
output rows are taken from the `margin` of `a`. (`a = 1` means rows;
`a = 2` means columns.) The names of the other dimension of the output
are taken from `v`, but not `margin`. Rather, the names of the other
dimension of the output are taken from the other dimension of `v`. If
`margin_v` is `1`, the names of the other dimension of the output are
taken from columns of `v`. If `margin_v` is `2`, the names of the other
dimension of the output are taken from rows of `v`.

The values of the output matrix are obtained from `v` when `a_piece`
matches `v_piece` using the `RCLabels` package. The `v_piece`s of `v`'s
`margin_v` must be unique. The default values for `a_piece` and
`v_piece` are "all", meaning that the entire label of `a` should be
matched to the entire label of `v`. Other options for `a_piece` and
`v_piece` are "pref" and "suff", which will match the prefix or suffix
of the labels. Alternatively, prepositions can be given such that
objects of prepositions will be matched. Examples include "from" or
"in". Row and column types from `v` are applied to the output. If the
piece given in `a_piece` is not present in row or column names of `a`,
`NA_real_` is returned. If the piece given in `v_piece` is not present
in row or column names of `v`, `NA_real_` is returned.

Note that `notation` and `prepositions` should be lists if `a` is a list
but a single value otherwise. The default values of `notation` and
`prepositions` take care of this requirement, switching on the type of
`a` (list or not).

The class of the output object is determined from `a`. If `a` is a
`Matrix`, the output will be a `Matrix`. Otherwise, the output will be a
`matrix`.

`mat_from_store_byname()` is a more-capable replacement for the
deprecated
[`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md).
`mat_from_store_byname()` allows matrices in the `v` argument. If
[`vec_from_store_byname()`](https://matthewheun.github.io/matsbyname/reference/vec_from_store_byname.md)
was used previously with a column vector in the `v` argument and a
column vector was the desired output, `mat_from_store_byname()` should
be a drop-in replacement.

## Examples

``` r
a <- matrix(42, nrow = 3, ncol = 5, 
            dimnames = list(c("Electricity [from b in c]", 
                              "Coal [from e in f]", 
                              "Crude oil [from Production in USA]"), 
                            c("Main activity producer electricity plants", 
                              "Wind turbines", 
                              "Oil refineries", 
                              "Coal mines", 
                              "Automobiles"))) %>%
  setrowtype("Product") %>% setcoltype("Industry")
a
#>                                    Main activity producer electricity plants
#> Electricity [from b in c]                                                 42
#> Coal [from e in f]                                                        42
#> Crude oil [from Production in USA]                                        42
#>                                    Wind turbines Oil refineries Coal mines
#> Electricity [from b in c]                     42             42         42
#> Coal [from e in f]                            42             42         42
#> Crude oil [from Production in USA]            42             42         42
#>                                    Automobiles
#> Electricity [from b in c]                   42
#> Coal [from e in f]                          42
#> Crude oil [from Production in USA]          42
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
v <- matrix(1:7, nrow = 7, ncol = 1, 
            dimnames = list(c("Electricity", 
                              "Peat", 
                              "Hydro", 
                              "Crude oil",
                              "Coal", 
                              "Hard coal (if no detail)", 
                              "Brown coal"), 
                            "phi")) %>%
  setrowtype("Product") %>% setcoltype("phi")
v
#>                          phi
#> Electricity                1
#> Peat                       2
#> Hydro                      3
#> Crude oil                  4
#> Coal                       5
#> Hard coal (if no detail)   6
#> Brown coal                 7
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"
vec_from_store_byname(a, v, a_piece = "pref")
#> Warning: `vec_from_store_byname()` was deprecated in matsbyname 0.6.15.
#> ℹ Please use `mat_from_store_byname()` instead.
#> ℹ `mat_from_store_byname()`, a more-capable replacement,
#> ℹ allows matrices in the `v` argument and matrices in the output.
#>                                    phi
#> Electricity [from b in c]            1
#> Coal [from e in f]                   5
#> Crude oil [from Production in USA]   4
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"
vec_from_store_byname(a, v, a_piece = "noun")
#>                                    phi
#> Electricity [from b in c]            1
#> Coal [from e in f]                   5
#> Crude oil [from Production in USA]   4
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"

v2 <- matrix(1:7, nrow = 7, ncol = 1, 
             dimnames = list(c("Electricity", 
                               "Peat", 
                               "USA", 
                               "c",
                               "Coal", 
                               "Hard coal (if no detail)", 
                               "f"), 
                             "phi")) %>%
  setrowtype("Product") %>% setcoltype("phi")
vec_from_store_byname(a, v2, a_piece = "in")
#>                                    phi
#> Electricity [from b in c]            4
#> Coal [from e in f]                   7
#> Crude oil [from Production in USA]   3
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"

# Works with lists
v3 <- matrix(1:7, nrow = 7, ncol = 1, 
             dimnames = list(c("Electricity [from USA]", 
                               "Peat [from nowhere]", 
                               "Production [from GHA]", 
                               "e [from ZAF]",
                               "Coal [from AUS]", 
                               "Hard coal (if no detail) [from GBR]", 
                               "b [from Nebraska]"), 
                             "phi")) %>%
  setrowtype("Product") %>% setcoltype("phi")
a_list <- list(a, a)
v_list <- list(v3, v3)
vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from")
#> [[1]]
#>                                    phi
#> Electricity [from b in c]           NA
#> Coal [from e in f]                  NA
#> Crude oil [from Production in USA]   1
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"
#> 
#> [[2]]
#>                                    phi
#> Electricity [from b in c]           NA
#> Coal [from e in f]                  NA
#> Crude oil [from Production in USA]   1
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "phi"
#> 

# Also works in a data frame
df <- tibble::tibble(a = list(a, a, a), 
                     v = list(v3, v3, v3))
df %>%
  dplyr::mutate(
    actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from")
  )
#> # A tibble: 3 × 3
#>   a             v             actual       
#>   <list>        <list>        <list>       
#> 1 <dbl [3 × 5]> <int [7 × 1]> <dbl [3 × 1]>
#> 2 <dbl [3 × 5]> <int [7 × 1]> <dbl [3 × 1]>
#> 3 <dbl [3 × 5]> <int [7 × 1]> <dbl [3 × 1]>
```
