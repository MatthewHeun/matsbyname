# Rename row or column names via regexp pattern

It is sometimes helpful to rename row or column names for a list of
matrices via a `regexp_pattern`. When `a` is a matrix or a list of
matrices, `regexp_pattern` indicates which characters are replaced by
`replacement`.

## Usage

``` r
rename_via_pattern_byname(
  a,
  margin = list(c(1, 2)),
  regexp_pattern = "$^",
  replacement,
  pieces = "all",
  prepositions = RCLabels::prepositions_list,
  notation = RCLabels::bracket_notation,
  ...
)
```

## Arguments

- a:

  A matrix or list of matrices.

- margin:

  The margin on which replacements are performed. Default is `c(1, 2)`,
  meaning both row (`1`) and column (`2`) names will be replaced.

- regexp_pattern:

  The regular expression pattern that will be replaced in the row or
  column labels. Default is "\$^", meaning nothing will be matched.

- replacement:

  The string to replace the `regexp_pattern`.

- pieces:

  The pieces of labels to be searched for `regexp_pattern`. See
  [RCLabels::replace_by_pattern](https://matthewheun.github.io/RCLabels/reference/regex_funcs.html)
  for details. Default is "all".

- prepositions:

  Prepositions to use while searching for `pieces`. Default is
  [RCLabels::prepositions_list](https://matthewheun.github.io/RCLabels/reference/prepositions_list.html).

- notation:

  The notation used for for searching `pieces`. Default is
  [RCLabels::bracket_notation](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html).

- ...:

  Other arguments passed to
  [`gsub()`](https://rdrr.io/r/base/grep.html), such as `ignore.case`,
  `perl`, `fixed`, or `useBytes`. Arguments in `...` apply to all
  matrices in `a`. See examples.

## Value

A modified version of `a`.

## Details

Note that `margin` can be a rowtype or coltype string which will be
de-referenced to the integer margin (`1` for rows or `2` for columns).

Internally, this function calls
[RCLabels::replace_by_pattern](https://matthewheun.github.io/RCLabels/reference/regex_funcs.html).

## Examples

``` r
ma <- matrix(c(1, 2), nrow = 2,
             dimnames = list(c("Natural gas [from Supply]",
                               "row2"), 
                             "col")) |> 
  setrowtype("Product") |> setcoltype("Industry")
mb <- matrix(c(1, 2), nrow = 2,
             dimnames = list(c("Natural gas [from Supply]",
                               "Fuel oil [from Supply]"), 
                             "col")) |> 
  setrowtype("Product") |> setcoltype("Industry")
ma |> 
  rename_via_pattern_byname(regexp_pattern = " [from Supply]",
                            replacement = " bogus", 
                            fixed = TRUE)
#>                   col
#> Natural gas bogus   1
#> row2                2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
list(ma, mb) |> 
  rename_via_pattern_byname(margin = 1,
                            regexp_pattern = " [from Supply]",
                            replacement = " from Supply", 
                            fixed = TRUE)
#> [[1]]
#>                         col
#> Natural gas from Supply   1
#> row2                      2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>                         col
#> Natural gas from Supply   1
#> Fuel oil from Supply      2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
res1 <- tibble::tibble(m = list(ma, mb)) |> 
  dplyr::mutate(
    m1 = .data[["m"]] |> 
      rename_via_pattern_byname(regexp_pattern = " [from Supply]", 
                                replacement = "", 
                                fixed = TRUE)
  )
res1$m1
#> [[1]]
#>             col
#> Natural gas   1
#> row2          2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>             col
#> Natural gas   1
#> Fuel oil      2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
# Transpose mb and use a string for the margin.
# The string (in this case "Product")
# is dereferenced to an integer margin.
# In this case, the rownames of the first matrix
# and the colnames of the second matrix are replaced,
# because those are on the "Product" margin.
res2 <- tibble::tibble(m = list(ma, 
                                transpose_byname(mb))) |> 
  dplyr::mutate(
    m2 = .data[["m"]] |> 
      rename_via_pattern_byname(margin = "Product", 
                                regexp_pattern = " [from Supply]", 
                                replacement = "", 
                                fixed = TRUE)
  )
rowtype(res2$m2[[1]])
#> [1] "Product"
coltype(res2$m2[[2]])
#> [1] "Product"
res2$m2
#> [[1]]
#>             col
#> Natural gas   1
#> row2          2
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>     Natural gas Fuel oil
#> col           1        2
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Product"
#> 
```
