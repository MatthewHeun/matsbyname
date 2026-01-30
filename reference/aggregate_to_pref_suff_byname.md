# Aggregate a matrix to prefixes or suffixes of row and/or column names

**\[superseded\]** Row and column names are often constructed in the
form `prefix_start` `prefix` `prefix_end` `suffix_start` `suffix`
`suffix_end` and described by a notation vector. (See `notation_vec()`.)
This function performs aggregation by prefix or suffix according to a
notation vector.

## Usage

``` r
aggregate_to_pref_suff_byname(
  a,
  aggregation_map = NULL,
  keep,
  margin = c(1, 2),
  notation,
  pattern_type = "exact"
)
```

## Arguments

- a:

  A matrix of list of matrices to be aggregated by prefix or suffix.

- aggregation_map:

  See
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md).

- keep:

  See
  [`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md)

- margin:

  the dimension over which aggregation is to be performed; `1` for rows,
  `2` for columns, or `c(1, 2)` for both.

- notation:

  See `notation_vec()`.

- pattern_type:

  See
  [`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md).

## Value

An aggregated version of `a`.

## Details

This function is a convenience function, as it bundles sequential calls
to two helper functions,
[`rename_to_pref_suff_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_pref_suff_byname.md)
and
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md).
All arguments are passed to the helper functions.

## Examples

``` r
# This function is superseded. 
# Instead, use `aggregate_pieces_byname()`.
# For example:
m <- matrix((1:9), byrow = TRUE, nrow = 3, 
            dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")))
m
#>         c1 -> z c2 -> y c3 -> y
#> r1 -> b       1       2       3
#> r2 -> b       4       5       6
#> r3 -> a       7       8       9
aggregate_pieces_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#>    c1 c2 c3
#> r1  1  2  3
#> r2  4  5  6
#> r3  7  8  9
aggregate_pieces_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
#>    y z
#> a 17 7
#> b 16 5

# Original examples:
# Aggregation by prefixes does nothing more than rename, because all prefixes are different.
# Doing renaming like this (without also aggregating) is potentially dangerous, because  
# some rows and some columns could end up with same names.
aggregate_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
#>    c1 c2 c3
#> r1  1  2  3
#> r2  4  5  6
#> r3  7  8  9
# Aggregation by suffix reduces the number of rows and columns, 
# because there are same suffixes in both rows and columns
aggregate_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
#>    y z
#> a 17 7
#> b 16 5
```
