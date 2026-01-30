# Rename matrix rows and columns by prefix and suffix

**\[superseded\]** It can be convenient to rename rows or columns of
matrices based on retaining prefixes or suffixes. This function provides
that capability.

## Usage

``` r
rename_to_pref_suff_byname(a, keep, margin = c(1, 2), notation)
```

## Arguments

- a:

  a matrix or list of matrices whose rows or columns will be renamed.

- keep:

  one of "prefix" or "suffix" indicating which part of the row or column
  name to retain.

- margin:

  one of `1`, `2`, or `c(1, 2)` where `1` indicates rows and `2`
  indicates columns.

- notation:

  See `notation_vec()`.

## Value

`a` with potentially different row or column names.

## Details

A prefix is defined by an opening string (`prefix_open`) and a closing
string (`prefix_close`). A suffix is defined by an opening string
(`suffix_open`) and a closing string (`suffix_close`). If `sep` is
provided and none of `prefix_open`, `prefix_close`, `suffix_open`, and
`suffix_close` are provided, default arguments become: \* `prefix_open`:
"", \* `prefix_close`: `sep`, \* `suffix_open`: `sep`, and \*
`suffix_close`: "".

The `keep` parameter tells which portion to retain (prefixes or
suffixes),

If prefixes or suffixes are not found in a row and/or column name, that
name is unchanged.

## Examples

``` r
# This function is superseded. 
# Instead, use `rename_to_piece_byname()`.
# For example:
m <- matrix(c(1, 2, 
              3, 4, 
              5, 6), nrow = 3, byrow = TRUE, 
            dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
m
#>        a -> b c -> d
#> a -> b      1      2
#> r2          3      4
#> r3          5      6
rename_to_piece_byname(m, piece = "pref", notation = RCLabels::arrow_notation)
#>    a c
#> a  1 2
#> r2 3 4
#> r3 5 6
# Note, labels are lost, because some labels are missing a suffix.
rename_to_piece_byname(m, piece = "suff", notation = RCLabels::arrow_notation)
#>   b d
#> b 1 2
#>   3 4
#>   5 6
# Original documentation:
rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
#>    a c
#> a  1 2
#> r2 3 4
#> r3 5 6
rename_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
#>   b d
#> b 1 2
#>   3 4
#>   5 6
```
