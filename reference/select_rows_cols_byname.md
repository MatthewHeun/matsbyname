# Select (or deselect) rows or columns

Arguments indicate which columns are to be retained and which are to be
removed by routing to
[`select_rows_byname()`](https://matthewheun.github.io/matsbyname/reference/select_rows_byname.md)
or
[`select_cols_byname()`](https://matthewheun.github.io/matsbyname/reference/select_cols_byname.md)
based on the value of `margin`. `margin` can be a string or vector of
strings that are matched to row and column types.

## Usage

``` r
select_rows_cols_byname(
  a,
  margin = c(1, 2),
  retain_pattern = "$^",
  remove_pattern = "$^",
  ignore.case = FALSE,
  perl = FALSE,
  fixed = FALSE,
  useBytes = FALSE
)
```

## Arguments

- a:

  A matrix or a list of matrices.

- margin:

  `1` for rows, `2` for columns, or `c(1, 2)` (the default) for both.
  Can be a string or vector of strings that indicate row and/or column
  types.

- retain_pattern:

  An extended regex or list of extended regular expressions that
  specifies which columns of `m` to retain. Default pattern ("\$^")
  retains nothing.

- remove_pattern:

  An extended regex or list of extended regular expressions that
  specifies which columns of `m` to remove. Default pattern ("\$^")
  removes nothing.

- ignore.case, perl, fixed, useBytes:

  Arguments passed to [`grep()`](https://rdrr.io/r/base/grep.html).

## Value

A matrix that is a subset of `a` with columns selected by
`retain_pattern` and `remove_pattern`.

## Details

If `a` is `NULL`, `NULL` is returned.

For maximum flexibility, arguments can be extended regex patterns that
are matched against row or column names. If no row or column (depending
on `margin`) names of `a` match the `retain_pattern`, `NULL` is
returned. If no row or column (depending on `margin`) names of `a` match
the `remove_pattern`, `a` is returned.

Retaining takes precedence over removing, always.

Some typical patterns are:

- "^Electricity\$\|^Oil\$": row or column names that are EXACTLY
  "Electricity" or "Oil".

- "^Electricity\|^Oil": row or column names that START WITH
  "Electricity" or "Oil".

- "Electricity\|Oil": row or column names that CONTAIN "Electricity" or
  "Oil" anywhere within them.

If the row or column labels contain "\[" or "\]", care should be taken
to escape those characters.
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html)
is helpful in such situations. This function assumes that
`retain_pattern` and `remove_pattern` have already been suitably
escaped.

Given a list of row or column names, a pattern can be constructed easily
using
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
escapes regex strings using
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html).

Note that the default `retain_pattern` and `remove_pattern` ("\$^")
retain nothing and remove nothing.

Note that if all columns are removed from `a`, `NULL` is returned.

## Examples

``` r
m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("p", 1:4))) |> 
  setrowtype("Industries") |> setcoltype("Commodities")
select_rows_cols_byname(m, 
                        margin = 2, # for columns
                        retain_pattern = RCLabels::make_or_pattern(c("p1", "p4"), 
                        pattern_type = "exact"))
#>    p1 p4
#> i1  1 13
#> i2  2 14
#> i3  3 15
#> i4  4 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
select_rows_cols_byname(m, 
                        margin = 2, 
                        remove_pattern = RCLabels::make_or_pattern(c("p1", "p3"), 
                        pattern_type = "exact"))
#>    p2 p4
#> i1  5 13
#> i2  6 14
#> i3  7 15
#> i4  8 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# Also works for lists and data frames
select_rows_cols_byname(list(m,m), margin = 2, 
                        retain_pattern = "^p1$|^p4$")
#> [[1]]
#>    p1 p4
#> i1  1 13
#> i2  2 14
#> i3  3 15
#> i4  4 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    p1 p4
#> i1  1 13
#> i2  2 14
#> i3  3 15
#> i4  4 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
```
