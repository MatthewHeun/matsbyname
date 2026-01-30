# Select columns of a matrix (or list of matrices) by name

Arguments indicate which columns are to be retained and which are to be
removed. For maximum flexibility, arguments are extended regex patterns
that are matched against column names.

## Usage

``` r
select_cols_byname(
  a,
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

  a matrix or a list of matrices

- retain_pattern:

  an extended regex or list of extended regular expressions that
  specifies which columns of `m` to retain. Default pattern ("\$^")
  retains nothing.

- remove_pattern:

  an extended regex or list of extended regular expressions that
  specifies which columns of `m` to remove. Default pattern ("\$^")
  removes nothing.

- ignore.case, perl, fixed, useBytes:

  Arguments passed to [`grep()`](https://rdrr.io/r/base/grep.html).

## Value

a matrix that is a subset of `a` with columns selected by
`retain_pattern` and `remove_pattern`.

## Details

If `a` is `NULL`, `NULL` is returned.

Patterns are compared against column names using extended regex. If no
column names of `a` match the `retain_pattern`, `NULL` is returned. If
no column names of `a` match the `remove_pattern`, `a` is returned.

Retaining columns takes precedence over removing columns, always.

Some typical patterns are:

- "^Electricity\$\|^Oil\$": column names that are EXACTLY "Electricity"
  or "Oil".

- "^Electricity\|^Oil": column names that START WITH "Electricity" or
  "Oil".

- "Electricity\|Oil": column names that CONTAIN "Electricity" or "Oil"
  anywhere within them.

Given a list of column names, a pattern can be constructed easily using
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
escapes regex strings using
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html).
This function assumes that `retain_pattern` and `remove_pattern` have
already been suitably escaped.

Note that the default `retain_pattern` and `remove_pattern` ("\$^")
retain nothing and remove nothing.

If the row or column labels contain "\[" or "\]", care should be taken
to escape those characters.
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html)
is helpful in such situations.

Note that if all columns are removed from `a`, `NULL` is returned.

## Examples

``` r
m <- matrix(1:16, ncol = 4, dimnames=list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
select_cols_byname(m, 
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
select_cols_byname(m, 
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
select_cols_byname(list(m,m), retain_pattern = "^p1$|^p4$")
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
