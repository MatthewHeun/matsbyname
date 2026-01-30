# Select (or de-select) rows of a matrix (or list of matrices) by name

Arguments indicate which rows are to be retained and which are to be
removed. For maximum flexibility, arguments are extended regex patterns
that are matched against row names.

## Usage

``` r
select_rows_byname(
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

  A matrix or a list of matrices.

- retain_pattern:

  An extended regex or list of extended regular expressions that
  specifies which rows of `a` to retain. Default pattern ("\$^") retains
  nothing.

- remove_pattern:

  An extended regex or list of extended regular expressions that
  specifies which rows of `a` to remove, Default pattern ("\$^") removes
  nothing.

- ignore.case, perl, fixed, useBytes:

  Arguments passed to [`grep()`](https://rdrr.io/r/base/grep.html).

## Value

A matrix that is a subset of `m` with rows selected by `retain_pattern`
and `remove_pattern`.

## Details

If `a` is `NULL`, `NULL` is returned.

Patterns are compared against row names using extended regex. If no row
names of `a` match the `retain_pattern`, `NULL` is returned. If no row
names of `a` match the `remove_pattern`, `m` is returned. Note that the
default `retain_pattern` and `remove_pattern` ("\$^") retain nothing and
remove nothing.

Retaining rows takes precedence over removing rows, always.

Some typical patterns are:

- "^Electricity\$\|^Oil\$": row names that are EXACTLY "Electricity" or
  EXACTLY "Oil".

- "^Electricity\|^Oil": row names that START WITH "Electricity" or START
  WITH "Oil".

- "Electricity\|Oil": row names that CONTAIN "Electricity" or CONTAIN
  "Oil" anywhere within them.

Given a list of column names, a pattern can be constructed easily using
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html).
[`RCLabels::make_or_pattern()`](https://matthewheun.github.io/RCLabels/reference/make_or_pattern.html)
escapes regex strings using
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html).
This function assumes that `retain_pattern` and `remove_pattern` have
already been suitably escaped.

If the row or column labels contain "\[" or "\]", care should be taken
to escape those characters.
[`Hmisc::escapeRegex()`](https://rdrr.io/pkg/Hmisc/man/escapeRegex.html)
is helpful in such situations.

Note that if all rows are removed from `a`, `NULL` is returned.

## Examples

``` r
m <- matrix(1:16, ncol = 4, dimnames = list(c(paste0("i", 1:4)), paste0("p", 1:4))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
select_rows_byname(m, 
                   retain_pattern = RCLabels::make_or_pattern(c("i1", "i4"),
                   pattern_type = "exact"))
#>    p1 p2 p3 p4
#> i1  1  5  9 13
#> i4  4  8 12 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
select_rows_byname(m, 
                   remove_pattern = RCLabels::make_or_pattern(c("i1", "i3"), 
                   pattern_type = "exact"))
#>    p1 p2 p3 p4
#> i2  2  6 10 14
#> i4  4  8 12 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
# Also works for lists and data frames
select_rows_byname(list(m, m), retain_pattern = "^i1$|^i4$")
#> [[1]]
#>    p1 p2 p3 p4
#> i1  1  5  9 13
#> i4  4  8 12 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
#> [[2]]
#>    p1 p2 p3 p4
#> i1  1  5  9 13
#> i4  4  8 12 16
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Commodities"
#> 
```
