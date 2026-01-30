# Using summarise in matsbyname

``` r
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(matsbyname)
library(tibble)
```

## Introduction

`matsbyname` functions in which operands are specified in a `...`
argument are ambiguous when applied to a data frame. But there is an
argument (`.summarise`) that signals intention, allowing the ambiguous
functions to be used flexibly with data frames.

## “Normal” functions

For normal functions, such as `+` and
[`mean()`](https://rdrr.io/r/base/mean.html), there is no ambiguity
about their operation in a data frame.

``` r
df <- tibble::tribble(~x, ~y, ~z, 
                       1,  2,  3, 
                       4,  5,  6)
# Typically, operations are done across rows.
df %>% 
  dplyr::mutate(
    a = x + y + z,
    b = rowMeans(.)
  )
#> # A tibble: 2 × 5
#>       x     y     z     a     b
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1     2     3     6     2
#> 2     4     5     6    15     5
```

To perform the same operations down columns, use
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html).

``` r
df %>% 
  dplyr::summarise(
    x = sum(x), 
    y = sum(y), 
    z = sum(z)
  )
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     5     7     9
df %>% 
  dplyr::summarise(
    x = mean(x), 
    y = mean(y), 
    z = mean(z)
  )
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1   2.5   3.5   4.5
```

## `matsbyname::sum_byname()`

What does
[`matsbyname::sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
mean for a data frame? Will it give sums across rows (as `+`), or will
it give sums down columns (as
[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html))?
This ambiguity is present for all `*_byname()` functions in which
operands are specified via the `...` argument, including
[`matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.md),
[`hadamardproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/hadamardproduct_byname.md),
[`mean_byname()`](https://matthewheun.github.io/matsbyname/reference/mean_byname.md),
etc.

To resolve the ambiguity, use the `.summarise` argument. The default
value of `.summarise` is `FALSE`, meaning that the functions normally
operate across rows. If you want to perform the action down columns, set
`.summarise = TRUE`.

``` r
df %>% 
  dplyr::mutate(
    a = sum_byname(x, y, z), 
    b = mean_byname(x, y, z)
  )
#> # A tibble: 2 × 5
#>       x     y     z     a     b
#>   <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1     2     3     6     2
#> 2     4     5     6    15     5
df %>% 
  dplyr::summarise(
    x = sum_byname(x, .summarise = TRUE) %>% unlist(), 
    y = sum_byname(y, .summarise = TRUE) %>% unlist(), 
    z = sum_byname(z, .summarise = TRUE) %>% unlist()
  )
#> # A tibble: 1 × 3
#>       x     y     z
#>   <dbl> <dbl> <dbl>
#> 1     5     7     9
```

## Summary

The `.summarise` argument broadens the range of applicability for many
`matsbyname` functions, especially when used with data frames. The
default is `.summarise = FALSE`, meaning that operations will be
performed across columns. Set `.summarise = TRUE` argument to signal
intent to perform operations down a column.
