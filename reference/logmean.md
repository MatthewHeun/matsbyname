# Logarithmic mean of two numbers

Calculates the logarithmic mean of two numbers.

## Usage

``` r
logmean(a, b, base = exp(1))
```

## Arguments

- a:

  the first operand (must be non-negative)

- b:

  the second operand (must be non-negative)

- base:

  the base of the logarithm used in this calculation. (Default is
  `exp(1)`.)

## Value

`0` if `a = 0` or `b = 0`; `x1` if `a == b`; and
`(a - b) / log(a/b, base = base)` for all other values of `a` and `b`

## Details

This is an internal helper function for `logarithmicmean_byname`.

## Examples

``` r
matsbyname:::logmean(0, 0) # 0
#> [1] 0
matsbyname:::logmean(0, 1) # 0
#> [1] 0
matsbyname:::logmean(1, 0) # 0
#> [1] 0
matsbyname:::logmean(1, 1) # 1
#> [1] 1
matsbyname:::logmean(2, 1)
#> [1] 1.442695
matsbyname:::logmean(1, 2) # commutative
#> [1] 1.442695
matsbyname:::logmean(1, 10) # base = exp(1), the default
#> [1] 3.90865
matsbyname:::logmean(1, 10, base = 10)
#> [1] 9
```
