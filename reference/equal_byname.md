# Compare two matrices "by name" for equality

If operands are matrices, they are completed and sorted relative to one
another prior to comparison.

## Usage

``` r
equal_byname(..., .summarise = FALSE, tol = 0)
```

## Arguments

- ...:

  Operands to be compared.

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`). Default is `FALSE` (across lists).

- tol:

  A double that tells how precisely equal the values of `a` and `b` must
  be. Default is `0`.

## Value

`TRUE` iff all information is equal, including row and column types
*and* row and column names *and* entries in the matrices.

## Details

Comparisons are made by
`equal_matrix_or_Matrix(a, b, tolerance = abs(tol))` so that variations
among numbers within `tol` will still return `TRUE.`

If EXACT comparison is needed, use
[`identical_byname()`](https://matthewheun.github.io/matsbyname/reference/identical_byname.md),
which compares using `identical(a, b)`.

`tol` should be a single value that applies to all items in `...`.

## Examples

``` r
a <- matrix(1:4, nrow = 2)
b <- matrix(1:4, nrow = 2)
equal_byname(a, b)
#> [1] TRUE
equal_byname(a, b + 1e-100)
#> [1] TRUE
identical_byname(a, b + 1e-100)
#> [1] FALSE
a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
equal_byname(a, b) # FALSE because a has row and column types, but b does not.
#> [1] FALSE
b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
equal_byname(a, b)
#> [1] TRUE
dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
equal_byname(a, b) # FALSE, because row and column names are not equal
#> [1] FALSE
dimnames(b) <- dimnames(a)
equal_byname(a, b)
#> [1] TRUE
```
