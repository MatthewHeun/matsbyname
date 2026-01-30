# Compare two matrices "by name" for exact equality

If operands are matrices, they are completed and sorted relative to one
another prior to comparison.

## Usage

``` r
identical_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands to be compared.

- .summarise:

  Tells whether the operation should be accomplished across lists
  (`FALSE`) or down lists (`TRUE`).

## Value

`TRUE` iff all information is identical, including row and column types
*and* row and column names *and* entries in the matrices.

## Details

Comparisons are made by `identical(a, b)` so that variations among
numbers within the computational precision will return `FALSE`.

If fuzzy comparison is needed, use
[`equal_byname`](https://matthewheun.github.io/matsbyname/reference/equal_byname.md),
which compares using `isTRUE(all.equal(a, b))`.

## Examples

``` r
a <- matrix(1:4, nrow = 2)
b <- matrix(1:4, nrow = 2)
identical_byname(a, b)
#> [1] TRUE
identical_byname(a, b + 1e-100)
#> [1] FALSE
a <- a %>% setrowtype("Industries") %>% setcoltype("Commodities")
identical_byname(a, b) # FALSE because a has row and column types, but b does not.
#> [1] FALSE
b <- b %>% setrowtype("Industries") %>% setcoltype("Commodities")
identical_byname(a, b)
#> [1] TRUE
dimnames(a) <- list(c("i1", "i2"), c("c1", "c2"))
dimnames(b) <- list(c("c1", "c2"), c("i1", "i2"))
identical_byname(a, b) # FALSE, because row and column names are not equal
#> [1] FALSE
dimnames(b) <- dimnames(a)
identical_byname(a, b)
#> [1] TRUE
```
