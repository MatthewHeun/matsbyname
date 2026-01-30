# Hatize and invert a vector

When dividing rows or columns of a matrix by elements of a vector, the
vector elements are placed on the diagonal of a new matrix, the diagonal
matrix is inverted, and the result is pre- or post-multiplied into the
matrix. This function performs the hatizing and inverting of vector `v`
in one step and takes advantage of computational efficiencies to achieve
the desired result. The computational shortcut is apparent when one
observes that the matrix produced by hatizing and inverting a vector is
a diagonal matrix whose non-zero elements are the numerical inverses of
the individual elements of `v`. So this function first inverts each
element of `v` then places the inverted elements on the diagonal of a
diagonal matrix.

## Usage

``` r
hatinv_byname(v, keep = NULL, inf_becomes = .Machine$double.xmax)
```

## Arguments

- v:

  The vector to be hatized and inverted.

- keep:

  See
  [`hatize_byname()`](https://matthewheun.github.io/matsbyname/reference/hatize_byname.md).

- inf_becomes:

  A value to be substitute for any `Inf` produced by the inversion
  process. Default is `.Machine$double.xmax`. Another reasonable value
  is `Inf`. Set to `NULL` to disable substitution.

## Value

a square diagonal matrix with inverted elements of `v` on the diagonal

## Details

Note that this function gives the same result as
`invert_byname(hatize_byname(v))`, except that
`invert_byname(hatize_byname(v))` fails due to a singular matrix error
when any of the elements of `v` are zero. This function will give
`inf_becomes` on the diagonal of the result for each zero element of
`v`, arguably a better answer. The sign of `Inf` is preserved in the
substitution. The default value of `inf_becomes` is
`.Machine$double.xmax`. Set `inf_becomes` to `NULL` to disable this
behavior.

The default behavior is helpful for cases when the result of
`hatinv_byname()` is later multiplied by `0` to obtain `0`. Multiplying
`Inf` by `0` gives `NaN` which would effectively end the stream of
calculations.

## Examples

``` r
v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
  setrowtype("Industries") %>% setcoltype(NA)
r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
  setrowtype(NA) %>% setcoltype("Commodities")
hatinv_byname(v, keep = "rownames")
#>     i1 i10  i2        i3   i4  i5        i6        i7    i8        i9
#> i1   1 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i10  0 0.1 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i2   0 0.0 0.5 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i3   0 0.0 0.0 0.3333333 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i4   0 0.0 0.0 0.0000000 0.25 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i5   0 0.0 0.0 0.0000000 0.00 0.2 0.0000000 0.0000000 0.000 0.0000000
#> i6   0 0.0 0.0 0.0000000 0.00 0.0 0.1666667 0.0000000 0.000 0.0000000
#> i7   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.1428571 0.000 0.0000000
#> i8   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.125 0.0000000
#> i9   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.1111111
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
hatinv_byname(r, keep = "colnames")
#>    c1  c2        c3   c4  c5
#> c1  1 0.0 0.0000000 0.00 0.0
#> c2  0 0.5 0.0000000 0.00 0.0
#> c3  0 0.0 0.3333333 0.00 0.0
#> c4  0 0.0 0.0000000 0.25 0.0
#> c5  0 0.0 0.0000000 0.00 0.2
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Commodities"
# This function also works with lists.
hatinv_byname(list(v, v), keep = "rownames")
#> [[1]]
#>     i1 i10  i2        i3   i4  i5        i6        i7    i8        i9
#> i1   1 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i10  0 0.1 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i2   0 0.0 0.5 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i3   0 0.0 0.0 0.3333333 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i4   0 0.0 0.0 0.0000000 0.25 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i5   0 0.0 0.0 0.0000000 0.00 0.2 0.0000000 0.0000000 0.000 0.0000000
#> i6   0 0.0 0.0 0.0000000 0.00 0.0 0.1666667 0.0000000 0.000 0.0000000
#> i7   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.1428571 0.000 0.0000000
#> i8   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.125 0.0000000
#> i9   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.1111111
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>     i1 i10  i2        i3   i4  i5        i6        i7    i8        i9
#> i1   1 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i10  0 0.1 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i2   0 0.0 0.5 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i3   0 0.0 0.0 0.3333333 0.00 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i4   0 0.0 0.0 0.0000000 0.25 0.0 0.0000000 0.0000000 0.000 0.0000000
#> i5   0 0.0 0.0 0.0000000 0.00 0.2 0.0000000 0.0000000 0.000 0.0000000
#> i6   0 0.0 0.0 0.0000000 0.00 0.0 0.1666667 0.0000000 0.000 0.0000000
#> i7   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.1428571 0.000 0.0000000
#> i8   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.125 0.0000000
#> i9   0 0.0 0.0 0.0000000 0.00 0.0 0.0000000 0.0000000 0.000 0.1111111
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
# Watch out for 0 values
v2 <- matrix(0:1, ncol = 1, dimnames = list(c(paste0("i", 0:1)), c("p1"))) %>%
  setrowtype("Industries") %>% setcoltype(NA)
# Produces singular matrix error
if (FALSE) v2 %>% hatize_byname() %>% invert_byname # \dontrun{}
# Handles 0 values well
hatinv_byname(v2, keep = "rownames")
#>               i0 i1
#> i0 1.797693e+308  0
#> i1  0.000000e+00  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
hatinv_byname(v2, inf_becomes = 42, keep = "rownames")
#>    i0 i1
#> i0 42  0
#> i1  0  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
hatinv_byname(v2, inf_becomes = NA, keep = "rownames")
#>    i0 i1
#> i0 NA  0
#> i1  0  1
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
# Deals with 1x1 matrices well, if the `keep` argument is set.
m <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
  setrowtype("Product -> Industry") %>% 
  setcoltype("Industry -> Product")
m %>% 
  hatinv_byname(keep = "rownames")
#>            r1
#> r1 0.02380952
#> attr(,"rowtype")
#> [1] "Product -> Industry"
#> attr(,"coltype")
#> [1] "Product -> Industry"
m %>% 
  hatinv_byname(keep = "colnames")
#>            c1
#> c1 0.02380952
#> attr(,"rowtype")
#> [1] "Industry -> Product"
#> attr(,"coltype")
#> [1] "Industry -> Product"
```
