# Creates a diagonal "hat" matrix from a vector

A "hat" matrix (or a diagonal matrix) is one in which the only non-zero
elements are along on the diagonal. To "hatize" a vector is to place its
elements on the diagonal of an otherwise-zero square matrix. `v` must be
a matrix object with at least one of its two dimensions of length 1
(i.e., a vector). The names on both dimensions of the hatized matrix are
the same and taken from the dimension of `v` that is *not* 1. Note that
the row names and column names are sorted prior to forming the "hat"
matrix.

## Usage

``` r
hatize_byname(v, keep = NULL)
```

## Arguments

- v:

  The vector from which a "hat" matrix is to be created.

- keep:

  One of "rownames" or "colnames" or `NULL`. If `NULL`, the default,
  names are kept from the dimension that is not size 1.

## Value

A square "hat" matrix with size equal to the length of `v`.

## Details

Hatizing a 1x1 vector is potentially undefined. The argument `keep`
determines whether to keep "rownames" or "colnames". By default `keep`
is `NULL`, meaning that the function should attempt to figure out which
dimension's names should be used for the hatized matrix on output. If
vector `v` could ever be 1x1, it is best to set a value for `keep` when
writing code that calls `hatize_byname()`.

If the caller specifies `keep = "colnames"` when `v` is a column vector,
an error is thrown. If the caller specifies `keep = "rownames"` when `v`
is a row vector, an error is thrown.

## Examples

``` r
v <- matrix(1:10, ncol = 1, dimnames = list(c(paste0("i", 1:10)), c("c1"))) %>%
  setrowtype("Industries") %>% setcoltype(NA)
v
#>     c1
#> i1   1
#> i2   2
#> i3   3
#> i4   4
#> i5   5
#> i6   6
#> i7   7
#> i8   8
#> i9   9
#> i10 10
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] NA
hatize_byname(v, keep = "rownames")
#>     i1 i10 i2 i3 i4 i5 i6 i7 i8 i9
#> i1   1   0  0  0  0  0  0  0  0  0
#> i10  0  10  0  0  0  0  0  0  0  0
#> i2   0   0  2  0  0  0  0  0  0  0
#> i3   0   0  0  3  0  0  0  0  0  0
#> i4   0   0  0  0  4  0  0  0  0  0
#> i5   0   0  0  0  0  5  0  0  0  0
#> i6   0   0  0  0  0  0  6  0  0  0
#> i7   0   0  0  0  0  0  0  7  0  0
#> i8   0   0  0  0  0  0  0  0  8  0
#> i9   0   0  0  0  0  0  0  0  0  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
r <- matrix(1:5, nrow = 1, dimnames = list(c("r1"), c(paste0("c", 1:5)))) %>%
  setrowtype(NA) %>% setcoltype("Commodities")
r
#>    c1 c2 c3 c4 c5
#> r1  1  2  3  4  5
#> attr(,"rowtype")
#> [1] NA
#> attr(,"coltype")
#> [1] "Commodities"
hatize_byname(r, keep = "colnames")
#>    c1 c2 c3 c4 c5
#> c1  1  0  0  0  0
#> c2  0  2  0  0  0
#> c3  0  0  3  0  0
#> c4  0  0  0  4  0
#> c5  0  0  0  0  5
#> attr(,"rowtype")
#> [1] "Commodities"
#> attr(,"coltype")
#> [1] "Commodities"
# This also works with lists.
hatize_byname(list(v, v), keep = "rownames")
#> [[1]]
#>     i1 i10 i2 i3 i4 i5 i6 i7 i8 i9
#> i1   1   0  0  0  0  0  0  0  0  0
#> i10  0  10  0  0  0  0  0  0  0  0
#> i2   0   0  2  0  0  0  0  0  0  0
#> i3   0   0  0  3  0  0  0  0  0  0
#> i4   0   0  0  0  4  0  0  0  0  0
#> i5   0   0  0  0  0  5  0  0  0  0
#> i6   0   0  0  0  0  0  6  0  0  0
#> i7   0   0  0  0  0  0  0  7  0  0
#> i8   0   0  0  0  0  0  0  0  8  0
#> i9   0   0  0  0  0  0  0  0  0  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>     i1 i10 i2 i3 i4 i5 i6 i7 i8 i9
#> i1   1   0  0  0  0  0  0  0  0  0
#> i10  0  10  0  0  0  0  0  0  0  0
#> i2   0   0  2  0  0  0  0  0  0  0
#> i3   0   0  0  3  0  0  0  0  0  0
#> i4   0   0  0  0  4  0  0  0  0  0
#> i5   0   0  0  0  0  5  0  0  0  0
#> i6   0   0  0  0  0  0  6  0  0  0
#> i7   0   0  0  0  0  0  0  7  0  0
#> i8   0   0  0  0  0  0  0  0  8  0
#> i9   0   0  0  0  0  0  0  0  0  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Industries"
#> 
# A 1x1 column vector is a degenerate case. 
# Row names and rowtype are transferred to the column.
matrix(42, nrow = 1, ncol = 1, dimnames = list("r1")) %>% 
  setrowtype("Product -> Industry") %>% 
  hatize_byname(keep = "rownames")
#>    r1
#> r1 42
#> attr(,"rowtype")
#> [1] "Product -> Industry"
#> attr(,"coltype")
#> [1] "Product -> Industry"
# A 1x1 row vector is a degenerate case. 
# Column names and coltype are transferred to the row.
matrix(42, nrow = 1, ncol = 1, dimnames = list(NULL, "c1")) %>% 
  setcoltype("Industry -> Product") %>% 
  hatize_byname(keep = "colnames")
#>    c1
#> c1 42
#> attr(,"coltype")
#> [1] "Industry -> Product"
#> attr(,"rowtype")
#> [1] "Industry -> Product"
# A 1x1 matrix with both row and column names generates a failure.
if (FALSE) { # \dontrun{
matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
  setrowtype("Product -> Industry") %>% 
  setcoltype("Industry -> Product") %>% 
  hatize_byname()
} # }
# But you could specify which you want keep, row names or column names.
m <- matrix(42, nrow = 1, ncol = 1, dimnames = list("r1", "c1")) %>% 
  setrowtype("Product -> Industry") %>% 
  setcoltype("Industry -> Product")
m
#>    c1
#> r1 42
#> attr(,"rowtype")
#> [1] "Product -> Industry"
#> attr(,"coltype")
#> [1] "Industry -> Product"
m %>% 
  hatize_byname(keep = "rownames")
#>    r1
#> r1 42
#> attr(,"rowtype")
#> [1] "Product -> Industry"
#> attr(,"coltype")
#> [1] "Product -> Industry"
m %>% 
  hatize_byname(keep = "colnames")
#>    c1
#> c1 42
#> attr(,"rowtype")
#> [1] "Industry -> Product"
#> attr(,"coltype")
#> [1] "Industry -> Product"
```
