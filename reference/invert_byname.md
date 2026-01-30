# Invert a matrix

This function transposes row and column names as well as row and column
types. Rows and columns of `a` are sorted prior to inverting.

## Usage

``` r
invert_byname(a, method = c("solve", "QR", "SVD"), tol = .Machine$double.eps)
```

## Arguments

- a:

  The matrix to be inverted. `a` must be square.

- method:

  One of "solve", "QR", or "SVD". Default is "solve". See details.

- tol:

  The tolerance for detecting linear dependencies in the columns of `a`.
  Default is `.Machine$double.eps`.

## Value

The inversion of `a`.

## Details

The `method` argument specifies which method should be used for
calculating the inverse. "solve" uses
[`base::solve()`](https://rdrr.io/r/base/solve.html) and the value of
`tol`. "QR" uses [`base::solve.qr()`](https://rdrr.io/r/base/qr.html)
and the value of `tol`. "SVD" uses
[`matrixcalc::svd.inverse()`](https://rdrr.io/pkg/matrixcalc/man/svd.inverse.html),
ignoring the `tol` argument.

Both `tol` and `method` should be a single values and apply to all
matrices in `a`.

If `a` is a singular matrix, names of zero rows and columns are reported
in the error message.

## Examples

``` r
m <- matrix(c(10,0,0,100), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) %>%
  setrowtype("Industry") %>% setcoltype("Commodity")
m
#>    c1  c2
#> i1 10   0
#> i2  0 100
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Commodity"
invert_byname(m)
#>     i1   i2
#> c1 0.1 0.00
#> c2 0.0 0.01
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
matrixproduct_byname(m, invert_byname(m))
#>    i1 i2
#> i1  1  0
#> i2  0  1
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Industry"
matrixproduct_byname(invert_byname(m), m)
#>    c1 c2
#> c1  1  0
#> c2  0  1
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Commodity"
invert_byname(list(m,m))
#> [[1]]
#>     i1   i2
#> c1 0.1 0.00
#> c2 0.0 0.01
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
#> 
#> [[2]]
#>     i1   i2
#> c1 0.1 0.00
#> c2 0.0 0.01
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
#> 
invert_byname(m, method = "QR")
#>     i1   i2
#> c1 0.1 0.00
#> c2 0.0 0.01
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
invert_byname(m, method = "SVD")
#>     i1   i2
#> c1 0.1 0.00
#> c2 0.0 0.01
#> attr(,"rowtype")
#> [1] "Commodity"
#> attr(,"coltype")
#> [1] "Industry"
```
