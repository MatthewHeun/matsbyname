# Calculate the singular value decomposition of a matrix

The singular value decomposition decomposes matrix **A** into **A** =
**U** **D** **V**^T, where **U** and **V** are orthogonal matrices and
**D** is a diagonal matrix. **U** is the left singular vectors of **A**.
**V** is the right singular vectors of **A**.

## Usage

``` r
svd_byname(a, which = c("d", "u", "v"))
```

## Arguments

- a:

  A matrix to be decomposed.

- which:

  The matrix to be returned. Default is "d". See details.

## Value

A matrix of the singular value decomposition of `a`.

## Details

`which` determines the part of the singular value decomposition to be
returned. "d" (default) gives the **D** matrix. "u" gives the **U**
matrix. "v" gives the **V** matrix (not its transpose).

## Examples

``` r
A = matrix(c(4, 0, 
             3, -5), nrow = 2, ncol = 2, byrow = TRUE, 
           dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
  setrowtype("Product") %>% setcoltype("Industry")
A
#>    c1 c2
#> r1  4  0
#> r2  3 -5
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
svd_byname(A) # Gives D matrix, by default
#>          c1       c2
#> r1 6.324555 0.000000
#> r2 0.000000 3.162278
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
svd_byname(A, which = "d")
#>          c1       c2
#> r1 6.324555 0.000000
#> r2 0.000000 3.162278
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
svd_byname(A, which = "u")
#>            r1         r2
#> r1 -0.4472136 -0.8944272
#> r2 -0.8944272  0.4472136
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Product"
svd_byname(A, which = "v")
#>            c1         c2
#> c1 -0.7071068 -0.7071068
#> c2  0.7071068 -0.7071068
#> attr(,"rowtype")
#> [1] "Industry"
#> attr(,"coltype")
#> [1] "Industry"
```
