# Calculate eigenvalues of a matrix

Calculate the eigenvalues of a matrix or a list of matrices.

## Usage

``` r
eigenvalues_byname(a)
```

## Arguments

- a:

  A matrix or list of matrices.

## Value

A vector of eigenvalues.

## Details

This function pairs with
[`eigenvectors_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvectors_byname.md);
the first value of the result is the eigenvalue for the eigenvector
reported in the first column of the result from
[`eigenvectors_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvectors_byname.md).
The second value of the result is the eigenvalue for the eigenvector
reported in the second column of the result from
[`eigenvectors_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvectors_byname.md).
Etc.

Internally, this function uses `base::eigen(only.values = TRUE)`.

[`complete_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/complete_rows_cols.md)
is called prior to calculating the eigenvalues.

## Examples

``` r
m <- matrix(c( 4,  6, 10, 
               3, 10, 13, 
              -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
            dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
m
#>    p1 p2 p3
#> p1  4  6 10
#> p2  3 10 13
#> p3 -2 -6 -8
eigenvalues_byname(m)
#> [1]  4.000000e+00  2.000000e+00 -8.364134e-16
eigenvalues_byname(list(m, 2*m))
#> [[1]]
#> [1]  4.000000e+00  2.000000e+00 -8.364134e-16
#> 
#> [[2]]
#> [1]  8.000000e+00  4.000000e+00 -1.672827e-15
#> 
DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
  dplyr::mutate(
    eigen_col = eigenvalues_byname(m_col)
  )
DF$eigen_col[[1]]
#> [1]  4.000000e+00  2.000000e+00 -8.364134e-16
DF$eigen_col[[2]]
#> [1]  8.000000e+00  4.000000e+00 -1.672827e-15
```
