# Calculate eigenvectors of a matrix

Calculate the eigenvectors of a matrix or a list of matrices.

## Usage

``` r
eigenvectors_byname(a)
```

## Arguments

- a:

  A matrix or list of matrices.

## Value

A matrix whose columns are the eigenvectors of `a`.

## Details

This function pairs with
[`eigenvalues_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvalues_byname.md);
the first column of the resulting matrix is the eigenvector for the
first eigenvalue reported by
[`eigenvalues_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvalues_byname.md).
The second column of the resulting matrix is the eigenvector for the
second eigenvalue reported by
[`eigenvalues_byname()`](https://matthewheun.github.io/matsbyname/reference/eigenvalues_byname.md).
Etc.

Internally, this function uses
[`base::eigen()`](https://rdrr.io/r/base/eigen.html).

[`complete_rows_cols()`](https://matthewheun.github.io/matsbyname/reference/complete_rows_cols.md)
is called prior to calculating the eigenvectors.

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
eigenvectors_byname(m)
#>            [,1]       [,2]       [,3]
#> [1,]  0.4574957  0.4082483 -0.5773503
#> [2,]  0.7624929 -0.8164966 -0.5773503
#> [3,] -0.4574957  0.4082483  0.5773503
eigenvectors_byname(list(m, 2*m))
#> [[1]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.4574957  0.4082483 -0.5773503
#> [2,]  0.7624929 -0.8164966 -0.5773503
#> [3,] -0.4574957  0.4082483  0.5773503
#> 
#> [[2]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.4574957  0.4082483 -0.5773503
#> [2,]  0.7624929 -0.8164966 -0.5773503
#> [3,] -0.4574957  0.4082483  0.5773503
#> 
DF <- tibble::tibble(m_col = list(m, 2*m)) %>% 
  dplyr::mutate(
    eigen_col = eigenvectors_byname(m_col)
  )
DF$eigen_col[[1]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.4574957  0.4082483 -0.5773503
#> [2,]  0.7624929 -0.8164966 -0.5773503
#> [3,] -0.4574957  0.4082483  0.5773503
DF$eigen_col[[2]]
#>            [,1]       [,2]       [,3]
#> [1,]  0.4574957  0.4082483 -0.5773503
#> [2,]  0.7624929 -0.8164966 -0.5773503
#> [3,] -0.4574957  0.4082483  0.5773503
```
