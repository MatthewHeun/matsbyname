# Create a Matrix amenable to use in the `matsbyname` package

The `matsbyname` package uses `Matrix` objects for its default data
representation, taking advantage of the sparse matrix capabilities of
`Matrix` compared to the base `matrix` class. This function routes to
[`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html), with
some important differences. See details.

## Usage

``` r
Matrix(
  data = NA,
  nrow = 1,
  ncol = 1,
  byrow = FALSE,
  dimnames = base::dimnames(data),
  sparse = NULL,
  doDiag = FALSE,
  forceCheck = FALSE,
  rowtype = matsbyname::rowtype(data),
  coltype = matsbyname::coltype(data)
)
```

## Arguments

- data:

  An optional numeric data vector or `matrix.`

- nrow:

  When `data` is not a `matrix` or a `Matrix`, the desired number of
  rows. Default is `1`.

- ncol:

  When `data` is not a `matrix` or a `Matrix`, the desired number of
  columns. Default is `1`.

- byrow:

  A boolean. If `FALSE` (the default) the Matrix is filled by columns,
  otherwise the Matrix is filled by rows.

- dimnames:

  A dimnames attribute for the Matrix: a list of two character
  components. Default is `base::dimnames(data)`.

- sparse:

  A boolean or `NULL`. Specifies whether the result should be sparse or
  not. By default (`NULL`), the Matrix is made sparse when more than
  half of the entries are `0`.

- doDiag:

  A boolean indicating if a `diagonalMatrix` object should be returned
  when the resulting Matrix is diagonal (mathematically). Default is
  `FALSE`, which is different from
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html).

- forceCheck:

  A boolean indicating if the checks for structure should happen when
  `data` is already a `Matrix` object. Default is `FALSE`.

- rowtype:

  The rowtype for the result. Default is `matsbyname::rowtype(data)`.

- coltype:

  The coltype for the result. Default is `matsbyname::coltype(data)`.

## Value

A `Matrix` object.

## Details

This function NEVER creates a symmetric matrix (e.g., `dsCMatrix`,
`dsyMatrix`, `dsRMatrix`, `lsyMatrix`, `nsyMatrix`), because symmetric
matrices do not respect some future changes to `dimnames`, which can
cause information loss in the `matsbyname` context. A non-symmetric
`Matrix` is assured by calling `as(out, "generalMatrix")` on the
outgoing `Matrix` object.

This function enables setting row and column types at the time of
construction with the `rowtype` and `coltype` arguments.

This function has different defaults compared to
[`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html),
including

- Here, the default for `doDiag` is `FALSE`, while the default for
  `doDiag` is `TRUE` for
  [`Matrix::Matrix()`](https://rdrr.io/pkg/Matrix/man/Matrix.html).

- Preserves rowtype and coltype on `data`.

This function is vectorized over a list of `matrix` objects supplied to
`data`. See examples.

## Examples

``` r
# matsbyname::Matrix() will not create a Matrix with a symmetric subclass.
# dgCMatrix is a general matrix.
matsbyname::Matrix(c(1, 0, 2, 
                     0, 0, 0, 
                     2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>           
#> [1,] 1 . 2
#> [2,] . . .
#> [3,] 2 . .
# But Matrix::Matrix() will create a symmetric matrix.
# dsCMatrix is a symmetric matrix.
Matrix::Matrix(c(1, 0, 2, 
                 0, 0, 0, 
                 2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
#> 3 x 3 sparse Matrix of class "dsCMatrix"
#>           
#> [1,] 1 . 2
#> [2,] . . .
#> [3,] 2 . .
# matsbyname::Matrix() will not create a diagonal matrix.
# dgeMatrix is a general matrix.
matsbyname::Matrix(c(1, 0, 
                     0, 1), byrow = TRUE, nrow = 2, ncol = 2)
#> 2 x 2 Matrix of class "dgeMatrix"
#>      [,1] [,2]
#> [1,]    1    0
#> [2,]    0    1
# But Matrix::Matrix() will create a diagonal matrix.
# ddiMatrix is a diagonal matrix.
Matrix::Matrix(c(1, 0, 
                 0, 1), byrow = TRUE, nrow = 2, ncol = 2)
#> 2 x 2 diagonal matrix of class "ddiMatrix"
#>      [,1] [,2]
#> [1,]    1    .
#> [2,]    .    1
# This function is vectorized over lists of `matrix` objects in `data`.
m <- matrix(c(1, 0, 2, 
              0, 0, 0, 
              2, 0, 0), byrow = TRUE, nrow = 3, ncol = 3)
matsbyname::Matrix(list(m, m))
#> [[1]]
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>           
#> [1,] 1 . 2
#> [2,] . . .
#> [3,] 2 . .
#> 
#> [[2]]
#> 3 x 3 sparse Matrix of class "dgCMatrix"
#>           
#> [1,] 1 . 2
#> [2,] . . .
#> [3,] 2 . .
#> 
```
