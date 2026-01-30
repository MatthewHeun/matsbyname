# Create a "byname" matrix from a vector

This function creates a "byname" matrix, or list of matrices, from
`.dat`, depending on the input arguments. This function is similar to
[`matrix()`](https://rdrr.io/r/base/matrix.html), but with "byname"
characteristics.

## Usage

``` r
create_matrix_byname(
  .dat,
  nrow,
  ncol,
  byrow = FALSE,
  dimnames,
  matrix_class = c("matrix", "Matrix")
)
```

## Arguments

- .dat:

  The data to be used to create the matrix, in a list format, or as a
  data frame column containing a list of the data to be used for each
  observation.

- nrow:

  The number of rows to be used to create the matrix, in a list format,
  or as a data frame column containing a list of the number of rows to
  be used for each observation.

- ncol:

  The number of columns to be used to create the matrix, in a list
  format, or as a data frame column containing a list of the number of
  columns to be used for each observation.

- byrow:

  The argument stating whether the matrix should be filled by rows or by
  columns (FALSE by column, TRUE by row), in a list format, or as a data
  frame column containing a list of the byrow argument for each
  observation. Default is `FALSE.`

- dimnames:

  The dimension names to be used for creating the matrices, in a list
  format, or as a data frame column containing a list of the dimension
  names to be used for each observation.

- matrix_class:

  One of "matrix" or "Matrix". "matrix" creates a
  [`base::matrix`](https://rdrr.io/r/base/matrix.html) object with the
  [`matrix()`](https://rdrr.io/r/base/matrix.html) function. "Matrix"
  creates a
  [`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html) object
  using the
  [`matsbyname::Matrix()`](https://matthewheun.github.io/matsbyname/reference/Matrix.md)
  function. This could be a sparse matrix. Default is "matrix".

## Value

A matrix, list of matrices, or column in a data frame, depending on the
input arguments.

## Details

Row and column names are taken from the `dimnames` argument.

Any row or column type information on `.dat` is preserved on output.

The created object(s) can be of type
[`base::matrix`](https://rdrr.io/r/base/matrix.html) or
[`Matrix::Matrix`](https://rdrr.io/pkg/Matrix/man/Matrix.html), the
latter enables sparse objects to save both memory and disk.

## Examples

``` r
create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                     dimnames = list(c("r1", "r2"), "c1"))
#>    c1
#> r1  1
#> r2  2
create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                     dimnames = list(list("r1", "c1"), list("R1", "C1")))
#> [[1]]
#>    c1
#> r1  1
#> 
#> [[2]]
#>    C1
#> R1  2
#> 
```
