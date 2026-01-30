# Create row vectors from data

This function takes data in the `.dat` and creates row vectors.

## Usage

``` r
create_rowvec_byname(
  .dat,
  dimnames = NA,
  rowname = NA,
  matrix_class = c("matrix", "Matrix")
)
```

## Arguments

- .dat:

  Data to be converted to row vectors.

- dimnames:

  The dimension names to be used for creating the row vector, in a list
  format, or as a data frame column containing a list of the dimension
  names to be used for each observation.

- rowname:

  The name of the row of the row vector.

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

A row vector, a list of row vectors, or a data frame column of row
vectors, depending on the values of `.dat` and `class`.

## Details

The row and column names in the resulting row vector are taken from
`rowname` and the names of `.dat`. If set, `dimnames` overrides
`rowname` and the names of `.dat`.

Row types and column types are taken from the row type and column type
attributes of `.dat`.

This function is a "byname" function that can accept a single number, a
vector, a list, or a data frame in `.dat`.

## Examples

``` r
# Works with single numbers
create_rowvec_byname(c(c1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), rowname = "r1")
#>    c1
#> r1  1
#> attr(,"rowtype")
#> [1] "rt"
#> attr(,"coltype")
#> [1] "ct"
# Works with vectors
create_rowvec_byname(c(c1 = 1, c2 = 2), rowname = "r1")
#>    c1 c2
#> r1  1  2
# Works with a list
create_rowvec_byname(list(c(c1 = 1, c2 = 2), c(C1 = 3, C2 = 4, C3 = 5)), 
                     rowname = list("r1", "R1"))
#> [[1]]
#>    c1 c2
#> r1  1  2
#> 
#> [[2]]
#>    C1 C2 C3
#> R1  3  4  5
#> 
# Works in a tibble, too.
# (Must be a tibble, not a data frame, so that names are preserved.)
dat <- list(c(c1 = 1),
            c(C1 = 2, C2 = 3), 
            c(c1 = 1, c2 = 2, c3 = 3, c4 = 4, c5 = 5, c6 = 6))
rnms <- list("r1", "R1", "r1")
df1 <- tibble::tibble(dat, rnms)
df1
#> # A tibble: 3 × 2
#>   dat       rnms     
#>   <list>    <list>   
#> 1 <dbl [1]> <chr [1]>
#> 2 <dbl [2]> <chr [1]>
#> 3 <dbl [6]> <chr [1]>
df1 <- df1 %>%
  dplyr::mutate(
    rowvec_col = create_rowvec_byname(dat, rowname = rnms)
  )
df1$rowvec_col[[1]]
#>    c1
#> r1  1
df1$rowvec_col[[2]]
#>    C1 C2
#> R1  2  3
df1$rowvec_col[[3]]
#>    c1 c2 c3 c4 c5 c6
#> r1  1  2  3  4  5  6
```
