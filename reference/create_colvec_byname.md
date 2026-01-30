# Create column vectors from data

This function takes data in the `.dat` and creates column vectors.

## Usage

``` r
create_colvec_byname(
  .dat,
  dimnames = NA,
  colname = NA,
  matrix_class = c("matrix", "Matrix")
)
```

## Arguments

- .dat:

  Data to be converted to column vectors.

- dimnames:

  The dimension names to be used for creating the column vector, in a
  list format, or as a data frame column containing a list of the
  dimension names to be used for each observation.

- colname:

  The name of the column of the colvector.

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

A column vector, a list of column vectors, or a data frame column of
column vectors, depending on the value of `.dat` and `class`.

## Details

The row and column names in the resulting column vector are taken from
the names of `.dat` and `colname`. If set, `dimnames` overrides the
names of `.dat` and `colname`.

This function is a "byname" function that can accept a single number, a
vector, a list, or a data frame in `.dat`.

Row types and column types are taken from the row type and column type
attributes of `.dat`.

## Examples

``` r
# Works with single numbers
create_colvec_byname(c(r1 = 1) %>% setrowtype("rt") %>% setcoltype("ct"), 
                     colname = "r1")
#>    r1
#> r1  1
#> attr(,"rowtype")
#> [1] "rt"
#> attr(,"coltype")
#> [1] "ct"
# Works with vectors
create_colvec_byname(c(r1 = 1, r2 = 2), colname = "c1")
#>    c1
#> r1  1
#> r2  2
# Works with a list
create_colvec_byname(list(c(r1 = 1, r2 = 2), c(R1 = 3, R2 = 4, R3 = 5)), 
                     colname = list("c1", "C1"))
#> [[1]]
#>    c1
#> r1  1
#> r2  2
#> 
#> [[2]]
#>    C1
#> R1  3
#> R2  4
#> R3  5
#> 
# Works in a tibble, too.
# (Must be a tibble, not a data frame, so that names are preserved.)
dat <- list(c(r1 = 1, r2 = 2),
            c(R1 = 2, R2 = 3), 
            c(r1 = 1, r2 = 2, r3 = 3, r4 = 4, r5 = 5, r6 = 6))
cnms <- list("c1", "C1", "c1")
df1 <- tibble::tibble(dat, cnms)
df1
#> # A tibble: 3 × 2
#>   dat       cnms     
#>   <list>    <list>   
#> 1 <dbl [2]> <chr [1]>
#> 2 <dbl [2]> <chr [1]>
#> 3 <dbl [6]> <chr [1]>
df1 <- df1 %>%
  dplyr::mutate(
    colvec_col = create_colvec_byname(dat, colname = cnms)
  )
df1$colvec_col[[1]]
#>    c1
#> r1  1
#> r2  2
df1$colvec_col[[2]]
#>    C1
#> R1  2
#> R2  3
df1$colvec_col[[3]]
#>    c1
#> r1  1
#> r2  2
#> r3  3
#> r4  4
#> r5  5
#> r6  6
```
