# Convert a matrix or list of matrices between named form and indexed form

Matrices can be in named matrix form or triplet form. Named matrix form
is the usual representation for the `matsindf` package, wherein names
for rows and columns are included in the `dimnames` attribute of every
matrix or Matrix object, consuming memory. Typically, neither zero rows
nor zero columns are present. In some instances, many sparse matrices
with the same names will be created, leading to inefficiencies due to
dimname storage with every matrix object. It can be more
memory-efficient to store named matrices in integer triplet form, (a
table format with matrix data represented as a data frame with row
integer (i), column integer (j), and value (value) columns. (Row names
and column names can be stored as character string in the `i` and `j`
columns, too, called character triplet form.) Integer triplet form is
required for databases that do not recognize a matrix as a native
storage format. In integer triplet form, a separate (external) mapping
between row and column indices and row and column names must be
maintained. (In integer triplet form, it becomes the responsibility of
the caller to maintain a consistent mapping between row and column
indices and row and column names. However, rowtype and coltype are
retained as attributes of both integer and character triplet data
frames.) These functions convert from named matrix form to integer
triplet form (`to_triplet()`) and vice versa (`to_named_matrix()`) using
row and column name mappings supplied in the `index_map` argument.
`to_triplet()` and `to_named_matrix()` are inverses of each other, with
row and column order not necessarily preserved. See examples.

## Usage

``` r
to_triplet(
  a,
  index_map,
  retain_zero_structure = FALSE,
  row_index_colname = "i",
  col_index_colname = "j",
  value_colname = "value",
  rownames_colname = "rownames",
  colnames_colname = "colnames"
)

to_named_matrix(
  a,
  index_map,
  matrix_class = c("matrix", "Matrix"),
  row_index_colname = "i",
  col_index_colname = "j",
  value_colname = "value",
  .rnames = "rownames",
  .cnames = "colnames"
)
```

## Arguments

- a:

  For `to_triplet()`, a matrix or list of matrices to be converted to
  triplet form. For `to_named_matrix()`, a data frame or list of data
  frames in triplet form to be converted to named matrix form.

- index_map:

  A mapping between row and column names and row and column indices. See
  details.

- retain_zero_structure:

  A boolean that tells whether to retain the structure of zero matrices
  when creating triplets. Default is `FALSE`. See details.

- row_index_colname, col_index_colname:

  The names of row and column index columns in data frames. Defaults are
  "i" and "j", respectively.

- value_colname:

  The name of the value column in data frames. Default is "value".

- rownames_colname, colnames_colname:

  The name of row name and column name columns in data frames. Defaults
  are "rownames" and "colnames", respectively.

- matrix_class:

  One of "matrix" (standard) or "Matrix" (sparse) representation for
  matrices. Default is "matrix".

- .rnames, .cnames:

  Column names used internally. Defaults are "rownames" and "colnames".

## Value

`to_triplet()` returns `a` as a data frame or list of data frames in
triplet form. `to_named_matrix()` returns `a` as a named matrix or a
list of matrices in named form.

## Details

`index_map` must be one of the following:

- A single data frame of two columns, with one an integer column and the
  other a character column. When a single data frame, it will be applied
  to both rows and columns.

- An unnamed list of exactly two data frames, each data frame must have
  only an integer column and a character column. The first data frame of
  `index_map` is interpreted as the mapping between row names and row
  indices and the second data frame of `index_map` is interpreted as the
  mapping between column names and column indices.

- A named list of two or more data frames, in which the names of
  `index_map` are interpreted as row and column types, with named data
  frames applied as the mapping for the associated row or column type.
  For example the data frame named "Industry" would be applied to the
  dimension (row or column) with an "Industry" type. When both row and
  column have "Industry" type, the "Industry" mapping is applied to
  both. When sending named data frames in `index_map`, `a` must have
  both a row type and a column type. If an appropriate mapping cannot be
  found in `index_map`, an error is raised. Both matching data frames
  must have only an integer column and a character column.

When converting to indexed form, rowtype and coltype are retained as
attributes. See
[`rowtype()`](https://matthewheun.github.io/matsbyname/reference/rowtype.md)
and
[`coltype()`](https://matthewheun.github.io/matsbyname/reference/coltype.md).

If any indices are unavailable in the `index_map`, an error is raised.
It is an error to repeat a name in the name column of an `index_map`
data frame. It is an error to repeat an index in the index column of an
`index_map` data frame.

If `a` is `NULL`, `NULL` is returned. If `a` is a list and any member of
the list is `NULL`, `NULL` is returned in that position.

By default, `to_triplet()` will return a zero-row data frame when `a` is
a zero matrix. Set `retain_zero_structure = TRUE` to return all entries
in the zero matrix.

## Examples

``` r
triplet <- data.frame(i = as.integer(c(9, 7, 5, 9, 7, 5)), 
                      j = as.integer(c(3, 3, 3, 4, 4, 4)), 
                      value = c(1, 2, 3, 4, 5, 6)) |> 
  setrowtype("rows") |> setcoltype("cols")
triplet
#>   i j value
#> 1 9 3     1
#> 2 7 3     2
#> 3 5 3     3
#> 4 9 4     4
#> 5 7 4     5
#> 6 5 4     6
rowtype(triplet)
#> [1] "rows"
coltype(triplet)
#> [1] "cols"
# We have more indices than actual entries in the martix
r_indices <- data.frame(names = paste0("r", 1:101),
                        indices = 1:101)
head(r_indices)
#>   names indices
#> 1    r1       1
#> 2    r2       2
#> 3    r3       3
#> 4    r4       4
#> 5    r5       5
#> 6    r6       6
c_indices <- data.frame(names = paste0("c", 1:101),
                        indices = 1:101)
head(c_indices)
#>   names indices
#> 1    c1       1
#> 2    c2       2
#> 3    c3       3
#> 4    c4       4
#> 5    c5       5
#> 6    c6       6
# Names are interpreted as row and column types
indices <- list(cols = c_indices, rows = r_indices)
named <- to_named_matrix(triplet, indices)
named
#>    c3 c4
#> r5  3  6
#> r7  2  5
#> r9  1  4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
triplet2 <- to_triplet(named, indices)

# Although not in the same row order, 
# triplet and triplet2 are the same.
triplet2
#> # A tibble: 6 × 3
#>       i     j value
#>   <int> <int> <dbl>
#> 1     5     3     3
#> 2     7     3     2
#> 3     9     3     1
#> 4     5     4     6
#> 5     7     4     5
#> 6     9     4     4
rowtype(triplet2)
#> [1] "rows"
coltype(triplet2)
#> [1] "cols"
# And the same matrix can be recovered from triplet2
to_named_matrix(triplet2, indices)
#>    c3 c4
#> r5  3  6
#> r7  2  5
#> r9  1  4
#> attr(,"rowtype")
#> [1] "rows"
#> attr(,"coltype")
#> [1] "cols"
```
