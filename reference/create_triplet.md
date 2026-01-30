# Create a triplet from a matrix

Creates a data frame triplet with columns `i_col`, `j_col`, and
`value_col` from matrix `m`. Zero entries are not reported. `i` and `j`
integers are directly from `m` and not referenced to any global set of
`i` and `j` values.

## Usage

``` r
create_triplet(
  m,
  retain_zero_structure = FALSE,
  i_col = "i",
  j_col = "j",
  value_col = "value"
)
```

## Arguments

- m:

  A `matrix` or `Matrix` to be converted to triplet form.

- retain_zero_structure:

  A boolean that tells whether to retain the structure of zero matrices.
  Default is `FALSE`.

- i_col, j_col, value_col:

  String names of i, j, and x columns.

## Value

A `tibble` triplet representation of `m`.

## Details

When `m` is a zero matrix, a zero-row data frame is returned by default
(`retain_zero_structure = FALSE`). But when `retain_zero_structure` is
`TRUE`, zero entries are reported for all rows and columns, thereby
preserving the structure of the matrix.
