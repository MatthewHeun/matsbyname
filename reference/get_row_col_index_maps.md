# Figure out row and column index maps

The `index_map` argument can take several forms. This function figures
out (for a given `a_mat`) the index maps for rows (first data frame in
the return list) and columns (second data frame in the return list).

## Usage

``` r
get_row_col_index_maps(a_mat, ind_maps)
```

## Arguments

- a_mat:

  A matrix for which index maps should be determined.

- ind_maps:

  A single data frame or a list of two or more data frames of potential
  index maps.

## Value

A list of two data frames. The first data frame is the index map for the
rows of `a_mat`. The second data frame is the index map for the columns
of `a_mat`.

## Details

`ind_maps` can be a single data frame, in which case the single data
frame will be applied to both rows and columns of `a_mat`.

`ind_maps` can also be a 2-item list, in which case the first item is
applied to rows and the second item is applied to columns.

Finally, `ind_maps` can be a named list of length 2 or more, in which
case the names are interpreted as row or column types. Names in the
`ind_maps` list are matched to row and column types and applied as
required.

This is a non-exported function meant only for internal use.
