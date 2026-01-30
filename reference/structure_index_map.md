# Set the structure of an index map

Index maps must be a data frame with one integer column and one
character string column. This function verifies the structure and
ensures that the integer column is first.

## Usage

``` r
structure_index_map(index_map)
```

## Arguments

- index_map:

  The index map data frame to be structured.

## Value

A data frame with an integer column as the first column and a character
string column as the second column.

## Details

This is a non-exported function meant only for internal use.
