# Prepare a vector argument

This is a helper function for many `*_byname` functions.

## Usage

``` r
prep_vector_arg(a, vector_arg)
```

## Arguments

- a:

  A matrix or list of matrices.

- vector_arg:

  The vector argument over which to apply a calculation.

## Value

`vector_arg`, possibly modified when `a` is a list.

## Details

It is potentially ambiguous to specify a vector or matrix argument, say,
`margin = c(1, 2)` when applying the `*_byname` functions to unary list
of `a`. Rather, one should specify, say, `margin = list(c(1, 2))` to
avoid ambiguity. If `a` is a list, `vector_arg` is not a list and has
length \> 1 and length not equal to the length of a, this function
returns a list value for `vector_arg`. If `a` is not a list and
`vector_arg` is a list, this function returns an un-recursive, unlisted
version of `vector_arg`.

Note that if `vector_arg` is a single matrix, it is automatically
enclosed by a list when `a` is a list.

## Examples

``` r
m <- matrix(c(2, 2))
prep_vector_arg(m, vector_arg = c(1,2))
#> [1] 1 2
prep_vector_arg(list(m), vector_arg = c(1,2))
#> [[1]]
#> [1] 1 2
#> 
prep_vector_arg(list(m, m), vector_arg = c(1,2))
#> [1] 1 2
prep_vector_arg(list(m, m, m), vector_arg = c(1,2))
#> [[1]]
#> [1] 1 2
#> 
```
