# Complete matrices relative to one another and sort into same row, column order

Completes each matrix relative to each other, thereby assuring that both
matrices have same row and column names. Missing rows and columns
(relative to the other matrix) are filled with `fill`. Thereafter, rows
and columns of the matrices are sorted such that they are in the same
order (by name). To complete rows of `a` relative to columns of `b` (and
vice versa),
[`transpose_byname()`](https://matthewheun.github.io/matsbyname/reference/transpose_byname.md)
the `b` argument.

## Usage

``` r
complete_and_sort(
  a,
  b,
  fill = 0,
  margin = c(1, 2),
  roworder = NA,
  colorder = NA
)
```

## Arguments

- a:

  The first matrix

- b:

  The second (optional) matrix.

- fill:

  rows and columns added to `a` and `b` will contain the value `fill` (a
  double).

- margin:

  Specifies the dimension(s) of `a` and `b` over which completing and
  sorting will occur

- roworder:

  Specifies a custom ordering for rows of returned matrices. Unspecified
  rows are dropped.

- colorder:

  Specifies a custom ordering for columns of returned matrices.
  Unspecified columns are dropped.

## Value

A named list containing completed and sorted versions of `a` and `b`.

## Details

`margin` has nearly the same semantic meaning as in
[`apply()`](https://rdrr.io/r/base/apply.html). For rows only, give `1`;
for columns only, give `2`; for both rows and columns, give `c(1,2)`,
the default value.

If only `a` is specified, rows of `a` are completed and sorted relative
to columns of `a`. If neither `a` nor `b` have dimnames, `a` and `b` are
returned unmodified. If only one of `a` or `b` has dimnames, an error is
thrown.

## Examples

``` r
m1 <- matrix(c(1:6), nrow=3, dimnames = list(c("r1", "r2", "r3"), c("c2", "c1")))
m2 <- matrix(c(7:12), ncol=3, dimnames = list(c("r3", "r4"), c("c2", "c3", "c4")))
complete_and_sort(m1)
#>    c1 c2 r1 r2 r3
#> c1  0  0  0  0  0
#> c2  0  0  0  0  0
#> r1  4  1  0  0  0
#> r2  5  2  0  0  0
#> r3  6  3  0  0  0
complete_and_sort(m1, m2)
#> $a
#>    c1 c2 c3 c4
#> r1  4  1  0  0
#> r2  5  2  0  0
#> r3  6  3  0  0
#> r4  0  0  0  0
#> 
#> $b
#>    c1 c2 c3 c4
#> r1  0  0  0  0
#> r2  0  0  0  0
#> r3  0  7  9 11
#> r4  0  8 10 12
#> 
complete_and_sort(m1, m2, roworder = c("r3", "r2", "r1"))
#> $a
#>    c1 c2 c3 c4
#> r3  6  3  0  0
#> r2  5  2  0  0
#> r1  4  1  0  0
#> 
#> $b
#>    c1 c2 c3 c4
#> r3  0  7  9 11
#> r2  0  0  0  0
#> r1  0  0  0  0
#> 
complete_and_sort(m1, m2, colorder = c("c4", "c3")) # Drops un-specified columns
#> $a
#>    c4 c3
#> r1  0  0
#> r2  0  0
#> r3  0  0
#> r4  0  0
#> 
#> $b
#>    c4 c3
#> r1  0  0
#> r2  0  0
#> r3 11  9
#> r4 12 10
#> 
complete_and_sort(m1, m2, margin = 1)
#> $a
#>    c2 c1
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> r4  0  0
#> 
#> $b
#>    c2 c3 c4
#> r1  0  0  0
#> r2  0  0  0
#> r3  7  9 11
#> r4  8 10 12
#> 
complete_and_sort(m1, m2, margin = 2)
#> $a
#>    c1 c2 c3 c4
#> r1  4  1  0  0
#> r2  5  2  0  0
#> r3  6  3  0  0
#> 
#> $b
#>    c1 c2 c3 c4
#> r3  0  7  9 11
#> r4  0  8 10 12
#> 
complete_and_sort(m1, t(m2))
#> $a
#>    c1 c2 r3 r4
#> c2  0  0  0  0
#> c3  0  0  0  0
#> c4  0  0  0  0
#> r1  4  1  0  0
#> r2  5  2  0  0
#> r3  6  3  0  0
#> 
#> $b
#>    c1 c2 r3 r4
#> c2  0  0  7  8
#> c3  0  0  9 10
#> c4  0  0 11 12
#> r1  0  0  0  0
#> r2  0  0  0  0
#> r3  0  0  0  0
#> 
complete_and_sort(m1, t(m2), margin = 1)
#> $a
#>    c2 c1
#> c2  0  0
#> c3  0  0
#> c4  0  0
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> 
#> $b
#>    r3 r4
#> c2  7  8
#> c3  9 10
#> c4 11 12
#> r1  0  0
#> r2  0  0
#> r3  0  0
#> 
complete_and_sort(m1, t(m2), margin = 2)
#> $a
#>    c1 c2 r3 r4
#> r1  4  1  0  0
#> r2  5  2  0  0
#> r3  6  3  0  0
#> 
#> $b
#>    c1 c2 r3 r4
#> c2  0  0  7  8
#> c3  0  0  9 10
#> c4  0  0 11 12
#> 
v <- matrix(1:6, ncol=2, dimnames=list(c("r3", "r1", "r2"), c("c2", "c1")))
complete_and_sort(v, v)
#> $a
#>    c1 c2
#> r1  5  2
#> r2  6  3
#> r3  4  1
#> 
#> $b
#>    c1 c2
#> r1  5  2
#> r2  6  3
#> r3  4  1
#> 
# Also works with lists
complete_and_sort(list(m1,m1), list(m2,m2))
#> $a
#> $a[[1]]
#>    c2 c1
#> r1  1  4
#> r2  2  5
#> r3  3  6
#> r4  0  0
#> 
#> $a[[2]]
#>    c1 c2 c3 c4
#> r1  4  1  0  0
#> r2  5  2  0  0
#> r3  6  3  0  0
#> 
#> 
#> $b
#> $b[[1]]
#>    c2 c3 c4
#> r1  0  0  0
#> r2  0  0  0
#> r3  7  9 11
#> r4  8 10 12
#> 
#> $b[[2]]
#>    c1 c2 c3 c4
#> r3  0  7  9 11
#> r4  0  8 10 12
#> 
#> 
```
