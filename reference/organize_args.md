# Organize binary arguments

Organizes arguments of binary (2 arguments) `_byname` functions. Actions
performed are:

- if only one argument is a list, make the other argument also a list of
  equal length.

- if both arguments are lists, ensure that they are same length.

- if one argument is a matrix and the other is a constant, make the
  constant into a matrix.

- ensures that row and column types match for `typematch_margins`.

- ensures that list item names match if both `a` and `b` are lists; no
  complaints are made if neither `a` nor `b` has names.

- completes and sorts the matrices.

## Usage

``` r
organize_args(a, b, match_type = "all", fill)
```

## Arguments

- a:

  the first argument to be organized

- b:

  the second argument to be organized

- match_type:

  one of `"all"`, `"matmult"`, `"none"`. When both `a` and `b` are
  matrices, "`all`" (the default) indicates that rowtypes of `a` must
  match rowtypes of `b` and coltypes of `a` must match coltypes of `b`.
  If "`matmult`", coltypes of `a` must match rowtypes of `b`.

- fill:

  a replacement value for `a` or `b` if either is missing or `NULL`.

## Value

a list with two elements (named `a` and `b`) containing organized
versions of the arguments
