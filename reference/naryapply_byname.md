# Apply a function "by name" to any number of operands

Applies `FUN` to all operands in `...`. Other arguments have similar
meaning as
[`binaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/binaryapply_byname.md).
See details for more information.

## Usage

``` r
naryapply_byname(
  FUN,
  ...,
  .FUNdots = NULL,
  match_type = c("all", "matmult", "none"),
  set_rowcoltypes = TRUE,
  .organize = TRUE,
  .summarise = FALSE
)
```

## Arguments

- FUN:

  a binary function to be applied "by name" to all operands in `...`.

- ...:

  the operands for `FUN`.

- .FUNdots:

  a list of additional named arguments passed to `FUN`.

- match_type:

  One of "all", "matmult", or "none". When `...` are matrices, "all"
  (the default) indicates that rowtypes of all `...` matrices must match
  and coltypes of all `...` matrices must match. If "matmult", the
  coltype of the first operand must match the rowtype of the second
  operand for every sequential invocation of `FUN`. If "none", neither
  coltypes nor rowtypes are checked by `naryapply_byname()`.

- set_rowcoltypes:

  Tells whether to apply row and column types from operands in `...` to
  the output of each sequential invocation of `FUN`. Set `TRUE` (the
  default) to apply row and column types. Set `FALSE`, to *not* apply
  row and column types to the output.

- .organize:

  A boolean that tells whether or not to automatically complete operands
  in `...` relative to each other and sort the rows and columns of the
  completed matrices. This organizing is done on each sequential
  invocation of `FUN`. Normally, this should be `TRUE` (the default).
  However, if `FUN` takes over this responsibility, set to `FALSE`.

- .summarise:

  A boolean that tells whether this call is considered a summarise
  operation (like
  [`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)).
  Default is `FALSE`.

## Value

the result of applying `FUN` to all operands in `...`

## Details

If only one `...` argument is supplied, `FUN` must be capable of
handling one argument, and the call is routed to
[`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md).
When `set_rowcoltypes` is `TRUE`, the `rowcoltypes` argument of
[`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md)
is set to "all", but when `set_rowcoltypes` is `FALSE`, the
`rowcoltypes` argument of
[`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md)
is set to "none". If finer control is desired, the caller should use
[`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md)
directly. If more than one argument is passed in `...`, `FUN` must be a
binary function, but its use in by `naryapply_byname()` is "n-ary."
Arguments `match_type`, `set_rowcoltypes`, and `.organize` have same
meaning as for
[`binaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/binaryapply_byname.md).
Thus, all of the operands in `...` must obey the rules of type matching
when `match_type` is `TRUE`.

`naryapply_byname()` and
[`cumapply_byname()`](https://matthewheun.github.io/matsbyname/reference/cumapply_byname.md)
are similar. Their differences can be described by considering a data
frame. `naryapply_byname()` applies `FUN` to several columns (variables)
of the data frame. For example,
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
applied to several variables gives another column containing the sums
across each row of the data frame.
[`cumapply_byname()`](https://matthewheun.github.io/matsbyname/reference/cumapply_byname.md)
applies `FUN` to successive entries in a single column. For example
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
applied to a single column gives the sum of all numbers in that column.

## Examples

``` r
naryapply_byname(FUN = sum_byname, 2, 3)
#> [1] 5
naryapply_byname(FUN = sum_byname, 2, 3, 4, -4, -3, -2)
#> [1] 0
# Routes to unaryapply_byname
naryapply_byname(FUN = `^`, list(1,2,3), .FUNdots = list(2))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 4
#> 
#> [[3]]
#> [1] 9
#> 
```
