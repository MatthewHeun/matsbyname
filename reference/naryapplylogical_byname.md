# Apply a function logically to numbers, matrices, or lists of numbers or matrices

Operands should be logical, although numerical operands are accepted.
Numerical operands are interpreted as `0` is `FALSE`, and any other
number is `TRUE`.

## Usage

``` r
naryapplylogical_byname(
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

  a binary function (that returns logical values) to be applied over
  operands

- ...:

  operands; constants, matrices, or lists of matrices

- .FUNdots:

  a list of additional named arguments passed to `FUN`.

- match_type:

  One of "all", "matmult", or "none". When `...` are matrices, "all"
  (the default) indicates that rowtypes of all `...` matrices must match
  and coltypes of all `...` matrices must match. If "matmult", the
  coltype of the first operand must match the rowtype of the second
  operand for every sequential invocation of `FUN`. If "none", neither
  coltypes nor rowtypes are checked by
  [`naryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/naryapply_byname.md).

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

the result of `FUN` applied logically to `...`

## Details

This function is not exported, thereby retaining the right to future
changes.

## Examples

``` r
matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, TRUE)
#> [1] TRUE
matsbyname:::naryapplylogical_byname(`&`, TRUE, TRUE, FALSE)
#> [1] FALSE
```
