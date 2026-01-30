# Apply a binary function "by name"

If either `a` or `b` is missing or `NULL`, `0` is passed to `FUN` in its
place. Note that if either `a` and `b` are lists, elements must be named
the same. The names of list elements of `a` are applied to the output.

## Usage

``` r
binaryapply_byname(
  FUN,
  a,
  b,
  .FUNdots = NULL,
  match_type = c("all", "matmult", "none"),
  set_rowcoltypes = TRUE,
  .organize = TRUE
)
```

## Arguments

- FUN:

  a binary function to be applied "by name" to `a` and `b`.

- a:

  the first operand for `FUN`.

- b:

  the second operand for `FUN`.

- .FUNdots:

  a list of additional named arguments passed to `FUN.`

- match_type:

  one of "all", "matmult", or "none". When both `a` and `b` are
  matrices, "all" (the default) indicates that rowtypes of `a` must
  match rowtypes of `b` and coltypes of `a` must match coltypes of `b`.
  If "matmult", coltypes of `a` must match rowtypes of `b`. If "none",
  neither coltypes nor rowtypes are checked.

- set_rowcoltypes:

  tells whether to apply row and column types from `a` and `b` to the
  output. Set `TRUE` (the default) to apply row and column types to the
  output. Set `FALSE`, to *not* apply row and column types to the
  output.

- .organize:

  a boolean that tells whether or not to automatically complete `a` and
  `b` relative to each other and sort the rows and columns of the
  completed matrices. Normally, this should be `TRUE` (the default).
  However, if `FUN` takes over this responsibility, set to `FALSE`.

## Value

the result of applying `FUN` "by name" to `a` and `b`.

## Examples

``` r
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
  setrowtype("Products") %>% setcoltype("Industries")
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
sum_byname(U, Y)
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
binaryapply_byname(`+`, U, Y)
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```
