# Apply a unary function by name

`FUN` is applied to `a` using additional arguments `.FUNdots` to `FUN`.
If `a` is a list, the names of `a` are applied to the output.

## Usage

``` r
unaryapply_byname(
  FUN,
  a,
  .FUNdots = NULL,
  rowcoltypes = c("all", "transpose", "row", "col", "none")
)
```

## Arguments

- FUN:

  a unary function to be applied "by name" to `a`.

- a:

  the argument to `FUN`.

- .FUNdots:

  a list of additional named arguments passed to `FUN`.

- rowcoltypes:

  a string that tells how to transfer row and column types of `a` to
  output. See details.

## Value

the result of applying `FUN` "by name" to `a`.

## Details

Note that `.FUNdots` can be a rectangular two-dimensional list of
arguments to `FUN`. If so, `.FUNdots` is interpreted as follows:

- The first dimension of `.FUNdots` contains named arguments to `FUN`.

- The second dimension of `.FUNdots` contains unique values of the named
  arguments to be applied along the list that is `a`.

The length of the first dimension of `.FUNdots` is the number of
arguments supplied to `FUN`. The length of the second dimension of
`.FUNdots` must be equal to the length of `a`.

See
[`prepare_.FUNdots()`](https://matthewheun.github.io/matsbyname/reference/prepare_.FUNdots.md)
for more details on the `.FUNdots` argument.

Options for the `rowcoltypes` argument are:

- "all": transfer both row and column types of `a` directly to output.

- "transpose": rowtype of `a` becomes coltype of output; coltype of `a`
  becomes rowtype of output. "transpose" is helpful for `FUN`s that
  transpose `a` upon output.

- "row": rowtype of `a` becomes both rowtype and coltype of output.

- "col": coltype of `a` becomes both rowtype and coltype of output.

- "none": rowtype and coltype not set by `unaryapply_byname`. Rather,
  `FUN` will set rowtype and coltype.

Note that `rowcoltypes` should not be a vector or list of strings.
Rather, it should be a single string.

## Examples

``` r
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
  setrowtype("Products") %>% setcoltype("Industries")
difference_byname(0, U)
#>    i1 i2
#> p1 -1 -3
#> p2 -2 -4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
unaryapply_byname(`-`, U)
#>    i1 i2
#> p1 -1 -3
#> p2 -2 -4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```
