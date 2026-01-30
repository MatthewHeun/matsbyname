# Prepare the `.FUNdots` argument for `*apply_byname` functions.

This is a helper function for the various `*apply_byname` functions.

## Usage

``` r
prepare_.FUNdots(a, .FUNdots)
```

## Arguments

- a:

  the main argument to an `*apply_byname` function.

- .FUNdots:

  a list of additional arguments to be applied to `FUN` in one of the
  `*apply_byname` functions.

## Value

a reconfigured version of `.FUNdots`, ready for use by an
`*apply_byname` function.

- both a and the item of .FUNdots are lists

  - if the item of .FUNdots (a list itself) has length different from 1
    or length(a), throw an error

  - if the item of .FUNdots (a list itself) has length 1, replicate the
    single item to be a list of length = length(a)

  - if the item of .FUNdots (a list itself) has length = length(a), use
    the item of .FUNdots as is

- a is NOT a list, but the item of .FUNdots IS a list

  - pass the argument along and hope for the best. This situation is
    probably an error. If so, it will become apparent soon.

- a is a list but the item (argument) of .FUNdots is NOT a list This
  situation could be ambiguous. Let's say the list of `a` values has
  length 2, and an argument `margin = c(1, 2)`. Should `margin = 1` be
  applied to `a[[1]]` and `margin = 2` be applied to `a[[2]]`? Or should
  `margin = c(1, 2)` be applied to both `a[[1]]` and `a[[2]]`? This
  ambiguity should be handled by using the function
  [`prep_vector_arg()`](https://matthewheun.github.io/matsbyname/reference/prep_vector_arg.md)
  within the function that calls
  [`unaryapply_byname()`](https://matthewheun.github.io/matsbyname/reference/unaryapply_byname.md).
  For an example, see
  [`identize_byname()`](https://matthewheun.github.io/matsbyname/reference/identize_byname.md).
  When the arguments are coming in from a data frame, there will be no
  ambiguity, but the information will not be coming `.FUNdots[[i]]` as a
  list. Optimizing for the data frame case, this function allows vectors
  of length equal to the length of the list `a`, interpreting such
  vectors as applying in sequence to each `a` in turn. So the algorithm
  is as follows:

  - if a non-NULL item of .FUNdots (which is not a list) has length
    other than 1 or length(a), throw an error.

  - if a non-NULL item of .FUNdots (which is not a list) has length = 1,
    replicate that single item to be a list of length = length(a).

  - if a non-NULL item of .FUNdots (which is not a list) has length =
    length(a), leave it as-is.

- neither a nor the item of .FUNdots is a list

  - a should have length = 1, but a single matrix reports its length as
    the number of elements of the matrix. So, we can't check length in
    this situation.

  - the item of .FUNdots is assumed to have length 1 and passed along

## Details

We have four cases between a and any single item of .FUNdots:

- both a and the item of .FUNdots are lists

  - if the item of .FUNdots (a list itself) has length different from 1
    or length(a), throw an error

  - if the item of .FUNdots (a list itself) has length 1, replicate the
    single item to be a list of length = length(a)

  - if the item of .FUNdots (a list itself) has length = length(a), use
    the item of .FUNdots as is

- a is a list but the item (argument) of .FUNdots is NOT a list

  - if the item of .FUNdots (which is not a list) has length != 1, throw
    an error, because there is ambiguity how the item of .FUNdots should
    be treated.

  - if the item of .FUNdots (which is not a list) has length = 1,
    replicate that single item to be a list of length = length(a)

- a is NOT a list, but the item of .FUNdots IS a list

  - pass the argument along and hope for the best. This situation is
    probably an error. If so, it will become apparent soon.

- neither a nor the item of .FUNdots is a list

  - a should have length = 1, but a single matrix reports its length as
    the number of elements of the matrix. So, we can't check length in
    this situation.

  - the item of .FUNdots is assumed to have length 1 and passed along
