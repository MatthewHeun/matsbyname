# Use Cases and Examples for \`matsbyname\`

## Introduction

Matrices are important mathematical objects, and they often describe
networks of flows among nodes. Example networks are given in the
following table.

| System type   | Flows           | Nodes                   |
|:--------------|:----------------|:------------------------|
| Ecological    | nutrients       | organisms               |
| Manufacturing | materials       | factories               |
| Economic      | money           | economic sectors        |
| Energy        | energy carriers | energy conversion steps |

The power of matrices lies in their ability to organize network-wide
calculations, thereby simplifying the work of analysts who study entire
systems. However, three problems arise when performing matrix operations
in `R` and other languages.

### Problem 1

Although built-in matrix functions ensure size conformity of matrix
operands, they do not respect the names of rows and columns (known as
`dimnames` in `R`). In the following example, **U** represents a *use*
matrix that contains the quantity of each product used by each industry,
and **Y** represents a *final demand* matrix that contains the quantity
of each product consumed by final demand industries. If the rows and
columns are not in the same order, the sum of the matrices is
nonsensical.

``` r
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames))
U
#>    i1 i2
#> p1  1  3
#> p2  2  4
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames)))
Y
#>    i2 i1
#> p2  1  3
#> p1  2  4
# This sum is nonsensical.  Neither row nor column names are respected.
U + Y 
#>    i1 i2
#> p1  2  6
#> p2  4  8
```

As a result, analysts performing matrix operations must maintain strict
order of rows and columns across all calculations.

``` r
# Make a new version of Y (Y2), this time with dimnames in same order as U
Y2 <- matrix(4:1, ncol = 2, dimnames = list(productnames, industrynames))
Y2
#>    i1 i2
#> p1  4  2
#> p2  3  1
# Now the sum is sensible. Both row and column names are respected.
U + Y2
#>    i1 i2
#> p1  5  5
#> p2  5  5
```

### Problem 2

In many cases, operand matrices may have different numbers or different
names of rows or columns. This situation can occur when, for example,
products or industries changes across time periods. When performing
matrix operations, rows or columns of zeros must be added to ensure name
conformity.

``` r
Y3 <- matrix(5:8, ncol = 2, dimnames = list(c("p1", "p3"), c("i1", "i3")))
Y3
#>    i1 i3
#> p1  5  7
#> p3  6  8
# Nonsensical because neither row names nor column names are respected. 
# The "p3" rows and "i3" columns of Y3 have been added to 
# "p2" rows and "i2" columns of U.
# Row and column names for the sum are taken from the first operand (U).
U + Y3
#>    i1 i2
#> p1  6 10
#> p2  8 12
# Rather, need to insert missing rows in both U and Y before summing.
U_2000 <- matrix(c(1, 3, 0,
                   2, 4, 0,
                   0, 0, 0),
                 ncol = 3, byrow = TRUE, 
                 dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3")))
Y_2000 <- matrix(c(5, 0, 7,
                   0, 0, 0, 
                   6, 0, 8),
                 ncol = 3, byrow = TRUE,
                 dimnames = list(c("p1", "p2", "p3"), c("i1", "i2", "i3")))
U_2000
#>    i1 i2 i3
#> p1  1  3  0
#> p2  2  4  0
#> p3  0  0  0
Y_2000
#>    i1 i2 i3
#> p1  5  0  7
#> p2  0  0  0
#> p3  6  0  8
U_2000 + Y_2000
#>    i1 i2 i3
#> p1  6  3  7
#> p2  2  4  0
#> p3  6  0  8
```

The analyst’s burden is cumbersome. But worse problems await.

Respecting names (and adding rows and columns of zeroes) can lead to an
inability to invert matrices downstream, as shown in the following
example.

``` r
# The original U matrix is invertible.
solve(U)
#>    p1   p2
#> i1 -2  1.5
#> i2  1 -0.5
# The version of U that contains zero rows and columns (U_2000)
# is singular and cannot be inverted.
tryCatch(solve(U_2000), error = function(err){print(err)})
#> <simpleError in solve.default(U_2000): Lapack routine dgesv: system is exactly singular: U[3,3] = 0>
```

### Problem 3

Matrix functions provided by `R` and other languages do not ensure type
conformity for matrix operands to matrix algebra functions. In the
example of matrix multiplication, columns of the multiplicand must
contain the same type of information as the as the rows of the
multiplier. If the columns of **A** are countries, then the rows of
**B** must also be countries (and in the same order) if **A** `%*%`
**B** is to make sense.

## `matsbyname`

The `matsbyname` package automatically addresses all three problems
above. It performs smart matrix operations that

- respect row and column names
  - by inserting rows and columns of zeroes as necessary and
  - by re-ordering rows and columns to ensure conformity of the names of
    operand rows and columns, and
- respect row and column types, enforcing conformity as appropriate.

These features are available without analyst intervention, as shown in
the following example.

``` r
# Same as U + Y2, without needing to create Y2.
sum_byname(U, Y)
#>    i1 i2
#> p1  5  5
#> p2  5  5
# Same as U_2000 + Y_2000, but U and Y3 are unmodified.
sum_byname(U, Y3)
#>    i1 i2 i3
#> p1  6  3  7
#> p2  2  4  0
#> p3  6  0  8
# Eliminate zero-filled rows and columns. Same result as solve(U).
U_2000 %>% clean_byname(margin = c(1,2), clean_value = 0) %>% solve()
#>    p1   p2
#> i1 -2  1.5
#> i2  1 -0.5
```

In addition to
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
and
[`clean_byname()`](https://matthewheun.github.io/matsbyname/reference/clean_byname.md),
the `matsbyname` package contains many additional matrix algebra
functions that respect the names of rows and columns. Commonly-used
functions are:

- [`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
- [`difference_byname()`](https://matthewheun.github.io/matsbyname/reference/difference_byname.md)
- [`hadamardproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/hadamardproduct_byname.md)
- [`matrixproduct_byname()`](https://matthewheun.github.io/matsbyname/reference/matrixproduct_byname.md)
- [`quotient_byname()`](https://matthewheun.github.io/matsbyname/reference/quotient_byname.md)
- [`rowsums_byname()`](https://matthewheun.github.io/matsbyname/reference/rowsums_byname.md)
- [`colsums_byname()`](https://matthewheun.github.io/matsbyname/reference/colsums_byname.md)
- [`invert_byname()`](https://matthewheun.github.io/matsbyname/reference/invert_byname.md),
  and
- [`transpose_byname()`](https://matthewheun.github.io/matsbyname/reference/transpose_byname.md).

The full list of functions can be found with
[`?matsbyname`](https://matthewheun.github.io/matsbyname/reference/matsbyname-package.md)
and clicking the `Index` link.

Furthermore, `matsbyname` works well with its sister package,
`matsindf`. `matsindf` creates data frames whose entries are not numbers
but entire matrices, thereby enabling the use of `matsbyname` functions
in [`tidyverse`](https://tidyverse.org) functional programming.

When used together, `matsbyname` and `matsindf` allow analysts to wield
simultaneously the power of both [matrix
mathematics](https://en.wikipedia.org/wiki/Matrix_(mathematics)) and
[`tidyverse`](https://tidyverse.org) functional programming.

This vignette demonstrates the power of matrix mathematics performed
`byname`.

## `matsbyname` features

The `matsbyname` package has several features that both simplify
analyses and ensure their correctness.

### Setting row and column names

In the preceding examples, row and column names were provided by the
`dimnames` argument to the
[`matrix()`](https://rdrr.io/r/base/matrix.html) function. However,
`matsbyname` provides the
[`setcolnames_byname()`](https://matthewheun.github.io/matsbyname/reference/setcolnames_byname.md)
and
[`setrownames_byname()`](https://matthewheun.github.io/matsbyname/reference/setrownames_byname.md)
functions to perform the same tasks using the pipe operator (`%>%` or
`|>`).

``` r
U_2 <- matrix(1:4, ncol = 2) %>% 
  setrownames_byname(productnames) %>% setcolnames_byname(industrynames)
U_2
#>    i1 i2
#> p1  1  3
#> p2  2  4
```

### Setting row and column types

Row and column types can be understood by analogy: row and column types
are to matrices in matrix algebra as units are to scalars in scalar
algebra. Just as careful tracking of units is necessary in scalar
calculations, careful tracking of row and column types is necessary in
matrix operations. Because `matsbyname` keeps track of row and column
types automatically, much of the burden of dealing with row and column
types is removed from the analyst.

Row and column types are character strings stored as attributes of the
matrix object, and `matsbyname` functions ensure correctness of matrix
operations by checking row and column types, throwing errors if needed.
Row and column types can be set by the functions
[`setrowtype()`](https://matthewheun.github.io/matsbyname/reference/setrowtype.md)
and
[`setcoltype()`](https://matthewheun.github.io/matsbyname/reference/setcoltype.md)
and retrieved by the functions
[`rowtype()`](https://matthewheun.github.io/matsbyname/reference/rowtype.md)
and
[`coltype()`](https://matthewheun.github.io/matsbyname/reference/coltype.md).
Consider matrices **A**, **B**, and **C**:

``` r
A <- matrix(1:4, ncol = 2) %>% 
  setrownames_byname(productnames) %>% setcolnames_byname(industrynames) %>% 
  setrowtype("Products") %>% setcoltype("Industries")
A
#>    i1 i2
#> p1  1  3
#> p2  2  4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
B <- matrix(8:5, ncol = 2) %>% 
  setrownames_byname(productnames) %>% setcolnames_byname(industrynames) %>% 
  setrowtype("Products") %>% setcoltype("Industries")
B
#>    i1 i2
#> p1  8  6
#> p2  7  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
C <- matrix(1:4, ncol = 2) %>% 
  setrownames_byname(industrynames) %>% setcolnames_byname(productnames) %>% 
  setrowtype("Industries") %>% setcoltype("Products")
C
#>    p1 p2
#> i1  1  3
#> i2  2  4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
```

**B** can be added to **A**, because row and column types are identical.

``` r
sum_byname(A, B)
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```

However, **C** cannot be added to **A** (or **B**), because row and
column types disagree.

``` r
tryCatch(sum_byname(A, C), error = function(err){print(err)})
#> <simpleError in organize_args(a, b, fill = 0, match_type = match_type): rowtype(a) (Products) != rowtype(b) (Industries).>
```

In this case, a sum is possible if **C** is transposed prior to adding
to **A**, because row and column types of **A** and **C**^(T) agree.

``` r
sum_byname(A, transpose_byname(C))
#>    i1 i2
#> p1  2  5
#> p2  5  8
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```

Matrices **A** and **B** can be element-multiplied and element-divided
for the same reason they can be summed: row and column types agree.

``` r
hadamardproduct_byname(A, B)
#>    i1 i2
#> p1  8 18
#> p2 14 20
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
quotient_byname(A, B)
#>           i1  i2
#> p1 0.1250000 0.5
#> p2 0.2857143 0.8
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```

Note that **A** and **C** can be matrix-multiplied, because the column
type of **A** and the row type of **C** are identical (`Industries`).
The result is a `Products`-by-`Products` matrix.

``` r
matrixproduct_byname(A, C)
#>    p1 p2
#> p1  7 15
#> p2 10 22
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Products"
```

However, **A** and **B** cannot be matrix-multiplied, because the column
type of **A** (`Industries`) and the row type of **B** (`Products`) are
different.

``` r
tryCatch(matrixproduct_byname(A, B), error = function(err){print(err)})
#> <simpleError in organize_args(a, b, fill = 0, match_type = match_type): coltype(a) != rowtype(b): Industries != Products.>
```

Analysts are encouraged to set row and column types on matrices, thereby
taking advantage of `matsbyname`’s type-tracking feature to improve
their matrix-based analyses.

### `*_byname` functions work well with lists

Another feature of the `matsbyname` package is that it works when
arguments to functions are lists of matrices, returning lists as
appropriate.

``` r
sum_byname(A, list(B, B))
#> [[1]]
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
hadamardproduct_byname(list(A, A), B)
#> [[1]]
#>    i1 i2
#> p1  8 18
#> p2 14 20
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> p1  8 18
#> p2 14 20
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
matrixproduct_byname(list(A, A), list(C, C))
#> [[1]]
#>    p1 p2
#> p1  7 15
#> p2 10 22
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Products"
#> 
#> [[2]]
#>    p1 p2
#> p1  7 15
#> p2 10 22
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Products"
```

### `matsbyname` works well with `matsindf`

The `matsindf` package provides functions that collapse
[tidy](https://tidyr.tidyverse.org/articles/tidy-data.html) data frames
of matrix elements into data frames of matrices. Data frames of
matrices, such as those created by `matsindf`, are like magic
spreadsheets in which single cells contain entire matrices.

The following example demonstrates an approach to creating a data frame
of matrices.

``` r
tidy <- data.frame(
  matrix = c("A", "A", "A", "A", "B", "B", "B", "B"),
  row = c("p1", "p1", "p2", "p2", "p1", "p1", "p2", "p2"),
  col = c("i1", "i2", "i1", "i2", "i1", "i2", "i1", "i2"),
  vals = c(1, 3, 2, 4, 8, 6, 7, 5)
) %>%
  mutate(
    rowtype = "Industries",
    coltype  = "Products"
  )
tidy
#>   matrix row col vals    rowtype  coltype
#> 1      A  p1  i1    1 Industries Products
#> 2      A  p1  i2    3 Industries Products
#> 3      A  p2  i1    2 Industries Products
#> 4      A  p2  i2    4 Industries Products
#> 5      B  p1  i1    8 Industries Products
#> 6      B  p1  i2    6 Industries Products
#> 7      B  p2  i1    7 Industries Products
#> 8      B  p2  i2    5 Industries Products
mats <- tidy %>%
  group_by(matrix) %>%
  matsindf::collapse_to_matrices(matnames = "matrix", matvals = "vals",
                                 rownames = "row", colnames = "col",
                                 rowtypes = "rowtype", coltypes = "coltype") %>%
  rename(
    matrix.name = matrix,
    matrix = vals
  )
mats
#>   matrix.name     matrix
#> 1           A 1, 2, 3, 4
#> 2           B 8, 7, 6, 5
mats$matrix[[1]]
#>    i1 i2
#> p1  1  3
#> p2  2  4
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
mats$matrix[[2]]
#>    i1 i2
#> p1  8  6
#> p2  7  5
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
```

## Using `matsbyname` with `matsindf`

Because

- `matsbyname` works well with lists, and
- data frame columns are implemented as lists, and
- the `matsindf` package can create data frames of matrices, and
- the [tidyr](https://tidyr.tidyverse.org) and
  [dplyr](https://dplyr.tidyverse.org) packages work with data frames,

`matsbyname` functions can be used with
[tidyr](https://tidyr.tidyverse.org) and
[dplyr](https://dplyr.tidyverse.org) functions (such as `spread` and
`mutate`) to perform matrix algebra within data frames of matrices. A
single `matsbyname` instruction performs the same operation on all rows
of a `matsindf` data frame. Loops begone!

``` r
result <- mats %>%
  tidyr::spread(key = matrix.name, value = matrix) %>%
  # Duplicate the row to demonstrate byname operating simultaneously
  # on all rows of the data frame.
  dplyr::bind_rows(., .) %>%
  dplyr::mutate(
    # Create a column of constants.
    c = RCLabels::make_list(x = 1:2, n = 2, lenx = 2),
    # Sum all rows of the data frame with a single instruction.
    sum = sum_byname(A, B),
    # Multiply matrices in the sum column by corresponding constants in the c column.
    product = hadamardproduct_byname(c, sum)
  )
result
#>            A          B c        sum        product
#> 1 1, 2, 3, 4 8, 7, 6, 5 1 9, 9, 9, 9     9, 9, 9, 9
#> 2 1, 2, 3, 4 8, 7, 6, 5 2 9, 9, 9, 9 18, 18, 18, 18
result$sum[[1]]
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
result$sum[[2]]
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
result$product[[1]]
#>    i1 i2
#> p1  9  9
#> p2  9  9
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
result$product[[2]]
#>    i1 i2
#> p1 18 18
#> p2 18 18
#> attr(,"rowtype")
#> [1] "Industries"
#> attr(,"coltype")
#> [1] "Products"
```

## Suggested workflow using `matsbyname` and `matsindf`

A suggested analysis workflow is as follows:

- Munge data into tidy data frame with columns for matrix name, element
  value, row name, column name, row type, and column type, similar to
  `tidy` above.
- Use
  [`matsindf::collapse_to_matrices()`](https://matthewheun.github.io/matsindf/reference/collapse_to_matrices.html)
  to create a data frame of matrices with columns for matrix names and
  matrices themselves, similar to `mats` above.
- [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  the matrices to obtain a data frame with columns for each matrix,
  similar to `result` above.
- Perform matrix algebra operations on the columns of matrices using
  `*_byname` functions.
- [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  the columns into a data frame with a single column of matrices.
- Use
  [`matsindf::expand_to_tidy()`](https://matthewheun.github.io/matsindf/reference/expand_to_tidy.html)
  to create a tidy data frame of matrix elements.
- [`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
  the data as necessary.
- Graph or perform other manipulations of the data.

For more information and a detailed example of this workflow, see the
vignette for the `matsindf` package.

## Summary

The `matsbyname` package simplifies analyses in which row and column
names ought to be respected. It provides optional row and column types,
thereby ensuring that only valid matrix operations are performed.
Finally, `matsbyname` functions work equally well with lists to allow
use of `*_byname` functions with [tidyr](https://tidyr.tidyverse.org)
and [dplyr](https://dplyr.tidyverse.org) approaches to manipulating
data.
