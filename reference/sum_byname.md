# Name-wise addition of matrices

Performs a union and sorting of addend and augend row and column names
prior to summation. Zeroes are inserted for missing matrix elements.
Treats missing or `NULL` operands as `0`.

## Usage

``` r
sum_byname(..., .summarise = FALSE)
```

## Arguments

- ...:

  Operands: constants, matrices, or lists of matrices.

- .summarise:

  When `TRUE`, a operands are summed down lists. When `FALSE` (the
  default), items are summed across lists.

## Value

A matrix representing the name-wise sum of arguments.

## Details

For this function, a list of lists of operands is ambiguous. Should the
operands be summed across lists (first items summed across all lists,
second items summed across all list, etc.) or should each list be summed
along each list? In the first case, the return object will have length
equal to the length of the lists in the `...` argument. In the second
case, the return object will have length equal to the number of lists in
the `...` argument. The first case is like summing across rows of a data
frame. The second case is like summing down columns of a data frame. The
`summarise` argument distinguishes between these two cases. The default
value for `summarise` is `FALSE`, giving the first behavior. Set
`summarise` to `TRUE` to cause this function to act like
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
for its list of arguments. If `.summarise = TRUE`, the data value is
guaranteed to be a list. If the call to `sum_byname(.summarise = TRUE)`
is made in the context of a data frame, the column returned is
guaranteed to be a list column. See the aggregation vignette for
additional details and examples.

## Examples

``` r
library(dplyr)
sum_byname(2, 2)
#> [1] 4
sum_byname(2, 2, 2)
#> [1] 6
sum_byname(2, 2, -2, -2)
#> [1] 0
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
  setrowtype("Products") %>% setcoltype("Industries")
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
sum_byname(U, 100)
#>     i1  i2
#> p1 101 103
#> p2 102 104
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
sum_byname(200, Y)
#>     i1  i2
#> p1 204 202
#> p2 203 201
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
U + Y # Non-sensical.  Row and column names not respected.
#>    i1 i2
#> p1  2  6
#> p2  4  8
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
sum_byname(U, U)
#>    i1 i2
#> p1  2  6
#> p2  4  8
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
sum_byname(U, Y)
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
sum_byname(U, U, Y, Y)
#>    i1 i2
#> p1 10 10
#> p2 10 10
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, productnames)) %>%
  setrowtype("Industries") %>% setcoltype("Products")
U + V # row and column names are non-sensical and blindly taken from first argument (U)
#>    i1 i2
#> p1  2  6
#> p2  4  8
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
if (FALSE) sum_byname(U, V) # \dontrun{} # Fails, because row and column types are different
# This also works with lists
sum_byname(list(U,U), list(Y,Y))
#> [[1]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
sum_byname(list(U,U), list(100,100))
#> [[1]]
#>     i1  i2
#> p1 101 103
#> p2 102 104
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>     i1  i2
#> p1 101 103
#> p2 102 104
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
sum_byname(list(U,U), as.list(rep_len(100, 2)))
#> [[1]]
#>     i1  i2
#> p1 101 103
#> p2 102 104
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>     i1  i2
#> p1 101 103
#> p2 102 104
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF <- data.frame(U = I(list()), Y = I(list()))
DF[[1,"U"]] <- U
DF[[2,"U"]] <- U
DF[[1,"Y"]] <- Y
DF[[2,"Y"]] <- Y
sum_byname(DF$U, DF$Y)
#> [[1]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF %>% mutate(sums = sum_byname(U, Y))
#>            U          Y       sums
#> 1 1, 2, 3, 4 1, 2, 3, 4 5, 5, 5, 5
#> 2 1, 2, 3, 4 1, 2, 3, 4 5, 5, 5, 5
sum_byname(U) # If only one argument, return it.
#>    i1 i2
#> p1  1  3
#> p2  2  4
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
sum_byname(2, NULL) # Gives 2
#> [1] 2
sum_byname(2, NA)   # Gives NA
#> [1] NA
sum_byname(NULL, 1) # Gives 1
#> [1] 1
sum_byname(list(NULL, 1), list(1, 1))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
DF2 <- data.frame(U = I(list()), Y = I(list()))
DF2[[1,"U"]] <- NULL
DF2[[2,"U"]] <- U
DF2[[1,"Y"]] <- Y
DF2[[2,"Y"]] <- Y
sum_byname(DF2$U, DF2$Y)
#> [[1]]
#>    i1 i2
#> p1  4  2
#> p2  3  1
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
#> [[2]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
#> 
DF3 <- DF2 %>% mutate(sums = sum_byname(U, Y))
DF3
#>            U          Y       sums
#> 1            1, 2, 3, 4 4, 3, 2, 1
#> 2 1, 2, 3, 4 1, 2, 3, 4 5, 5, 5, 5
DF3$sums[[1]]
#>    i1 i2
#> p1  4  2
#> p2  3  1
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
DF3$sums[[2]]
#>    i1 i2
#> p1  5  5
#> p2  5  5
#> attr(,"rowtype")
#> [1] "Products"
#> attr(,"coltype")
#> [1] "Industries"
```
