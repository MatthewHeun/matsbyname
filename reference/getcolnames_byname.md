# Gets column names

Gets column names in a way that is amenable to use in chaining
operations in a functional programming way

## Usage

``` r
getcolnames_byname(a)
```

## Arguments

- a:

  The matrix or data frame from which column names are to be retrieved

## Value

Column names of `m`.

## Examples

``` r
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
getcolnames_byname(m)
#> [1] "c1" "c2" "c3"
# This also works for lists
getcolnames_byname(list(m,m))
#> [[1]]
#> [1] "c1" "c2" "c3"
#> 
#> [[2]]
#> [1] "c1" "c2" "c3"
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
getcolnames_byname(DF$m)
#> [[1]]
#> [1] "c1" "c2" "c3"
#> 
#> [[2]]
#> [1] "c1" "c2" "c3"
#> 
```
