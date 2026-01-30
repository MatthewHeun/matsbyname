# Gets row names

Gets row names in a way that is amenable to use in chaining operations
in a functional programming way

## Usage

``` r
getrownames_byname(a)
```

## Arguments

- a:

  The matrix or data frame on which row names are to be retrieved

## Value

row names of `a`

## Examples

``` r
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
  setrowtype("Industries") %>% setcoltype("Commodities")
getrownames_byname(m)
#> [1] "i1" "i2"
# This also works for lists
getrownames_byname(list(m,m))
#> [[1]]
#> [1] "i1" "i2"
#> 
#> [[2]]
#> [1] "i1" "i2"
#> 
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
getrownames_byname(DF$m)
#> [[1]]
#> [1] "i1" "i2"
#> 
#> [[2]]
#> [1] "i1" "i2"
#> 
```
