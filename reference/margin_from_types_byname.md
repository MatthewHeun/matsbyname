# Translate row and column types to integer margins

Converts row and column types to integer margins, based on `a` and
`types`. If `types` is not a character vector, `types` is returned
unmodified. If `types` is a character vector, an integer vector is
returned corresponding to the margins on which `types` are found. If
`types` are not found in the row or column types of `a`, `NA_integer_`
is returned.

## Usage

``` r
margin_from_types_byname(a, types)
```

## Arguments

- a:

  A matrix or list of matrices.

- types:

  A character vector or list of character vectors representing row or
  column types whose corresponding integer margins in `a` are to be
  determined.

## Value

A vector of integers or list of vectors of integers corresponding to the
margins on which `types` exist.

## Examples

``` r
# Works for single matrices
m <- matrix(1) %>%
  setrowtype("Product") %>% setcoltype("Industry")
margin_from_types_byname(m, "Product")
#> [1] 1
margin_from_types_byname(m, "Industry")
#> [1] 2
margin_from_types_byname(m, c("Product", "Industry"))
#> [1] 1 2
margin_from_types_byname(m, c("Industry", "Product"))
#> [1] 1 2
# Works for lists of matrices
margin_from_types_byname(list(m, m), types = "Product")
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1
#> 
margin_from_types_byname(list(m, m), types = "Industry")
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 2
#> 
margin_from_types_byname(list(m, m), types = c("Product", "Product"))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1
#> 
margin_from_types_byname(list(m, m), types = c("Industry", "Industry"))
#> [[1]]
#> [1] 2
#> 
#> [[2]]
#> [1] 2
#> 
margin_from_types_byname(list(m, m), types = c("Product", "Industry"))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
margin_from_types_byname(list(m, m), types = list("Product", "Industry"))
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 2
#> 
margin_from_types_byname(list(m, m), types = list(c("Product", "Industry")))
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2
#> 
margin_from_types_byname(list(m, m), types = list(c("Product", "Industry"), 
                                                  c("Product", "Industry")))
#> [[1]]
#> [1] 1 2
#> 
#> [[2]]
#> [1] 1 2
#> 
# Works in a data frame
m2 <- matrix(2) %>%
  setrowtype("Industry") %>% setcoltype("Product")
df <- tibble::tibble(m = list(m, m2), types = list("Product", c("Product", "Industry")))
res <- df %>%
  dplyr::mutate(
    margin = margin_from_types_byname(m, types)
 )
res$margin
#> [[1]]
#> [1] 1
#> 
#> [[2]]
#> [1] 1 2
#> 
```
