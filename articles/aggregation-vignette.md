# Aggregation in \`matsbyname\`

## Introduction

The `matsbyname` package provides several functions to assist renaming
and aggregating rows and columns of matrices. This vignette shows how to
use those functions.

## `aggregate_byname()`

By default
(`aggregation_map = NULL, margin = c(1,2), pattern_type = "exact"`),
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md)
sums all rows and columns with the same names, with the effect that
remaining row and column names are unique.

``` r
m <- matrix(c(1, 2, 3, 4, 
              5, 6, 7, 8, 
              9, 10, 11, 12), nrow = 3, ncol = 4, byrow = TRUE,
            dimnames = list(c("duck", "duck", "goose"), 
                            c("John", "Paul", "George", "Ringo")))
m
#>       John Paul George Ringo
#> duck     1    2      3     4
#> duck     5    6      7     8
#> goose    9   10     11    12
aggregate_byname(m)
#>       George John Paul Ringo
#> duck      10    6    8    12
#> goose     11    9   10    12
```

An `aggregation_map` can be provided, giving instructions for rows or
columns to aggregate and the names of the results. `aggregation_map`
should be a list of named strings. The entries in the list give names of
rows and columns to be aggregated. The names of the list entries provide
the names of the resulting aggregates.

``` r
m
#>       John Paul George Ringo
#> duck     1    2      3     4
#> duck     5    6      7     8
#> goose    9   10     11    12
aggregate_byname(m, aggregation_map = list(birds = c("duck", "goose"), 
                                           guitarists = c("John", "Paul", "George")))
#>       guitarists Ringo
#> birds         54    24
```

The margin over which the aggregation is to be performed is given by the
`margin` argument (`1` for rows, `2` for columns).

``` r
m
#>       John Paul George Ringo
#> duck     1    2      3     4
#> duck     5    6      7     8
#> goose    9   10     11    12
aggregate_byname(m, aggregation_map = list(Beatles = c("John", "Paul", "George", "Ringo")), 
                 margin = 2)
#>       Beatles
#> duck       10
#> duck       26
#> goose      42
```

`aggregation_map` can use regular expressions to identify rows and
columns to aggregate. Use `pattern_type = "literal"` for this feature.

``` r
m
#>       John Paul George Ringo
#> duck     1    2      3     4
#> duck     5    6      7     8
#> goose    9   10     11    12
aggregate_byname(m, aggregation_map = list(guitarists = "^[JPG]"), 
                 margin = 2, pattern_type = "literal")
#>       guitarists Ringo
#> duck           6     4
#> duck          18     8
#> goose         30    12
```

Note that rows and columns of aggregated matrices are always sorted
alphabetically.

``` r
m
#>       John Paul George Ringo
#> duck     1    2      3     4
#> duck     5    6      7     8
#> goose    9   10     11    12
aggregate_byname(m, aggregation_map = list(birds = c("duck", "goose"), 
                                           zguitarists = c("John", "Paul", "George")))
#>       Ringo zguitarists
#> birds    24          54
```

It is an error to aggregate over a margin and leave identically named
rows or columns. The following function call will fail, because it
aggregates over both rows and columns (using the default
`margin = c(1,2)`) with nothing in the `aggregation_map` to aggregate
the two “duck” rows. The error is informative: “Row names not unique.
Duplicated row names are: duck”.

``` r
# Not run
aggregate_byname(m, aggregation_map = list(Beatles = c("John", "Paul", "George", "Ringo")))
```

## Pieces

Commonly, row and column names are complex, carrying information in
prefixes, suffixes, and prepositional phrases. `matsbyname` can
aggregate by pieces of a name, using the `RCLabels` package internally.

We’ll use the following matrix to demonstrate aggregating by pieces. The
rows and columns use different notations for names
([`RCLabels::bracket_notation`](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html)
for rows and
[`RCLabels::arrow_notation`](https://matthewheun.github.io/RCLabels/reference/arrow_notation.html)
for columns). The renaming and aggregation capabilities of `matsbyname`
still work, despite the different notations.

``` r
m_pieces <- matrix(c(1, 2, 3,
                     4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                   dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                                   c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
```

## `rename_to_piece_byname()`

Rows and columns can be renamed to their prefixes, suffixes, or objects
of prepositions, as demonstrated below.

``` r
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
rename_to_piece_byname(m_pieces, piece = "pref", margin = 1, 
                       notation = RCLabels::bracket_notation)
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity            1          2                  3
#> Electricity            4          5                  6
rename_to_piece_byname(m_pieces, piece = "suff", margin = 1, 
                       notation = RCLabels::bracket_notation)
#>            Motors -> MD Cars -> MD LED lamps -> Light
#> from Coal             1          2                  3
#> from Solar            4          5                  6
rename_to_piece_byname(m_pieces, piece = "from", margin = 1, 
                       notation = RCLabels::bracket_notation)
#>       Motors -> MD Cars -> MD LED lamps -> Light
#> Coal             1          2                  3
#> Solar            4          5                  6
rename_to_piece_byname(m_pieces, piece = "pref", margin = 2,
                       notation = RCLabels::arrow_notation)
#>                          Motors Cars LED lamps
#> Electricity [from Coal]       1    2         3
#> Electricity [from Solar]      4    5         6
rename_to_piece_byname(m_pieces, piece = "suff", margin = 2,
                       notation = RCLabels::arrow_notation)
#>                          MD MD Light
#> Electricity [from Coal]   1  2     3
#> Electricity [from Solar]  4  5     6
```

In the examples above, renaming was accomplished by specifying the
`notation` for row or column names. But notation for row and column
labels can also be inferred via
[`RCLabels::infer_notation()`](https://matthewheun.github.io/RCLabels/reference/infer_notation.html).
To infer notation when renaming, set `inf_notation = TRUE` (the default)
and give a list of notations from which the notation can be inferred in
the `notation` argument. By default,
`notation = list(RCLabels::notations_list)`, because each notation in
[`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html)
is a list itself.

``` r
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
rename_to_piece_byname(m_pieces, piece = "pref", margin = 1)
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity            1          2                  3
#> Electricity            4          5                  6
```

When inferring notation, both margins can be renamed at the same time,
despite having different notations.

``` r
rename_to_piece_byname(m_pieces, piece = "pref", margin = c(1, 2))
#>             Motors Cars LED lamps
#> Electricity      1    2         3
#> Electricity      4    5         6
```

But `margin = list(c(1, 2))` is the default, so the code can be simpler
still.

``` r
rename_to_piece_byname(m_pieces, piece = "pref")
#>             Motors Cars LED lamps
#> Electricity      1    2         3
#> Electricity      4    5         6
rename_to_piece_byname(m_pieces, piece = "suff")
#>            MD MD Light
#> from Coal   1  2     3
#> from Solar  4  5     6
```

Sometimes, a row or column label can match more than one possible
`notation`. In the above example, the row names are inferred to conform
to
[`RCLabels::bracket_notation`](https://matthewheun.github.io/RCLabels/reference/bracket_notation.html),
the first match in
[`RCLabels::notations_list`](https://matthewheun.github.io/RCLabels/reference/notations_list.html).  
To specify the most-specific notation, set
`choose_most_specific = TRUE`. With `choose_most_specific = TRUE`,
[`RCLabels::from_notation`](https://matthewheun.github.io/RCLabels/reference/from_notation.html)
is inferred, the suffixes are different, and the renamed rows no longer
contain “from”.

``` r
rename_to_piece_byname(m_pieces, piece = "suff", choose_most_specific = TRUE)
#>       MD MD Light
#> Coal   1  2     3
#> Solar  4  5     6
```

Note that “noun” is a synonym for “pref”.

``` r
rename_to_piece_byname(m_pieces, piece = "noun")
#>             Motors Cars LED lamps
#> Electricity      1    2         3
#> Electricity      4    5         6
```

The margin can be specified using row or column types from which the
numerical margin is inferred.

``` r
m_pieces_with_types <- m_pieces %>% 
  setrowtype("Product") %>% setcoltype("Industry")
m_pieces_with_types
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "pref", margin = "Product")
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity            1          2                  3
#> Electricity            4          5                  6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Product")
#>            Motors -> MD Cars -> MD LED lamps -> Light
#> from Coal             1          2                  3
#> from Solar            4          5                  6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "from", margin = "Product")
#>       Motors -> MD Cars -> MD LED lamps -> Light
#> Coal             1          2                  3
#> Solar            4          5                  6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Product", choose_most_specific = TRUE)
#>       Motors -> MD Cars -> MD LED lamps -> Light
#> Coal             1          2                  3
#> Solar            4          5                  6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
m_pieces_with_types %>% 
  rename_to_piece_byname(piece = "suff", margin = "Industry")
#>                          MD MD Light
#> Electricity [from Coal]   1  2     3
#> Electricity [from Solar]  4  5     6
#> attr(,"rowtype")
#> [1] "Product"
#> attr(,"coltype")
#> [1] "Industry"
```

Such renamings can be used for aggregations in which identically named
rows and columns are summed with
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md),
as demonstrated in section below.

## `aggregate_pieces_byname()`

[`aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.md)
bundles renaming and aggregating tasks in a single function call. First,
rows and/or columns are renamed to the requested `piece` with the
[`rename_to_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_piece_byname.md)
function. Then, aggregation is performed via
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md),
according to an `aggregation_map` and the `pattern_type`, if provided.
With the default `aggregation_map = NULL`, identically named pieces are
aggregated together.

``` r
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
# Aggregate Electricity in rows
aggregate_pieces_byname(m_pieces, piece = "pref", margin = 1, 
                        notation = RCLabels::bracket_notation)
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity            5          7                  9
# Aggregate useful energy types in columns
aggregate_pieces_byname(m_pieces, piece = "suff", margin = 2,
                        notation = RCLabels::arrow_notation)
#>                          Light MD
#> Electricity [from Coal]      3  3
#> Electricity [from Solar]     6  9
```

When an `aggregation_map` is supplied, it applies to the requested
`piece`, not to the original row and/or column names, as shown below.

``` r
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
# Aggregate by original energy type
aggregate_pieces_byname(m_pieces, piece = "from", margin = 1, 
                        notation = RCLabels::bracket_notation, 
                        aggregation_map = list(`All sources` = c("Coal", "Solar")))
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> All sources            5          7                  9

aggregate_pieces_byname(m_pieces, piece = "suff", margin = 2, 
                        notation = RCLabels::arrow_notation, 
                        aggregation_map = list(`Transport` = "MD"))
#>                          Light Transport
#> Electricity [from Coal]      3         3
#> Electricity [from Solar]     6         9
```

## Aggregations of lists and data frames of matrices

The functions for renaming and aggregating can be used on lists and data
frames of matrices.

``` r
m_pieces
#>                          Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity [from Coal]             1          2                  3
#> Electricity [from Solar]            4          5                  6
res <- rename_to_piece_byname(list(m_pieces, m_pieces), 
                              piece = list("pref", "suff"), 
                              margin = list(1, 2),
                              notation = list(RCLabels::bracket_notation, 
                                              RCLabels::arrow_notation))
res
#> [[1]]
#>             Motors -> MD Cars -> MD LED lamps -> Light
#> Electricity            1          2                  3
#> Electricity            4          5                  6
#> 
#> [[2]]
#>                          MD MD Light
#> Electricity [from Coal]   1  2     3
#> Electricity [from Solar]  4  5     6
df <- tibble::tibble(mats = list(m_pieces, m_pieces), 
                     pce = list("suff", "pref"), 
                     mgn = list(1, 2), 
                     am = list(list(Sources = c("Coal", "Solar")), 
                               list(Transport = c("Motors", "Cars"))), 
                     notn = list(RCLabels::from_notation, RCLabels::arrow_notation))
df
#> # A tibble: 2 × 5
#>   mats          pce       mgn       am               notn     
#>   <list>        <list>    <list>    <list>           <list>   
#> 1 <dbl [2 × 3]> <chr [1]> <dbl [1]> <named list [1]> <chr [4]>
#> 2 <dbl [2 × 3]> <chr [1]> <dbl [1]> <named list [1]> <chr [4]>
res2 <- df %>%
  dplyr::mutate(
    aggregated = aggregate_pieces_byname(mats, piece = pce, margin = mgn, 
                                         aggregation_map = am, notation = notn)
  )
res2
#> # A tibble: 2 × 6
#>   mats          pce       mgn       am               notn      aggregated   
#>   <list>        <list>    <list>    <list>           <list>    <list>       
#> 1 <dbl [2 × 3]> <chr [1]> <dbl [1]> <named list [1]> <chr [4]> <dbl [1 × 3]>
#> 2 <dbl [2 × 3]> <chr [1]> <dbl [1]> <named list [1]> <chr [4]> <dbl [2 × 2]>
res2$aggregated[[1]]
#>         Motors -> MD Cars -> MD LED lamps -> Light
#> Sources            5          7                  9
res2$aggregated[[2]]
#>                          LED lamps Transport
#> Electricity [from Coal]          3         3
#> Electricity [from Solar]         6         9
```

## Aggregation via `dplyr::summarise()`

Another type of aggregation is aided by the metadata columns of a
`matsindf`-style data frame. With single numbers, an aggregation might
look like this:

``` r
df_simple <- tibble::tribble(~key, ~val, 
                             "A", 1, 
                             "A", 2, 
                             "B", 10)
df_simple
#> # A tibble: 3 × 2
#>   key     val
#>   <chr> <dbl>
#> 1 A         1
#> 2 A         2
#> 3 B        10
df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(val = sum(val))
#> # A tibble: 2 × 2
#>   key     val
#>   <chr> <dbl>
#> 1 A         3
#> 2 B        10
```

The same aggregation gives unexpected results with the default arguments
to the
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
function (specifically, `.summarise = FALSE`), because
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md)
is ambiguous for a data frame. Should the column be returned unchanged,
because each element is interpreted as the augend for a series of sums
that is missing addends, in which case the length of the returned object
is the same as the length of the input? Or should the list of objects be
summed down the column, returning only a single item (for each group),
as in the
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
function? (See the vignette titled “Using summarise in matsbyname” for
additional detail about this ambiguity.) In the example below, the
grouping has no effect on the
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
function, because `sum_byname(.summarise = FALSE)` assumes that each row
of `val` is an augend without an addend. Furthermore,
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
will error, because it no longer allows returning more than one row per
group.
[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
must be used instead.

``` r
# 2 rows are expected. 3 are observed.
df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::reframe(val = sum_byname(val))
#> # A tibble: 3 × 2
#>   key     val
#>   <chr> <dbl>
#> 1 A         1
#> 2 A         2
#> 3 B        10
```

To signal intention to summarise down the `val` column, set
`.summarise = TRUE` in the call to
[`sum_byname()`](https://matthewheun.github.io/matsbyname/reference/sum_byname.md).
Note that `sum_byname(.summarise = TRUE)` *always* returns a list
column, because if the summarised column were to contain matrices, it
*must* be a list column.

``` r
res <- df_simple %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(val = sum_byname(val, .summarise = TRUE))
# res$val is a list column.
res
#> # A tibble: 2 × 2
#>   key   val      
#>   <chr> <list>   
#> 1 A     <dbl [1]>
#> 2 B     <dbl [1]>
res$val
#> [[1]]
#> [1] 3
#> 
#> [[2]]
#> [1] 10
```

The `.summarise = TRUE` argument works when there are matrices in a
`matsindf` data frame, too.

``` r
m <- matrix(c(11, 12, 13,
              21, 22, 23), nrow = 2, ncol = 3, byrow = TRUE, 
            dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
df <- tibble::tibble(key = c("A", "A", "B"), m = list(m, m, m))
unexpected <- df %>% 
  dplyr::group_by(key) %>% 
  dplyr::reframe(m = sum_byname(m))
# 2 rows are expected. 3 are observed.
unexpected
#> # A tibble: 3 × 2
#>   key   m            
#>   <chr> <list>       
#> 1 A     <dbl [2 × 3]>
#> 2 A     <dbl [2 × 3]>
#> 3 B     <dbl [2 × 3]>
res <- df %>% 
  dplyr::group_by(key) %>% 
  dplyr::summarise(m = sum_byname(m, .summarise = TRUE))
res
#> # A tibble: 2 × 2
#>   key   m            
#>   <chr> <list>       
#> 1 A     <dbl [2 × 3]>
#> 2 B     <dbl [2 × 3]>
res$m[[1]]
#>    c1 c2 c3
#> r1 22 24 26
#> r2 42 44 46
res$m[[2]]
#>    c1 c2 c3
#> r1 11 12 13
#> r2 21 22 23
```

## Working with aggregation maps

An aggregation map is defined to be a named list. But the source of that
named list is often a data frame in which many-to-one relationships are
defined.
[`agg_table_to_agg_map()`](https://matthewheun.github.io/matsbyname/reference/aggregation_map_helpers.md)
assists converting from a two-column data frame to an aggregation map.

``` r
df <- tibble::tribble(~member, ~role, ~band, 
                      "John", "guitarists", "The Beatles", 
                      "Paul", "guitarists", "The Beatles", 
                      "George", "guitarists", "The Beatles", 
                      "Ringo", "drummers", "The Beatles", 
                      "Mick", "singers", "Rolling Stones", 
                      "Keith", "guitarists", "Rolling Stones", 
                      "Ronnie", "guitarists", "Rolling Stones", 
                      "Bill", "guitarists", "Rolling Stones", 
                      "Charlie", "drummers", "Rolling Stones")
df
#> # A tibble: 9 × 3
#>   member  role       band          
#>   <chr>   <chr>      <chr>         
#> 1 John    guitarists The Beatles   
#> 2 Paul    guitarists The Beatles   
#> 3 George  guitarists The Beatles   
#> 4 Ringo   drummers   The Beatles   
#> 5 Mick    singers    Rolling Stones
#> 6 Keith   guitarists Rolling Stones
#> 7 Ronnie  guitarists Rolling Stones
#> 8 Bill    guitarists Rolling Stones
#> 9 Charlie drummers   Rolling Stones
bands_membs_agg_map <- agg_table_to_agg_map(df, few_colname = "band", many_colname = "member")
bands_membs_agg_map
#> $`Rolling Stones`
#> [1] "Mick"    "Keith"   "Ronnie"  "Bill"    "Charlie"
#> 
#> $`The Beatles`
#> [1] "John"   "Paul"   "George" "Ringo"
agg_table_to_agg_map(df, few_colname = "role", many_colname = "member")
#> $drummers
#> [1] "Ringo"   "Charlie"
#> 
#> $guitarists
#> [1] "John"   "Paul"   "George" "Keith"  "Ronnie" "Bill"  
#> 
#> $singers
#> [1] "Mick"
```

In a similar manner, an aggregation map can be converted to a data frame
to assist with join operations with data frames.

``` r
agg_map_to_agg_table(bands_membs_agg_map, 
                      few_colname = "bands",
                      many_colname = "members")
#> # A tibble: 9 × 2
#>   bands          members
#>   <chr>          <chr>  
#> 1 Rolling Stones Mick   
#> 2 Rolling Stones Keith  
#> 3 Rolling Stones Ronnie 
#> 4 Rolling Stones Bill   
#> 5 Rolling Stones Charlie
#> 6 The Beatles    John   
#> 7 The Beatles    Paul   
#> 8 The Beatles    George 
#> 9 The Beatles    Ringo
```

## Summary

The `matsbyname` package simplifies aggregation of matrix rows and
columns based on row and column names or pieces of row and column names.
In particular the functions
[`aggregate_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_byname.md),
[`rename_to_piece_byname()`](https://matthewheun.github.io/matsbyname/reference/rename_to_piece_byname.md),
and
[`aggregate_pieces_byname()`](https://matthewheun.github.io/matsbyname/reference/aggregate_pieces_byname.md)
provide flexibility in how renaming and aggregation can be accomplished.
When working with aggregation maps, the functions
[`agg_table_to_agg_map()`](https://matthewheun.github.io/matsbyname/reference/aggregation_map_helpers.md)
and
[`agg_map_to_agg_table()`](https://matthewheun.github.io/matsbyname/reference/aggregation_map_helpers.md)
assist conversion from one data shape to another.
