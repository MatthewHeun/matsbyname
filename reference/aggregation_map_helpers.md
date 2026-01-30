# Aggregation map conversions

Aggregation is a many-to-few operation where specifics are summed to
comprise broader categories. Examples include "John", "Paul", "George",
and "Ringo" aggregated to "Beatles"; and "Mick", "Keith", "Ronnie",
"Bill", and "Charlie" aggregated to "Stones". An aggregation map is a
named list that describes the aggregation to be performed. An
aggregation map for the examples above is
`list(Beatles = c("John", "Paul", "George", "Ringo"), Stones = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"))`
Aggregation maps can be generated from many shapes of data. These
functions assist with translating from different data shapes to
aggregation maps.

## Usage

``` r
agg_table_to_agg_map(.df, few_colname, many_colname)

agg_map_to_agg_table(aggregation_map, few_colname, many_colname)
```

## Arguments

- .df:

  A data frame from which an aggregation map is to be extracted.

- few_colname:

  The string name of a column in a data frame that corresponds to the
  "few" aggregated categories.

- many_colname:

  The string name of a column in a data frame that corresponds to the
  "many" specific items that will be aggregated.

- aggregation_map:

  An aggregation map to be converted to a data frame.

## Value

For `agg_table_to_agg_map()`, an aggregation map. For
`agg_map_to_agg_table()`, a `data.frame`, probably at `tibble`.

## Examples

``` r
bands <- tibble::tribble(~band, ~members, 
                         "The Beatles", "John", 
                         "The Beatles", "Paul", 
                         "The Beatles", "George", 
                         "The Beatles", "Ringo", 
                         # Rejects duplicates and NA
                         "The Beatles", "Ringo",
                         "The Beatles", NA, 
                         "Rolling Stones", "Mick", 
                         "Rolling Stones", "Keith",
                         "Rolling Stones", "Ronnie",
                         "Rolling Stones", "Bill",
                         "Rolling Stones", "Charlie")
agg_map <- agg_table_to_agg_map(bands, 
                                few_colname = "band",
                                many_colname = "members")
agg_map
#> $`Rolling Stones`
#> [1] "Mick"    "Keith"   "Ronnie"  "Bill"    "Charlie"
#> 
#> $`The Beatles`
#> [1] "John"   "Paul"   "George" "Ringo" 
#> 
agg_map_to_agg_table(agg_map, few_colname = "bands", many_colname = "members")
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
