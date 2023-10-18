## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(dplyr)
library(matsbyname)
library(tibble)

## -----------------------------------------------------------------------------
df <- tibble::tribble(~x, ~y, ~z, 
                       1,  2,  3, 
                       4,  5,  6)
# Typically, operations are done across rows.
df %>% 
  dplyr::mutate(
    a = x + y + z,
    b = rowMeans(.)
  )

## -----------------------------------------------------------------------------
df %>% 
  dplyr::summarise(
    x = sum(x), 
    y = sum(y), 
    z = sum(z)
  )
df %>% 
  dplyr::summarise(
    x = mean(x), 
    y = mean(y), 
    z = mean(z)
  )

## -----------------------------------------------------------------------------
df %>% 
  dplyr::mutate(
    a = sum_byname(x, y, z), 
    b = mean_byname(x, y, z)
  )
df %>% 
  dplyr::summarise(
    x = sum_byname(x, .summarise = TRUE) %>% unlist(), 
    y = sum_byname(y, .summarise = TRUE) %>% unlist(), 
    z = sum_byname(z, .summarise = TRUE) %>% unlist()
  )

