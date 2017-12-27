## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(magrittr)
library(dplyr)
library(byname)

## ------------------------------------------------------------------------
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames))
U
Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames)))
Y
# Non-sensical.  Row and column names not respected.
U + Y 

## ------------------------------------------------------------------------
# Make a new version of Y (Y2), this time with dimnames in same order as U
Y2 <- matrix(4:1, ncol = 2, dimnames = list(productnames, industrynames))
Y2
# Now the sum is sensible. Neither row nor column names are respected.
U + Y2

## ------------------------------------------------------------------------
Y3 <- matrix(5:8, ncol = 2, dimnames = list(c("p1", "p3"), c("i1", "i3")))
Y3
# Non-sensical. Neither row nor column names are respected. Both "p3" and "i3" are missing from sum.
U + Y3
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
Y_2000
U_2000 + Y_2000

## ------------------------------------------------------------------------
solve(U)
tryCatch(solve(U_2000), error = function(err){print(err)})

## ------------------------------------------------------------------------
# Same as U + Y2, without needing to create Y2.
sum_byname(U, Y)
# Same as U_2000 + Y_2000, without needing to modify U and Y3.
sum_byname(U, Y3)
# Eliminate Same result as solve(U)
U_2000 %>% clean_byname(margin = c(1,2), clean_value = 0) %>% solve

