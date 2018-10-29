## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(magrittr)
library(dplyr)
library(parallel)
library(matsbyname)

## ------------------------------------------------------------------------
U <- matrix(1:4, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
U
difference_byname(0, U)
unaryapply_byname(`-`, U)

## ------------------------------------------------------------------------
U <- matrix(1:4, ncol = 2, dimnames = list(c("p1", "p2"), c("i1", "i2"))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
U
Y <- matrix(1:4, ncol = 2, dimnames = list(c("p2", "p1"), c("i2", "i1"))) %>%
  setrowtype("Products") %>% setcoltype("Industries")
Y
sum_byname(U, Y)
binaryapply_byname(`+`, U, Y)

## ------------------------------------------------------------------------
cumapply_byname(sum_byname, list(1, 2, 3, 4))
cumapply_byname(elementproduct_byname, list(1, 2, 3, 4))

