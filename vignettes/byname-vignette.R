## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(magrittr)
library(dplyr)
library(tidyr)
library(byname)
library(matsindf)

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
# Same as U_2000 + Y_2000, but U and Y3 are unmodified.
sum_byname(U, Y3)
# Eliminate zero-filled rows and columns. Same result as solve(U).
U_2000 %>% clean_byname(margin = c(1,2), clean_value = 0) %>% solve()

## ------------------------------------------------------------------------
A <- matrix(1:4, nrow = 2, ncol = 2) %>% 
  setrownames_byname(productnames) %>% setcolnames_byname(industrynames) %>% 
  setrowtype("Products") %>% setcoltype("Industries")
A
B <- matrix(8:5, nrow = 2, ncol = 2) %>% 
  setrownames_byname(productnames) %>% setcolnames_byname(industrynames) %>% 
  setrowtype("Products") %>% setcoltype("Industries")
B
C <- matrix(1:4, nrow = 2, ncol = 2) %>% 
  setcolnames_byname(productnames) %>% setrownames_byname(industrynames) %>% 
  setrowtype("Industries") %>% setcoltype("Products")
C

## ------------------------------------------------------------------------
sum_byname(A, B)

## ------------------------------------------------------------------------
tryCatch(sum_byname(A, C), error = function(err){print(err)})

## ------------------------------------------------------------------------
sum_byname(A, transpose_byname(C))

## ------------------------------------------------------------------------
elementproduct_byname(A, B)
elementquotient_byname(A, B)

## ------------------------------------------------------------------------
matrixproduct_byname(A, C)

## ------------------------------------------------------------------------
tryCatch(matrixproduct_byname(A, B), error = function(err){print(err)})

## ------------------------------------------------------------------------
sum_byname(A, list(B, B))
elementproduct_byname(list(A, A), B)
matrixproduct_byname(list(A, A), list(C, C))

## ------------------------------------------------------------------------
tidy <- data.frame(
  matrix = c("A", "A", "A", "A", "B", "B", "B", "B"),
  row = c("p1", "p1", "p2", "p2", "p1", "p1", "p2", "p2"),
  col = c("i1", "i2", "i1", "i2", "i1", "i2", "i1", "i2"),
  vals = c(1, 3, 2, 4, 8, 6, 7, 5)
) %>% 
  mutate(
    rowtype = "Industries",
    coltype  = "Products"
  ) %>% 
  group_by(matrix)
tidy
mats <- tidy %>% 
  collapse_to_matrices(matnames = "matrix", values = "vals", 
                       rownames = "row", colnames = "col", 
                       rowtypes = "rowtype", coltypes = "coltype") %>% 
  rename(
    matrix.name = matrix,
    matrix = vals
  )
mats
mats$matrix[[1]]
mats$matrix[[2]]

## ------------------------------------------------------------------------
result <- mats %>% 
  spread(key = matrix.name, value = matrix) %>% 
  rbind(., .) %>% 
  mutate(
    c = 1:2,
    # Sums all rows of mats with a single instruction.
    sum = sum_byname(A, B),
    product = elementproduct_byname(c, sum)
  )
result
result$sum[[1]]
result$sum[[2]]
result$product[[1]]
result$product[[2]]

