## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(dplyr)
library(matsbyname)
library(tibble)

## -----------------------------------------------------------------------------
mysum <- function(a, margin = c(1, 2)) {
  sum_func <- function(a_mat, margin) {
    # When we get here, we will have a single matrix a
    if (1 %in% margin & 2 %in% margin) {
      return(sum(a_mat))
    }
    if (margin == 1) {
      return(rowSums(a_mat) %>% matrix(nrow = nrow(a_mat)))
    }
    if (margin == 2) {
      return(colSums(a_mat) %>% matrix(ncol = ncol(a_mat)))
    }
  }
  unaryapply_byname(sum_func, a, .FUNdots = list(margin = margin))
}

## -----------------------------------------------------------------------------
m <- matrix(1:4, nrow = 2, byrow = TRUE)
m
# Works for single matrices
mysum(m, margin = 1)
mysum(m, margin = 2)
mysum(m, margin = c(1, 2))

## -----------------------------------------------------------------------------
# Works for lists of matrices
mysum(list(one = m, two = m), margin = 1)
mysum(list(one = m, two = m), margin = 2)

## -----------------------------------------------------------------------------
# Works in data frames and tibbles
DF <- tibble::tibble(mcol = list(m, m, m))
res <- DF %>% 
  dplyr::mutate(
    rsums = mysum(mcol, margin = 1), 
    csums = mysum(mcol, margin = 2)
  )
res$rsums
res$csums

## -----------------------------------------------------------------------------
tryCatch(mysum(list(m, m, m), margin = c(1, 2)), 
         error = function(e) {strwrap(e, width = 60)})

## -----------------------------------------------------------------------------
mysum(list(m, m), margin = c(1, 2))

## -----------------------------------------------------------------------------
mysum(list(m, m, m), margin = list(c(1, 2)))

## -----------------------------------------------------------------------------
mysum(list(m, m, m), margin = list(1, 2, c(1, 2)))

## -----------------------------------------------------------------------------
tryCatch(mysum(list(m, m, m), margin = list(1, 2)), 
         error = function(e) {strwrap(e, width = 60)})

## -----------------------------------------------------------------------------
mysum2 <- function(a, margin = c(1, 2)) {
  margin <- prep_vector_arg(a, margin)
  sum_func <- function(a_mat, margin) {
    # When we get here, we will have a single matrix a
    if (1 %in% margin & 2 %in% margin) {
      return(sum(a_mat))
    }
    if (margin == 1) {
      return(rowSums(a_mat) %>% matrix(nrow = nrow(a_mat)))
    }
    if (margin == 2) {
      return(colSums(a_mat) %>% matrix(ncol = ncol(a_mat)))
    }
  }
  unaryapply_byname(sum_func, a, .FUNdots = list(margin = margin))
}

## -----------------------------------------------------------------------------
mysum2(list(m, m, m), margin = c(1, 2))

## -----------------------------------------------------------------------------
mysum2(list(m, m), margin = c(1, 2))

## -----------------------------------------------------------------------------
mysum2(list(m, m), margin = list(c(1, 2)))

## -----------------------------------------------------------------------------
DF2 <- tibble::tibble(mcol = list(m, m), margin = c(1, 2))
DF2
DF2$margin %>% class()

## -----------------------------------------------------------------------------
res2 <- DF2 %>% 
  dplyr::mutate(
    sums = mysum2(mcol, margin = margin)
  )
res2$sums

## -----------------------------------------------------------------------------
DF3 <- tibble::tibble(mcol = list(m, m, m), margin = list(1, c(1, 2), c(1, 2))) %>% 
  dplyr::mutate(
    sumcol = mysum2(mcol, margin = margin)
  )
DF3$sumcol

