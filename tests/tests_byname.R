# Contains tests for the byname package.

library(testthat)
library(byname)

context("Sums")

test_that("sums of constants", {
  
  # Simple sum of constants
  expect_equal(sum_byname(2, 3), 5)
})

test_that("sums of matrices", {
  # Matrices with commodity and industry names and types
  commoditynames <- c("c1", "c2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(commoditynames, industrynames)) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  Y <- matrix(1:4, ncol = 2, dimnames = list(rev(commoditynames), rev(industrynames))) %>%
    setrowtype("Commodities") %>% setcoltype("Industries")
  
  expect_equal(U + Y, 
               # This is a non-sensical test.  Row and column names are not respected. 
               # Row names, column names, and row and column types come from the first operand (U).
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  expect_equal(sum_byname(U, Y),
               # Now, row and column names are respected.
               matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  expect_equal(sum_byname(U, 100), U + 100)
  expect_equal(sum_byname(200, Y), Y + 200 %>% complete_and_sort())

})
  
  #' 
  #' V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, commoditynames)) %>%
  #'   setrowtype("Industries") %>% setcoltype("Commodities")
  #' U + V # row and column names are non-sensical and blindly taken from first argument (U)
  #' \dontrun{sum_byname(U, V)} # Fails, because row and column types are different
  #' # This also works with lists
  #' sum_byname(list(U,U), list(G,G))
  #' sum_byname(list(U,U), list(100,100))
  #' sum_byname(list(U,U), as.list(rep_len(100, 2)))
  #' DF <- data.frame(U = I(list()), G = I(list()))
  #' DF[[1,"U"]] <- U
  #' DF[[2,"U"]] <- U
  #' DF[[1,"G"]] <- G
  #' DF[[2,"G"]] <- G
  #' sum_byname(DF$U, DF$G)
  #' DF %>% mutate(sums = sum_byname(U, G))
  #' sum_byname(U) # If only one argument, return it.
  #' sum_byname(2, NULL) # Gives 2
  #' sum_byname(2, NA)   # Gives NA
  #' sum_byname(NULL, 1) # Gives 1
  #' sum_byname(list(NULL, 1), list(1, 1))
  #' DF2 <- data.frame(U = I(list()), G = I(list()))
  #' DF2[[1,"U"]] <- NULL
  #' DF2[[2,"U"]] <- U
  #' DF2[[1,"G"]] <- G
  #' DF2[[2,"G"]] <- G
  #' sum_byname(DF2$U, DF2$G)
  #' DF3 <- DF2 %>% mutate(sums = sum_byname(U, G))
  #' DF3
  #' DF3$sums[[1]]
  #' DF3$sums[[2]]