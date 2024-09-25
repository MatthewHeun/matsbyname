test_that("redistribute_byname() errors as expected", {
  expect_error(
    redistribute_byname(a = 42, margin = c(1, 2, 3)), 
    "margin must have length 1 or 2 in matsbyname::redistribute_byname\\(\\)"
  )
  expect_error(
    redistribute_byname(a = 42, margin = 4), 
    "margin must be 1, 2, or c\\(1, 2\\) in matsbyname::redistribute_byname\\(\\)"
  )
  expect_error(
    redistribute_byname(a = 42, margin = c(1, 1)), 
    "margin must contain unique integers in matsbyname::redistribute_byname\\(\\)"
  )
})


test_that("redistribute_byname() works with row redistribution", {
  a <- matrix(c(1, 2, 
                3, 4, 
                5, 6), 
              nrow = 3, ncol = 2, byrow = TRUE, 
              dimnames = list(c("r1", "r2", "r3"), 
                              c("c1", "c2")))
  
  res <- redistribute_byname(a, "r3", margin = 1)
  
  expected <- matrix(c(1 + 1/4*5, 2 + 2/6*6, 
                       3 + 3/4*5, 4 + 4/6*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
})



test_that("redistribute_byname() works with column redistribution", {
  a <- matrix(c(1, 2, 3,
                4, 5, 6), 
              nrow = 2, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), 
                              c("c1", "c2", "c3")))
  
  res <- redistribute_byname(a, "c3", margin = 2)
  
  expected <- matrix(c(1 + 1/3*3, 2 + 2/3*3, 
                       4 + 4/9*6, 5 + 5/9*6), 
                     nrow = 2, ncol = 2, byrow = TRUE, 
                     dimnames = list(c("r1", "r2"), 
                                     c("c1", "c2")))
  expect_equal(res, expected)
})
