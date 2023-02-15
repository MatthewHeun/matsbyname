
test_that("sum_byname() of constants works as expected", {
  # Simple sum of constants
  expect_equal(sum_byname(2, 3), 5)
  expect_equal(sum_byname(2, 3, 4), 9)
  
  # If summed against NULL, return the item.
  expect_equal(sum_byname(NULL, 1), 1)
  expect_equal(sum_byname(2, NULL), 2)
  expect_equal(sum_byname(2, NULL, NULL), 2)
  expect_equal(sum_byname(NULL, NULL, 2, NULL, NULL), 2)
  expect_equal(sum_byname(list(NULL, 1), list(2, 3)), list(2, 4))
  expect_equal(sum_byname(list(NULL, 1, 2), list(2, 3, NULL), list(4, NULL, 5)), list(6, 4, 7))
  # If summed against NA, return NA
  expect_equal(sum_byname(2, NA), NA_integer_)
  expect_equal(sum_byname(3, 2, NA), NA_integer_)
})


test_that("sum_byname() of matrices works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")

    # If only one argument, return it.
  expect_equal(sum_byname(U), U)
  
  Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Z <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  
  # This is a non-sensical test.  Row and column names are not respected. 
  # Row names, column names, and row and column types come from the first operand (U).
  expect_equal(U + Y, 
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  # Now, row and column names are respected.
  UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
  expect_equal(sum_byname(U, Y), UplusY)
  expect_equal(sum_byname(U, Y, Z), sum_byname(U, Z) %>% sum_byname(Y))
  
  expect_equal(sum_byname(U, 100), U + 100)
  expect_equal(sum_byname(200, Y), 200 + Y %>% sort_rows_cols() %>% 
                 setrowtype(rowtype(Y)) %>% 
                 setcoltype(coltype(Y)))
  
  # This is a non-sensical test.  Row and column names are not respected.
  # Row names, column names, and row and column types come from the first operand (U).
  V <- matrix(1:4, ncol = 2, dimnames = list(industrynames, productnames)) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(U + V,
               matrix(c(2, 4, 6, 8), ncol = 2, dimnames = dimnames(U)) %>% 
                 setrowtype(rowtype(U)) %>% setcoltype(coltype(U)))
  
  # We should not be able to add U and V, because their row and column types differ.
  # Would like to test for entire error message which is 
  # "rowtype(a) == rowtype(b) is not TRUE"
  # However, it seems that the testthat package has trouble dealing with "(" in error messages.
  # So, we'll just test for the first word.
  expect_error(sum_byname(U, V), "rowtype")
})


test_that("sum_byname() gives error when they are vectors without row names", {
  a <- matrix(c(1, 2, 3), nrow = 1, dimnames = list(NULL, c("c1", "c2", "c3"))) %>% 
    setcoltype("coltype")
  b <- 2 * a
  
  expect_error(sum_byname(a, b), "NULL dimnames for margin = 1 on a")
})


test_that("sum_byname() of matrices in lists and data frames works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Z <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")

  UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
  UYZ <- sum_byname(U, Y) %>% sum_byname(Z)
  Uplus100 <- U + 100

  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Y = I(list()), Z = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z
  
  # sum_byname should also work with lists.
  expect_equal(sum_byname(list(U,U), list(Y, Y)), list(UplusY, UplusY))
  expect_equal(sum_byname(list(U,U), list(100,100)), list(Uplus100, Uplus100))
  expect_equal(sum_byname(list(U,U), as.list(rep_len(100, 2))), list(Uplus100, Uplus100))
  
  # sum_byname also should work with data frames, as they are lists.
  expect_equal(sum_byname(DF$U, DF$Y), list(UplusY, UplusY))
  expect_equal(DF %>% dplyr::mutate(sums = sum_byname(U, Y)), 
               DF %>% dplyr::mutate(sums = list(UplusY, UplusY)))
  
  # And sum_byname should work with more than 2 operands.
  expect_equal(sum_byname(DF$U, DF$Y, DF$Z), list(UYZ, UYZ))
})


test_that("sum_byname() of Matrix objects in lists and data frames works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- Matrix::Matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- Matrix::Matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Z <- Matrix::Matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  
  UplusY <- Matrix::Matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
  UYZ <- sum_byname(U, Y) %>% sum_byname(Z)
  Uplus100 <- U + 100
  
  # Define a data frame to be used with testing below.
  DF <- data.frame(U = I(list()), Y = I(list()), Z = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF[[1,"Y"]] <- Y
  DF[[2,"Y"]] <- Y
  DF[[1,"Z"]] <- Z
  DF[[2,"Z"]] <- Z
  
  # sum_byname should also work with lists.
  expect_equal(sum_byname(list(U,U), list(Y, Y)), list(UplusY, UplusY))
  expect_equal(sum_byname(list(U,U), list(100,100)), list(Uplus100, Uplus100))
  expect_equal(sum_byname(list(U,U), as.list(rep_len(100, 2))), list(Uplus100, Uplus100))
  
  # sum_byname also should work with data frames, as they are lists.
  expect_equal(sum_byname(DF$U, DF$Y), list(UplusY, UplusY))
  expect_equal(DF %>% dplyr::mutate(sums = sum_byname(U, Y)), 
               DF %>% dplyr::mutate(sums = list(UplusY, UplusY)))
  
  # And sum_byname should work with more than 2 operands.
  expect_equal(sum_byname(DF$U, DF$Y, DF$Z), list(UYZ, UYZ))
})


test_that("sum_byname() of matrices that are in lists in a cell of a data frame works as expected", {
  test_func <- function(this_U, this_Y, this_Z) {
    UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(this_U)) %>%
      setrowtype(rowtype(this_U)) %>% setcoltype(coltype(this_U))
    UYZ <- sum_byname(this_U, this_Y, this_Z)
    
    ulist <- list(this_U, this_U)
    ylist <- list(this_Y, this_Y)
    zlist <- list(this_Z, this_Z)
    DF <- data.frame(Ulist = I(list()), Ylist = I(list()), Zlist = I(list()))
    # Put lists in each cell of the data frame.
    DF[[1,"Ulist"]] <- ulist
    DF[[2,"Ulist"]] <- ulist
    DF[[1,"Ylist"]] <- ylist
    DF[[2,"Ylist"]] <- ylist
    DF[[1,"Zlist"]] <- zlist
    DF[[2,"Zlist"]] <- zlist
    
    # Operate on the lists in each cell of the data frame.
    res <- DF %>% 
      dplyr::mutate(
        sum = sum_byname(Ulist, Ylist),
        bigsum = sum_byname(Ulist, Ylist, Zlist)
      )
    matsbyname:::expect_equal_matrix_or_Matrix(res$sum[[1]][[1]], UplusY)
    matsbyname:::expect_equal_matrix_or_Matrix(res$sum[[1]][[2]], UplusY)
    matsbyname:::expect_equal_matrix_or_Matrix(res$sum[[2]][[1]], UplusY)
    matsbyname:::expect_equal_matrix_or_Matrix(res$sum[[2]][[2]], UplusY)
    
    matsbyname:::expect_equal_matrix_or_Matrix(res$bigsum[[1]][[1]], UYZ)
    matsbyname:::expect_equal_matrix_or_Matrix(res$bigsum[[1]][[2]], UYZ)
    matsbyname:::expect_equal_matrix_or_Matrix(res$bigsum[[2]][[1]], UYZ)
    matsbyname:::expect_equal_matrix_or_Matrix(res$bigsum[[2]][[2]], UYZ)
  }
  
  # Test with matrix objects
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  Um <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Ym <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Zm <- matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  
  test_func(Um, Ym, Zm)

  # Test with Matrix objects
  UM <- Matrix::Matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  YM <- Matrix::Matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  ZM <- Matrix::Matrix(rev(1:4), ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  
  test_func(UM, YM, ZM)
})


test_that("sum_byname() of matrices that are in lists in a cell of a data frame works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matrix(1:4, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")

  UplusY <- matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))

  # Now check to see what happens when one of the operands
  # is a list and the other is not.
  DF2 <- data.frame(ulist2_col = I(list()), Y = I(list()))
  # Put lists in each cell of the data frame.
  ulist2 <- list(U, U)
  DF2[[1,"ulist2_col"]] <- ulist2
  DF2[[2,"ulist2_col"]] <- ulist2
  DF2[[1,"Y"]] <- Y
  DF2[[2,"Y"]] <- Y
  res2 <- DF2 %>% 
    dplyr::mutate(
      sum = sum_byname(ulist2_col, Y)
    )
  expect_equal(res2$sum[[1]][[1]], UplusY)
  expect_equal(res2$sum[[1]][[2]], UplusY)
  expect_equal(res2$sum[[2]][[1]], UplusY)
  expect_equal(res2$sum[[2]][[2]], UplusY)
  
  # Try when the matrix length will not be a multiple of the list length.
  U3 <- matrix(1:5, nrow = 5, ncol = 1, dimnames = list(c("p1", "p2", "p3", "p4", "p5"), "i1")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  U3plusY <- sum_byname(U3, Y)
  ulist3 <- list(U3, U3, U3)
  DF3 <- data.frame(ulist3_col = I(list()), Y = I(list()))
  DF3[[1,"ulist3_col"]] <- ulist3
  DF3[[2,"ulist3_col"]] <- ulist3
  DF3[[3,"ulist3_col"]] <- ulist3
  DF3[[1,"Y"]] <- Y
  DF3[[2,"Y"]] <- Y
  DF3[[3,"Y"]] <- Y
  res3 <- DF3 %>% 
    dplyr::mutate(
      sum = sum_byname(ulist3_col, Y)
    )
  expect_equal(res3$sum[[1]][[1]], U3plusY)
  expect_equal(res3$sum[[1]][[2]], U3plusY)
  expect_equal(res3$sum[[1]][[3]], U3plusY)
  expect_equal(res3$sum[[2]][[1]], U3plusY)
  expect_equal(res3$sum[[2]][[2]], U3plusY)
  expect_equal(res3$sum[[2]][[3]], U3plusY)
  expect_equal(res3$sum[[3]][[1]], U3plusY)
  expect_equal(res3$sum[[3]][[2]], U3plusY)
  expect_equal(res3$sum[[3]][[3]], U3plusY)
})


test_that("sum_byname() of matrices that are in lists in a cell of a data frame works with Matrix objects", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames)) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  Y <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(rev(productnames), rev(industrynames))) %>%
    setrowtype("Products") %>% setcoltype("Industries")
  
  UplusY <- matsbyname::Matrix(5, nrow = 2, ncol = 2, dimnames = dimnames(U)) %>%
    setrowtype(rowtype(U)) %>% setcoltype(coltype(U))
  
  # Now check to see what happens when one of the operands
  # is a list and the other is not.
  DF2 <- data.frame(ulist2_col = I(list()), Y = I(list()))
  # Put lists in each cell of the data frame.
  ulist2 <- list(U, U)
  DF2[[1,"ulist2_col"]] <- ulist2
  DF2[[2,"ulist2_col"]] <- ulist2
  DF2[[1,"Y"]] <- Y
  DF2[[2,"Y"]] <- Y
  res2 <- DF2 %>% 
    dplyr::mutate(
      sum = sum_byname(ulist2_col, Y)
    )
  expect_equal(res2$sum[[1]][[1]], UplusY)
  expect_equal(res2$sum[[1]][[2]], UplusY)
  expect_equal(res2$sum[[2]][[1]], UplusY)
  expect_equal(res2$sum[[2]][[2]], UplusY)
  
  # Try when the matrix length will not be a multiple of the list length.
  U3 <- matsbyname::Matrix(1:5, nrow = 5, ncol = 1, dimnames = list(c("p1", "p2", "p3", "p4", "p5"), "i1")) %>% 
    setrowtype("Products") %>% setcoltype("Industries")
  U3plusY <- sum_byname(U3, Y)
  ulist3 <- list(U3, U3, U3)
  DF3 <- data.frame(ulist3_col = I(list()), Y = I(list()))
  DF3[[1,"ulist3_col"]] <- ulist3
  DF3[[2,"ulist3_col"]] <- ulist3
  DF3[[3,"ulist3_col"]] <- ulist3
  DF3[[1,"Y"]] <- Y
  DF3[[2,"Y"]] <- Y
  DF3[[3,"Y"]] <- Y
  res3 <- DF3 %>% 
    dplyr::mutate(
      sum = sum_byname(ulist3_col, Y)
    )
  expect_equal(res3$sum[[1]][[1]], U3plusY)
  expect_equal(res3$sum[[1]][[2]], U3plusY)
  expect_equal(res3$sum[[1]][[3]], U3plusY)
  expect_equal(res3$sum[[2]][[1]], U3plusY)
  expect_equal(res3$sum[[2]][[2]], U3plusY)
  expect_equal(res3$sum[[2]][[3]], U3plusY)
  expect_equal(res3$sum[[3]][[1]], U3plusY)
  expect_equal(res3$sum[[3]][[2]], U3plusY)
  expect_equal(res3$sum[[3]][[3]], U3plusY)
})


test_that("sum_byname() works as expected via grouping and summarise", {
  df_simple <- tibble::tribble(~key, ~val, 
                               "A", 1, 
                               "A", 2, 
                               "B", 10)
  res_simple <- df_simple %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(val = sum(val))
  expected_simple <- tibble::tribble(~key, ~val, 
                                     "A", 3, 
                                     "B", 10)
  expect_equal(res_simple, expected_simple)
  
  # This works differently, and unexpectedly.
  res_simple2 <- df_simple %>% 
    dplyr::group_by(key) %>% 
    # We don't use the .summarise = TRUE condition, so
    # the resuult is just the original data frame (with groups)
    dplyr::reframe(val = sum_byname(val))
  # Here is what we expect.
  expect_equal(res_simple2, df_simple)
  
  res_simple3 <- df_simple %>% 
    dplyr::group_by(key) %>% 
    # Here, we use the .summarise = TRUE argument.
    dplyr::summarise(val = sum_byname(val, .summarise = TRUE))
  # So sum_byname() should produce the column sum along the data frame.
  # The result is a list column, because a list column
  # will also accommodate a list of matrices.
  expected_simple3 <- expected_simple
  expected_simple3$val <- list(3, 10)
  expect_equal(res_simple3, expected_simple3)
  
  m <- matrix(c(11, 12, 13,
                21, 22, 23), nrow = 2, ncol = 3, byrow = TRUE, 
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  df <- tibble::tibble(key = c("A", "A", "B"), m = list(m, m, m))
  res <- df %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(m = sum_byname(m, .summarise = TRUE))
  expected <- tibble::tibble(key = c("A", "B"), 
                             m = list(2 * m, m))
  expect_equal(res, expected)
  
  # Try summarise with 2 columns
  df2 <- df
  df2$m2 <- list(3*m, 4*m, 5*m)
  res2 <- df2 %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(m = sum_byname(m, .summarise = TRUE), m2 = sum_byname(m2, .summarise = TRUE))
  expect_equal(res2$m[[1]], 2 * m)
  expect_equal(res2$m[[2]], m)
  expect_equal(res2$m2[[1]], 7 * m)
  expect_equal(res2$m2[[2]], 5 * m)
})


test_that("sum_byname() works as expected via grouping and summarise for Matrix objects", {
  m <- matsbyname::Matrix(c(11, 12, 13,
                            21, 22, 23), nrow = 2, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  df <- tibble::tibble(key = c("A", "A", "B"), m = list(m, m, m))
  res <- df %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(m = sum_byname(m, .summarise = TRUE))
  expected <- tibble::tibble(key = c("A", "B"), 
                             m = list(2 * m, m))
  expect_equal(res, expected)
  
  # Try summarise with 2 columns
  df2 <- df
  df2$m2 <- list(3*m, 4*m, 5*m)
  res2 <- df2 %>% 
    dplyr::group_by(key) %>% 
    dplyr::summarise(m = sum_byname(m, .summarise = TRUE), m2 = sum_byname(m2, .summarise = TRUE))
  expect_equal(res2$m[[1]], 2 * m)
  expect_equal(res2$m[[2]], m)
  expect_equal(res2$m2[[1]], 7 * m)
  expect_equal(res2$m2[[2]], 5 * m)
})


test_that("sum_byname() works with sparse Matrix objects", {
  # Try with regular matrix objects
  a <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  b <- matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r3", "r4"), c("c3", "c4")))
  res1 <- sum_byname(a, b)
  expected1 <- matrix(0, nrow = 4, ncol = 4, dimnames = list(paste0("r", 1:4), paste0("c", 1:4)))
  expect_equal(res1, expected1)
  
  # Try with Matrix objects
  A <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  B <- Matrix::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r3", "r4"), c("c3", "c4")))

  res2 <- sum_byname(A, B)
  expected2 <- Matrix::Matrix(0, nrow = 4, ncol = 4, dimnames = list(paste0("r", 1:4), paste0("c", 1:4)))
  matsbyname:::expect_equal_matrix_or_Matrix(res2, expected2)
  
  D <- Matrix::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  E <- Matrix::Matrix(2, nrow = 2, ncol = 2, dimnames = list(c("r3", "r4"), c("c3", "c4")))
  res3 <- sum_byname(D, E)
  expected3 <- Matrix::Matrix(c(1, 1, 0, 0, 
                                1, 1, 0, 0,
                                0, 0, 2, 2, 
                                0, 0, 2, 2), nrow = 4, ncol = 4, byrow = TRUE,
                              dimnames = list(paste0("r", 1:4), paste0("c", 1:4)))
  matsbyname:::expect_equal_matrix_or_Matrix(res3, expected3)
  
  # Check with a matrix and a Matrix
  f <- matrix(2, nrow = 2, ncol = 2, dimnames = list(c("r3", "r4"), c("c3", "c4")))
  res4 <- sum_byname(f, D)
  expected4 <- expected3
  matsbyname:::expect_equal_matrix_or_Matrix(res4, expected4)
  
  res5 <- sum_byname(D, f)
  expected5 <- expected3
  matsbyname:::expect_equal_matrix_or_Matrix(res5, expected5)
})


test_that("sum_byname() fails with mismatch row or col types on Matrix objects", {
  A <- matsbyname::Matrix(0, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  B <- matsbyname::Matrix(1, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  
  expect_error(sum_byname(A, B), "rowtype\\(a\\) \\(rows\\) != rowtype\\(b\\) \\(Product\\)")
})
  