# This file contains tests for functions in Utilities.R.

###########################################################
context("Organizing arguments")
###########################################################

test_that("errors are generated when organize_args is called with baloney", {
  expect_error(matsbyname:::organize_args(b = 42), 
               "Missing argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = NULL, b = 42), 
               "Null argument a with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42), 
               "Missing argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = 42, b = NULL), 
               "Null argument b with no fill in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(1), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_error(matsbyname:::organize_args(a = matrix(1) %>% setcoltype("col"), 
                                          b = matrix(1) %>% setrowtype("bogus"), 
                                          match_type = "matmult"))
})


test_that("oddball match_type works as expected", {
  expect_equal(matsbyname:::organize_args(a = 1, b = 2, match_type = "none"), 
               list(a = 1, b = 2))
  expect_error(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "bogus"), 
               "Unknown match_type bogus in organize_args.")
  expect_equal(matsbyname:::organize_args(a = matrix(1), b = matrix(2), match_type = "none"),
               list(a = matrix(1), b = matrix(2)))
})


###########################################################
context("Selecting rows and columns")
###########################################################

test_that("an error is generated when no retain or remove patterns are default", {
  # Check with non-NULL values for a.
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_error(m %>% select_rows_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
  expect_error(m %>% select_cols_byname(), 
               "neither retain_pattern nor remove_pattern are different from default")
})


test_that("selecting rows and columns works even when everything is removed", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try to remove all rows
  expect_null(select_rows_byname(m, remove_pattern = "^r"))
  # Try to remove all columns
  expect_null(select_cols_byname(m, remove_pattern = "^c"))
})


test_that("selecting rows and columns works even when there is a NULL situation", {
  # Check the degenerate condition.
  expect_null(select_rows_byname(a = NULL))
  expect_null(select_cols_byname(a = NULL))
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% select_rows_byname(retain_pattern = "r1"), 
               matrix(c(1, 3), ncol = 2, dimnames = list("r1", c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_rows_byname(retain_pattern = "r3"))
  # Try with columns
  expect_equal(m %>% select_cols_byname(retain_pattern = "c1"), 
               matrix(1:2, nrow = 2, dimnames = list(c("r1", "r2"), "c1")) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  # Should work when there is nothing to select.
  expect_null(m %>% select_cols_byname(retain_pattern = "c3"))
})

test_that("setting row and column names works even when there is a NULL situation", {
  m <- matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Try with rows
  expect_equal(m %>% setrownames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("a", "b"), c("c1", "c2"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setrownames_byname(NULL, c("a", "b")))
  # Try with columns
  expect_equal(m %>% setcolnames_byname(c("a", "b")), 
               matrix(1:4, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("a", "b"))) %>% 
                 setrowtype("rows") %>% setcoltype("cols"))
  expect_null(setcolnames_byname(NULL, c("a", "b")))
})
  

###########################################################
context("Cleaning")
###########################################################

test_that("bad margins in clean_byname work as expected", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_error(clean_byname(m, margin = 42), 
               "margin = 42 in clean_byname\\(\\). Must be 1 or 2.")
  
})


test_that("cleaning both rows and cols works as expected", {
  m <- matrix(c(0, 0, 0, 1, 2, 3), nrow = 3, ncol = 2, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  expect_equal(clean_byname(m), 
               matrix(1:3, nrow = 3, ncol = 1, dimnames = list(c("r1", "r2", "r3"), "c2")))
})


test_that("cleaning a vector works as expected", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4"), c("c1")))
  expect_equal(clean_byname(v), 
               matrix(42, dimnames = list(c("r4"), c("c1"))))
})


test_that("cleaning works with unnamed rows and/or columns", {
  v <- matrix(c(0, 
                0, 
                0, 
                42), nrow = 4, dimnames = list(c("r1", "r2", "r3", "r4")))
  expect_equal(clean_byname(v), 
               matrix(42, nrow = 1, byrow = TRUE, dimnames = list(c("r4"), c(NULL))))
  
  
  unnamed <- matrix(c(1, 2, 0,
                      3, 4, 0,
                      5, 6, 0,
                      0, 0, 0), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed), matrix(c(1, 2,
                                               3, 4, 
                                               5, 6), nrow = 3, byrow = TRUE))
})


test_that("cleaning works with tolerance", {
  unnamed <- matrix(c(1, 2, 0.1,
                      3, 4, -0.1,
                      5, 6, 0.05,
                      0.01, -0.01, -0.05), nrow = 4, byrow = TRUE)
  expect_equal(clean_byname(unnamed, tol = 0.1), matrix(c(1, 2,
                                                          3, 4, 
                                                          5, 6), nrow = 3, byrow = TRUE))
  # Tighten tolerance to get different result.
  expect_equal(clean_byname(unnamed, tol = 0.0999), matrix(c(1, 2, 0.1, 
                                                             3, 4, -0.1,
                                                             5, 6, 0.05), nrow = 3, byrow = TRUE))
})


###########################################################
context("Is zero")
###########################################################

test_that("iszero_byname works as expected", {
  m <- matrix(0, nrow = 3, ncol = 2)
  expect_true(iszero_byname(m))
  n <- matrix(1, nrow = 42, ncol = 5)
  expect_false(iszero_byname(n))
})



###########################################################
context("Row and column naming")
###########################################################

test_that("getting row names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getrownames_byname(m), c("i1", "i2"))
  # This also works for lists
  expect_equal(getrownames_byname(list(m,m)), list(c("i1", "i2"), c("i1", "i2")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getrownames_byname(DF$m), list(c("i1", "i2"), c("i1", "i2")))
})


test_that("getting column names works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Products")
  expect_equal(getcolnames_byname(m), c("p1", "p2", "p3"))
  # This also works for lists
  expect_equal(getcolnames_byname(list(m,m)), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
  # Also works for data frames
  DF <- data.frame(m = I(list()))
  DF[[1,"m"]] <- m
  DF[[2,"m"]] <- m
  expect_equal(getcolnames_byname(DF$m), list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
})


test_that("setrownames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m, c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("a", "b")) %>% rownames(), 
               c("a", "b"))
  expect_equal(m %>% setrownames_byname(rownames(m2)) %>% rownames(), c("a", "b"))
  expect_equal(m %>% setrownames_byname(c("c", "d")) %>% rownames(), c("c", "d"))
  expect_null(m %>% setrownames_byname(NULL) %>% rownames())
  expect_equal(m %>% setrownames_byname(c(NA, NA)) %>% rownames(), c(NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the row name
  expect_equal(2 %>% setrownames_byname("row"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(c("row"), NULL)))
})


test_that("setcolnames_byname works as expected", {
  m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  expect_equal(m %>% setcolnames_byname(c("a", "b", "c")) %>% colnames(), 
               c("a", "b", "c"))
  expect_equal(m %>% setcolnames_byname(c("d", "e", "f")) %>% colnames(), c("d", "e", "f"))
  expect_null(m %>% setcolnames_byname(NULL) %>% colnames())
  expect_equal(m %>% setcolnames_byname(c(NA, NA, NA)) %>% colnames(), c(NA_character_, NA_character_, NA_character_))
  # The function should convert the constant to a matrix and apply the col name
  expect_equal(2 %>% setcolnames_byname("col"), 
               matrix(2, nrow = 1, ncol = 1, dimnames = list(NULL, c("col"))))
})


test_that("setting row names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setrownames_byname(m1, c("a", "b"))
  expect_equal(rownames(m2), c("a", "b"))
  m3 <- setrownames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("c", "d"))
  expect_equal(rownames(m3), c("c", "d"))
  m4 <- m1 %>% setrownames_byname(NULL)
  expect_null(rownames(m4))
  m5 <- m1 %>% setrownames_byname(c(NA, NA))
  expect_equal(rownames(m5), c(NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setrownames_byname(l1, rownames = list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without using a named argument (rownames) to see if they are inferred
  l2 <- setrownames_byname(l1, list(c("a", "b")))
  expect_equal(list(rownames(l2[[1]]), rownames(l2[[2]])), list(c("a", "b"), c("a", "b")))
  # Try without a list. This should fail, because a is applied to the first matrix and b is applied to the second matrix.
  # But each matrix needs 2 rownames.
  expect_error(setrownames_byname(l1, c("a", "b")), 
               "length of 'dimnames' \\[1\\] not equal to array extent")
  
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r1", "r2")))
    )
  expect_equal(rownames(DF2$mcol2[[1]]), c("r1", "r2"))
  expect_equal(rownames(DF2$mcol2[[2]]), c("r1", "r2"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setrownames_byname(mcol, list(c("r3", "r4")))
    )
  expect_equal(list(rownames(DF3$mcol2[[1]]), rownames(DF3$mcol2[[2]])), list(c("r3", "r4"), c("r3", "r4")))
})


test_that("setting row names works with different names for each matrix", {
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_rownames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setrownames_byname(mlist, rownames = new_rownames)
  expect_equal(getrownames_byname(renamed), new_rownames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), rownames_col = I(new_rownames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setrownames_byname(mcol, rownames = rownames_col)
    )
  expect_equal(getrownames_byname(DF_renamed$mcol_2), new_rownames)
})


test_that("setting col names works as expected", {
  m1 <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("p", 1:3))) %>%
    setrowtype("Industries") %>% setcoltype("Commodities")
  m2 <- setcolnames_byname(m1, c("a", "b", "c"))
  expect_equal(colnames(m2), c("a", "b", "c"))
  m3 <- setcolnames_byname(m1 %>% setrowtype("Industries") %>% setcoltype("Commodities"), c("d", "e", "f"))
  expect_equal(colnames(m3), c("d", "e", "f"))
  m4 <- m1 %>% setcolnames_byname(NULL)
  expect_null(colnames(m4))
  m5 <- m1 %>% setcolnames_byname(c(NA, NA, NA))
  expect_equal(colnames(m5), c(NA_character_, NA_character_, NA_character_))
  # This also works for lists
  l1 <- list(m1,m1)
  l2 <- setcolnames_byname(l1, c("a", "b", "c"))
  expect_equal(list(colnames(l2[[1]]), colnames(l2[[2]])), list(c("a", "b", "c"), c("a", "b", "c")))
  # This also works with data frames
  DF1 <- data.frame(mcol = I(list()))
  DF1[[1,"mcol"]] <- m1
  DF1[[2,"mcol"]] <- m1
  DF2 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(colnames(DF2$mcol2[[1]]), c("c1", "c2", "c3"))
  expect_equal(colnames(DF2$mcol2[[2]]), c("c1", "c2", "c3"))
  DF3 <- DF1 %>% 
    dplyr::mutate(
      mcol2 = setcolnames_byname(mcol, c("c1", "c2", "c3"))
    )
  expect_equal(list(colnames(DF3$mcol2[[1]]), colnames(DF3$mcol2[[2]])), list(c("c1", "c2", "c3"), c("c1", "c2", "c3")))
})


test_that("setting column names works with different names for each matrix", {
  m <- matrix(c(1, 2,
                3, 4, 
                5, 6), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "r2", "r3"), c("c1", "c2")))
  mlist <- list(m, m, m)
  new_colnames <- list(c("a", "b"), c("c", "d"), c("e", "f"))
  renamed <- setcolnames_byname(mlist, colnames = new_colnames)
  expect_equal(getcolnames_byname(renamed), new_colnames)
  
  # Try this in a data frame
  DF <- data.frame(mcol = I(mlist), colnames_col = I(new_colnames))
  DF_renamed <- DF %>% 
    dplyr::mutate(
      mcol_2 = setcolnames_byname(mcol, colnames = colnames_col)
    )
  expect_equal(getcolnames_byname(DF_renamed$mcol_2), new_colnames)
})


test_that("renaming rows to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("c1", "c2")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  actual <- rename_to_pref_suff_byname(m, keep = "prefix", margin = 1, notation = arrow_notation())
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "r2", "r3")
  actual <- rename_to_pref_suff_byname(m, keep = "suffix", margin = 1, notation = arrow_notation())
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suffix", margin = 1, notation = arrow_notation())
  expect_equal(actual, list(expected, expected))
})


test_that("renaming columns to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  colnames(expected) <- c("a", "c")
  actual <- rename_to_pref_suff_byname(m, keep = "prefix", margin = 2, notation = arrow_notation())
  expect_equal(actual, expected)
  
  expected <- m
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suffix", margin = 2, notation = arrow_notation())
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), keep = "suffix", margin = 2, notation = arrow_notation())
  expect_equal(actual, list(expected, expected))
  
  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows") %>% setcoltype("Cols")
  res <- rename_to_pref_suff_byname(m, keep = "suffix", notation = arrow_notation())
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Cols")
})


test_that("renaming rows and columns to prefix or suffix works as expected", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "r2", "r3"), c("a -> b", "c -> d")))
  expected <- m
  rownames(expected) <- c("a", "r2", "r3")
  colnames(expected) <- c("a", "c")
  # Default is margin = c(1, 2)
  actual <- rename_to_pref_suff_byname(m, keep = "prefix", notation = arrow_notation())
  expect_equal(actual, expected)
  
  expected <- m
  rownames(expected) <- c("b", "r2", "r3")
  colnames(expected) <- c("b", "d")
  actual <- rename_to_pref_suff_byname(m, keep = "suffix", notation = arrow_notation())
  expect_equal(actual, expected)
  
  # Check that renaming works for a list
  actual <- rename_to_pref_suff_byname(list(m, m), margin = list(c(1, 2)), keep = "suffix", notation = arrow_notation())
  expect_equal(actual, list(expected, expected))

  # Check that row and column types are preserved
  m <- m %>% setrowtype("Rows") %>% setcoltype("Cols")
  res <- rename_to_pref_suff_byname(m, keep = "prefix", notation = arrow_notation())
  expect_equal(rowtype(res), "Rows")
  expect_equal(coltype(res), "Cols")
})


test_that("renaming rows and cols to pref and suff also changes rowtype and coltype", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "c -> d", "e -> f"), c("g -> h", "i -> j"))) %>% 
    setrowtype("Industry -> Product") %>% setcoltype("Product -> Industry")
  res <- rename_to_pref_suff_byname(m, keep = "prefix", notation = arrow_notation())
  expect_equal(rownames(res), c("a", "c", "e"))
  expect_equal(colnames(res), c("g", "i"))
  expect_equal(rowtype(res), "Industry")
  expect_equal(coltype(res), "Product")

  res2 <- rename_to_pref_suff_byname(m, keep = "suffix", notation = arrow_notation())
  expect_equal(rownames(res2), c("b", "d", "f"))
  expect_equal(colnames(res2), c("h", "j"))
  expect_equal(rowtype(res2), "Product")
  expect_equal(coltype(res2), "Industry")
})


test_that("setting identical row/col names is OK", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a -> b", "a -> c", "r3"), c("a -> b", "a -> z")))
  expected <- m
  rownames(expected) <- c("a", "a", "r3")
  colnames(expected) <- c("a", "a")
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "prefix", notation = arrow_notation())
  # Interestingly the command View(actual) or View(expected)
  # shows row names that aren't true. 
  # The 2nd row of actual and expected is shown as "a.1", 
  # but the underlying objects have simply "a".
  # (The column names are shown correctly.)
  # This test ensures that R isn't messing with the actual row and column names on the objects.
  expect_equal(actual, expected)
})


test_that("renaming with full prefix identifiers works as expected.", {
  m <- matrix(c(1, 2, 
                3, 4, 
                5, 6), nrow = 3, byrow = TRUE, 
              dimnames = list(c("a [b]", "c [d]", "e [f]"), c("g [h]", "i [j]")))
  expected <- m
  dimnames(expected) <- list(c("a", "c", "e"), c("g", "i"))
  # The next call will create duplicate row names and column names in m. 
  actual <- rename_to_pref_suff_byname(m, keep = "prefix", notation = bracket_notation())
  expect_equal(actual, expected)
  
  expected2 <- m
  dimnames(expected2) <- list(c("a", "c", "e"), c("g [h]", "i [j]"))
  actual2 <- rename_to_pref_suff_byname(m, keep = "prefix", margin = 1, notation = bracket_notation())
  expect_equal(actual2, expected2)
  
  expected3 <- m
  dimnames(expected3) <- list(c("a [b]", "c [d]", "e [f]"), c("g", "i"))
  actual3 <- rename_to_pref_suff_byname(m, keep = "prefix", margin = 2, notation = bracket_notation())
  expect_equal(actual3, expected3)
  
  expected4 <- m
  dimnames(expected4) <- list(c("b", "d", "f"), c("h", "j"))
  actual4 <- rename_to_pref_suff_byname(m, keep = "suffix", notation = bracket_notation())
  expect_equal(actual4, expected4)

  expected5 <- m
  dimnames(expected5) <- list(c("b", "d", "f"), c("g [h]", "i [j]"))
  actual5 <- rename_to_pref_suff_byname(m, keep = "suffix", margin = 1, notation = bracket_notation())
  expect_equal(expected5, actual5)

  expected6 <- m
  dimnames(expected6) <- list(c("a [b]", "c [d]", "e [f]"), c("h", "j"))
  actual6 <- rename_to_pref_suff_byname(m, keep = "suffix", margin = 2, notation = bracket_notation())
  expect_equal(expected6, actual6)
  
  # Try with a list
  actual_list <- rename_to_pref_suff_byname(list(m, m), keep = "prefix", margin = 1, notation = bracket_notation())
  expect_equal(actual_list[[1]], expected2)
  expect_equal(actual_list[[2]], expected2)
  
  # Try in a data frame
  DF <- tibble::tibble(m = list(m, m, m, m, m, m), keep = c("prefix", "prefix", "prefix", "suffix", "suffix", "suffix"), 
                       margin = list(c(1, 2), 1, 2, c(1, 2), 1, 2),
                       notation = list(bracket_notation()),
                       expected = list(expected, expected2, expected3, expected4, expected5, expected6))
  
  res <- DF %>% 
    dplyr::mutate(
      actual = rename_to_pref_suff_byname(m, keep = keep, margin = margin, 
                                          notation = notation)
    )
  expect_equal(res$actual, res$expected)
})


###########################################################
context("Row and column types")
###########################################################

test_that("setrowtype and rowtype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products")
  expect_null(U %>% setrowtype(NULL) %>% rowtype())
  expect_equal(rowtype(U), "Products")
  # This also works for lists
  Ul <- setrowtype(list(U,U), rowtype = "Products")
  expect_equal(rowtype(Ul), list("Products", "Products"))
  Ul2 <- setrowtype(list(U,U), rowtype = "Junk")
  expect_equal(rowtype(Ul2), list("Junk", "Junk"))
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setrowtype(DF$U, "Products")
  expect_equal(rowtype(DF2), list("Products", "Products"))
  DF3 <- DF %>% dplyr::mutate(newcol = setrowtype(U, "Products"))
  expect_equal(DF3$newcol %>% rowtype, list("Products", "Products"))
})

test_that("setcoltype and coltype works as expected", {
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setcoltype("Industries")
  expect_null(U %>% setcoltype(NULL) %>% coltype())
  expect_equal(coltype(U), "Industries")
  # This also works for lists
  Ul <- setcoltype(list(U,U), coltype = "Industries")
  expect_equal(coltype(Ul), list("Industries", "Industries"))
  Ul2 <- setcoltype(list(U,U), coltype = "Junk")
  expect_equal(coltype(Ul2), list("Junk", "Junk"))
  # Check that it works when the lists are not same structure as a.
  Ul3 <- setcoltype(list(U,U), coltype = list("Junk", "Junk"))
  expect_equal(coltype(Ul3), list("Junk", "Junk"))
  Ul4 <- setcoltype(list(U,U,U), coltype = list("Junk", "Junk", "Bogus"))
  expect_equal(coltype(Ul4), list("Junk", "Junk", "Bogus"))
  Ul5 <- setcoltype(list(U,U,U), coltype = c("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  Ul6 <- setcoltype(list(U,U,U), coltype = list("Bogus"))
  expect_equal(coltype(Ul5), list("Bogus", "Bogus", "Bogus"))
  # This one should fail, becuase length of coltype is neither 1 nor length(a), namely 3.
  expect_error(setcoltype(list(U,U,U), coltype = list("Bogus", "Bogus")), 
               "In prepare_.FUNdots\\(\\), when both 'a' and '.FUNdots' are lists, each top-level argument in .FUNdots must have length = 1 or length = length\\(a\\) \\(= 3\\). Found length = 2 for argument 'coltype', which is a list.")
  
  # Also works for data frames
  DF <- data.frame(U = I(list()))
  DF[[1,"U"]] <- U
  DF[[2,"U"]] <- U
  DF2 <- setcoltype(DF$U, "Industries")
  expect_equal(coltype(DF2), list("Industries", "Industries"))
  DF3 <- DF %>% dplyr::mutate(newcol = setcoltype(U, "Industries"))
  expect_equal(DF3$newcol %>% coltype, list("Industries", "Industries"))
})

