test_that("agg_table_to_agg_map() works as expected", {
  bands <- tibble::tribble(~few_col, ~many_col, 
                           "The Beatles", "John", 
                           "The Beatles", "Paul", 
                           "The Beatles", "George", 
                           "The Beatles", "Ringo", 
                           # Test duplicates and NA
                           "The Beatles", "Ringo",
                           "The Beatles", NA, 
                           "Rolling Stones", "Mick", 
                           "Rolling Stones", "Keith",
                           "Rolling Stones", "Ronnie",
                           "Rolling Stones", "Bill",
                           "Rolling Stones", "Charlie")
  res <- agg_table_to_agg_map(bands, 
                              few_colname = "few_col",
                              many_colname = "many_col")
  expect_equal(res, list(`Rolling Stones` = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"),
                         `The Beatles` = c("John", "Paul", "George", "Ringo")))
})


test_that("agg_map_to_agg_table() works as expected", {
  agg_map <- list(`The Beatles` = c("John", "Paul", "George", "Ringo"), 
                  `Rolling Stones` = c("Mick", "Keith", "Ronnie", "Bill", "Charlie"))
  res <- agg_map_to_agg_table(agg_map, few_colname = "band", many_colname = "member")
  expect_equal(res, 
               tibble::tribble(~band, ~member, 
                               "The Beatles", "John", 
                               "The Beatles", "Paul", 
                               "The Beatles", "George", 
                               "The Beatles", "Ringo", 
                               "Rolling Stones", "Mick", 
                               "Rolling Stones", "Keith", 
                               "Rolling Stones", "Ronnie", 
                               "Rolling Stones", "Bill", 
                               "Rolling Stones", "Charlie"))
})


test_that("aggregate_byname() works as expected", {
  m <- matrix(1:9, nrow = 3, byrow = TRUE,
              dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  expected <- matrix(c(5, 7, 9,
                       7, 8, 9), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r3"), c("c1", "c2", "c3")))
  actual <- aggregate_byname(m, aggregation_map = list(a = c("r1", "r2")))
  expect_equal(actual, expected)
  
  # Try with wrong margin.
  # This will try to aggregate r1 and r2 in columns, but there are no r1 or r2 columns.
  expect_equal(aggregate_byname(m, aggregation_map = list(a = c("r1", "r2")), margin = 2), m)
  
  # Try to aggregate with only 1 row.
  # Should get same thing with a renamed column
  expected <- m
  dimnames(expected) <- list(c("r1", "a", "r3"), c("c1", "c2", "c3"))
  expect_equal(aggregate_byname(m, aggregation_map = list(a = c("r2")), margin = 1) %>% sort_rows_cols(margin = 1, roworder = dimnames(expected)[[1]]), expected)
  
  # Aggregate with a map that contains rows that don't exist.
  expect_equal(aggregate_byname(m, aggregation_map = list(a = c("r4", "r5", "42", "supercalifragilisticexpialidocious")), margin = 1), m)
})


test_that("aggregate_byname() works with Matrix objects", {
  M <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE,
                          dimnames = list(c("r1", "r2", "r3"), c("c1", "c2", "c3")))
  expected <- matrix(c(5, 7, 9,
                       7, 8, 9), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r3"), c("c1", "c2", "c3")))
  actual <- aggregate_byname(M, aggregation_map = list(a = c("r1", "r2")))
  matsbyname:::expect_equal_matrix_or_Matrix(actual, expected)
  
  # Try with wrong margin.
  # This will try to aggregate r1 and r2 in columns, but there are no r1 or r2 columns.
  expect_equal(aggregate_byname(M, aggregation_map = list(a = c("r1", "r2")), margin = 2), M)
  
  # Try to aggregate with only 1 row.
  # Should get same thing with a renamed column
  expected <- M
  dimnames(expected) <- list(c("r1", "a", "r3"), c("c1", "c2", "c3"))
  matsbyname:::expect_equal_matrix_or_Matrix(aggregate_byname(M, aggregation_map = list(a = c("r2")), margin = 1) %>%
                                               sort_rows_cols(margin = 1, roworder = dimnames(expected)[[1]]), 
                                             expected)
  
  # Aggregate with a map that contains rows that don't exist.
  expect_equal(aggregate_byname(M, aggregation_map = list(a = c("r4", "r5", "42", "supercalifragilisticexpialidocious")), margin = 1), M)
})


test_that("aggregate_byname() works as expected for NULL aggregation_map", {
  m <- matrix(1:9, nrow = 3, byrow = TRUE,
              dimnames = list(c("r1", "a", "a"), c("c1", "c2", "c3")))
  expected <- matrix(c(11, 13, 15,
                       1, 2, 3), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r1"), c("c1", "c2", "c3")))
  # Nothing should change, because we're asking for aggregation by columns which have no repeated names.
  expect_equal(aggregate_byname(m, margin = 2), m)
  # Now we should get the expected result
  expect_equal(aggregate_byname(m, margin = 1), expected)
  # And, again should get the expected result, because we're asking for margin = c(1, 2), the default
  expect_equal(aggregate_byname(m), expected)
  
  m1 <- matrix(42, nrow = 1, dimnames = list(c("r1"), c("c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e1 <- m1
  expect_equal(aggregate_byname(m1), e1)
  expect_equal(aggregate_byname(list(m1)), list(e1))
  
  # Now aggregate on both rows and columns when some names are duplicated in both rows and cols.
  # First, try to aggregate on rows.
  m2 <- matrix(1:9, nrow = 3, byrow = TRUE,
               dimnames = list(c("r1", "a", "a"), c("b", "b", "c3")))
  expected2 <- matrix(c(11, 13, 15,
                        1, 2, 3), nrow = 2, byrow = TRUE, dimnames = list(c("a", "r1"), c("b", "b", "c3")))
  expect_equal(aggregate_byname(m2, margin = 1), expected2)
  
  # Now try to aggregate on columns
  expected3 <- matrix(c(3, 3,
                        9, 6,
                        15, 9), nrow = 3, byrow = TRUE, dimnames = list(c("r1", "a", "a"), c("b", "c3")))
  expect_equal(aggregate_byname(m2, margin = 2), expected3)
  
  # Now try to aggregate both rows and columns.
  expected4 <- matrix(c(24, 15,
                        3, 3), nrow = 2, byrow = TRUE,
                      dimnames = list(c("a", "r1"), c("b", "c3")))
  expect_equal(aggregate_byname(m2), expected4)
})


test_that("aggregate_byname() works with Matrix objects for NULL aggregation_map", {
  M <- matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE,
              dimnames = list(c("r1", "a", "a"), c("c1", "c2", "c3")))
  expected <- matrix(c(11, 13, 15,
                       1, 2, 3), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r1"), c("c1", "c2", "c3")))
  # Nothing should change, because we're asking for aggregation by columns which have no repeated names.
  expect_equal(aggregate_byname(M, margin = 2), M)
  # Now we should get the expected result
  expect_equal(aggregate_byname(M, margin = 1), expected)
  # And, again should get the expected result, because we're asking for margin = c(1, 2), the default
  expect_equal(aggregate_byname(M), expected)
  
  M1 <- matsbyname::Matrix(42, nrow = 1, dimnames = list(c("r1"), c("c1")), 
                           rowtype = "rows", coltype = "cols")
  E1 <- M1
  expect_equal(aggregate_byname(M1), E1)
  expect_equal(aggregate_byname(list(M1)), list(E1))
  
  # Now aggregate on both rows and columns when some names are duplicated in both rows and cols.
  # First, try to aggregate on rows.
  M2 <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE,
                           dimnames = list(c("r1", "a", "a"), c("b", "b", "c3")))
  expected2 <- matsbyname::Matrix(c(11, 13, 15,
                                    1, 2, 3), nrow = 2, ncol = 3, byrow = TRUE, dimnames = list(c("a", "r1"), c("b", "b", "c3")))
  matsbyname:::expect_equal_matrix_or_Matrix(aggregate_byname(M2, margin = 1), expected2)
  
  # Now try to aggregate on columns
  expected3 <- matsbyname::Matrix(c(3, 3,
                                    9, 6,
                                    15, 9), nrow = 3, ncol = 2, byrow = TRUE, dimnames = list(c("r1", "a", "a"), c("b", "c3")))
  matsbyname:::expect_equal_matrix_or_Matrix(aggregate_byname(M2, margin = 2), expected3)
  
  # Now try to aggregate both rows and columns.
  expected4 <- matsbyname::Matrix(c(24, 15,
                                    3, 3), nrow = 2, ncol = 2, byrow = TRUE,
                                  dimnames = list(c("a", "r1"), c("b", "c3")))
  matsbyname:::expect_equal_matrix_or_Matrix(aggregate_byname(M2), expected4)
})


test_that("aggregate_byname() works as expected for lists", {
  m <- matrix(1:9, nrow = 3, byrow = TRUE,
              dimnames = list(c("r1", "a", "a"), c("c1", "c2", "c3")))
  expected <- matrix(c(11, 13, 15,
                       1, 2, 3), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r1"), c("c1", "c2", "c3")))
  expect_equal(aggregate_byname(list(m, m, m)), list(expected, expected, expected))
  
  expect_equal(aggregate_byname(list(m, m), margin = list(c(1, 2))), list(expected, expected))
  
  # Also check that row and column type are preserved
  m2 <- matrix(1:9, nrow = 3, byrow = TRUE, 
               dimnames = list(c("b", "a", "a"), c("e", "d", "d"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expected2 <- matrix(c(28, 11,
                        5, 1), nrow = 2, byrow = TRUE, 
                      dimnames = list(c("a", "b"), c("d", "e"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  expect_equal(aggregate_byname(m2), expected2)
  expect_equal(aggregate_byname(list(m2)), list(expected2))
  expect_equal(aggregate_byname(list(m2, m2), margin = list(c(1, 2))), list(expected2, expected2))
  expect_equal(aggregate_byname(list(m2, m2, m2, m2)), list(expected2, expected2, expected2, expected2))
})


test_that("aggregate_byname() works with Matrix objects for lists", {
  M <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE,
                          dimnames = list(c("r1", "a", "a"), c("c1", "c2", "c3")))
  expected <- matrix(c(11, 13, 15,
                       1, 2, 3), nrow = 2, byrow = TRUE,
                     dimnames = list(c("a", "r1"), c("c1", "c2", "c3")))
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, aggregate_byname(list(M, M, M)), list(expected, expected, expected))
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, aggregate_byname(list(M, M), margin = list(c(1, 2))), list(expected, expected))
  
  # Also check that row and column type are preserved
  m2 <- matsbyname::Matrix(1:9, nrow = 3, ncol = 3, byrow = TRUE, 
                           dimnames = list(c("b", "a", "a"), c("e", "d", "d")), 
                           rowtype = "rows", coltype = "cols")
  expected2 <- matrix(c(28, 11,
                        5, 1), nrow = 2, byrow = TRUE, 
                      dimnames = list(c("a", "b"), c("d", "e"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  res <- aggregate_byname(m2)
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, expected2)
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, aggregate_byname(list(m2)), list(expected2))
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, aggregate_byname(list(m2, m2), margin = list(c(1, 2))), list(expected2, expected2))
  Map(f = matsbyname:::expect_equal_matrix_or_Matrix, aggregate_byname(list(m2, m2, m2, m2), margin = list(c(1, 2))), list(expected2, expected2, expected2, expected2))
})


test_that("aggregate_byname() works when all rows collapse", {
  m <- matrix(1:6, byrow = TRUE, nrow = 2, ncol = 3, 
              dimnames = list(c("a", "a"), c("c1", "c2", "c3")))
  e <- matrix(c(5, 7, 9), byrow = TRUE, nrow = 1, ncol = 3,
              dimnames = list(c("a"), c("c1", "c2", "c3")))
  expect_equal(aggregate_byname(m), e)
})


test_that("aggregate_byname() works when all rows collapse in a Matrix", {
  m <- matsbyname::Matrix(1:6, byrow = TRUE, nrow = 2, ncol = 3,
                          dimnames = list(c("a", "a"), c("c1", "c2", "c3")))
  e <- matsbyname::Matrix(c(5, 7, 9), byrow = TRUE, nrow = 1, ncol = 3,
                          dimnames = list(c("a"), c("c1", "c2", "c3")))
  expect_equal(aggregate_byname(m), e)
})


test_that("aggregate_byname() works when aggregating all rows with an aggregation map", {
  m3 <- matrix(1:9, byrow = TRUE, nrow = 3, 
               dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e3 <- matrix(c(12, 15, 18), byrow = TRUE, nrow = 1, 
               dimnames = list(c("new_row"), c("c2", "c1", "c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  # Aggregate all rows
  am <- list(new_row = c("r1", "r2"))
  a3 <- aggregate_byname(m3, aggregation_map = am, margin = 1)
  expect_equal(aggregate_byname(m3, aggregation_map = am, margin = 1), e3)
})


test_that("aggregate_byname() works when aggregating all rows with an aggregation map for a Matrix", {
  m3 <- matsbyname::Matrix(1:9, byrow = TRUE, nrow = 3, ncol = 3,
                           dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1")), 
                           rowtype = "rows", coltype = "cols")
  e3 <- matsbyname::Matrix(c(12, 15, 18), byrow = TRUE, nrow = 1, ncol = 3,
                           dimnames = list(c("new_row"), c("c2", "c1", "c1")), 
                           rowtype = "rows", coltype = "cols")
  # Aggregate all rows
  am <- list(new_row = c("r1", "r2"))
  a3 <- aggregate_byname(m3, aggregation_map = am, margin = 1)
  expect_equal(aggregate_byname(m3, aggregation_map = am, margin = 1), e3)
})


test_that("aggregate_byname() works as expected in data frames", {
  m1 <- matrix(42, nrow = 1, dimnames = list(c("r1"), c("c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e1 <- m1
  m2 <- matrix(1:4, byrow = TRUE, nrow = 2, 
               dimnames = list(c("a", "a"), c("a", "a")))
  e2row <- matrix(c(4, 6), byrow = TRUE, nrow = 1, 
                  dimnames = list(c("a"), c("a", "a")))
  e2col <- matrix(c(3, 7), nrow = 2, dimnames = list(c("a", "a"), c("a")))
  e2both <- matrix(10, nrow = 1,
                   dimnames = list(c("a"), c("a")))
  m3 <- matrix(1:9, byrow = TRUE, nrow = 3, 
               dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e3row <- matrix(c(11, 13, 15, 
                    1, 2, 3), byrow = TRUE, nrow = 2,
                  dimnames = list(c("r1", "r2"), c("c2", "c1", "c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e3col <- matrix(c(5, 1, 
                    11, 4, 
                    17, 7), byrow = TRUE, nrow = 3, 
                  dimnames = list(c("r2", "r1", "r1"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e3both <- matrix(c(28, 11, 
                     5, 1), byrow = TRUE, nrow = 2, 
                   dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(aggregate_byname(m2, margin = 1), e2row)
  expect_equal(aggregate_byname(m3, margin = 1), e3row)
  expect_equal(aggregate_byname(m3, margin = c(1, 2)), e3both)
  
  DF <- tibble::tibble(m = list(m1, m1, m1, m2, m2, m2, m3, m3, m3), 
                       margin = list(1, 2, c(1,2), 1, 2, c(1, 2), 1, 2, c(1, 2)), 
                       expected = list(e1, e1, e1, e2row, e2col, e2both, e3row, e3col, e3both))
  
  expect_equal(aggregate_byname(DF$m, margin = DF$margin), DF$expected)
  
  res <- DF %>% 
    dplyr::mutate(
      actual = aggregate_byname(m, margin = margin), 
      equal = all.equal(actual, expected)
    )
  expect_true(all(res$equal))
  
  # Now add an aggregation map
  am <- list(new_row = c("r1", "r2"))
  e4row <- matrix(c(12, 15, 18), byrow = TRUE, nrow = 1, 
                  dimnames = list(c("new_row"), c("c2", "c1", "c1"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  e4col <- m3
  e4both <- matrix(c(28, 11, 
                     5, 1), byrow = TRUE, nrow = 2, dimnames = list(c("r1", "r2"), c("c1", "c2"))) %>% 
    setrowtype("rows") %>% setcoltype("cols")
  
  expect_equal(aggregate_byname(m3, aggregation_map = am, margin = 1), e4row)
  # The next call should fail, because we're 
  # trying to aggregate on columns, but
  # we're using an aggregation_map designed for rows.
  # The aggregation fails to produce any changes in the data frame.
  # When the aggregate_byname function tries to sort the columns, 
  # it encounters duplicated row names and fails.
  expect_error(aggregate_byname(m3, aggregation_map = am, margin = 2), "Row names not unique. Duplicated row names are: c1")
  # The next call should fail, because we're trying to aggregate on both rows and columns (margin = c(1, 2)), but
  # the aggregation_map only aggregates by rows.
  # When we try to sum across both margins, 
  # there is a duplicate name ("c1"), which causes a problem.
  expect_error(aggregate_byname(m3, aggregation_map = am, margin = c(1, 2)), "Row names not unique. Duplicated row names are: c1")
  
  # The next call should work.
  expect_equal(aggregate_byname(m3), e4both)
  
  DF2 <- tibble::tibble(
    m = list(m3, m3, m3), 
    margin = list(1, 2, c(1, 2)), 
    expected = list(e4row, e3col, e4both), 
    am = list(am, NULL, NULL)
  )
  res2 <- DF2 %>% 
    dplyr::mutate(
      actual = aggregate_byname(m, margin = margin, aggregation_map = am), 
      equal = all.equal(actual, expected)
    )
  expect_true(all(res2$equal))
})


test_that("aggregate_byname() works as expected in data frames for Matrix objects", {
  m1 <- matsbyname::Matrix(42, nrow = 1, ncol = 1, dimnames = list(c("r1"), c("c1")), 
                           rowtype = "rows", coltype = "cols")
  e1 <- m1
  m2 <- matsbyname::Matrix(1:4, byrow = TRUE, nrow = 2, ncol = 2,
                           dimnames = list(c("a", "a"), c("a", "a")))
  e2row <- matsbyname::Matrix(c(4, 6), byrow = TRUE, nrow = 1, ncol = 2,
                              dimnames = list(c("a"), c("a", "a")))
  e2col <- matsbyname::Matrix(c(3, 7), nrow = 2, ncol = 1, dimnames = list(c("a", "a"), c("a")))
  e2both <- matsbyname::Matrix(10, nrow = 1,
                               dimnames = list(c("a"), c("a")))
  m3 <- matsbyname::Matrix(1:9, byrow = TRUE, nrow = 3, ncol = 3,
                           dimnames = list(c("r2", "r1", "r1"), c("c2", "c1", "c1")), 
                           rowtype = "rows", coltype = "cols")
  e3row <- matsbyname::Matrix(c(11, 13, 15, 
                                1, 2, 3), byrow = TRUE, nrow = 2, ncol = 3,
                              dimnames = list(c("r1", "r2"), c("c2", "c1", "c1")), 
                              rowtype = "rows", coltype = "cols")
  e3col <- matsbyname::Matrix(c(5, 1, 
                                11, 4, 
                                17, 7), byrow = TRUE, nrow = 3, ncol = 2,
                              dimnames = list(c("r2", "r1", "r1"), c("c1", "c2")), 
                              rowtype = "rows", coltype = "cols")
  e3both <- matsbyname::Matrix(c(28, 11, 
                                 5, 1), byrow = TRUE, nrow = 2, ncol = 2,
                               dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                               rowtype = "rows", coltype = "cols")
  
  expect_equal(aggregate_byname(m2, margin = 1), e2row)
  expect_equal(aggregate_byname(m3, margin = 1), e3row)
  expect_equal(aggregate_byname(m3, margin = c(1, 2)), e3both)
  
  DF <- tibble::tibble(m = list(m1, m1, m1, m2, m2, m2, m3, m3, m3), 
                       margin = list(1, 2, c(1,2), 1, 2, c(1, 2), 1, 2, c(1, 2)), 
                       expected = list(e1, e1, e1, e2row, e2col, e2both, e3row, e3col, e3both))
  
  expect_equal(aggregate_byname(DF$m, margin = DF$margin), DF$expected)
  
  res <- DF %>% 
    dplyr::mutate(
      actual = aggregate_byname(m, margin = margin), 
      equal = all.equal(actual, expected)
    )
  expect_true(all(res$equal))
  
  # Now add an aggregation map
  am <- list(new_row = c("r1", "r2"))
  e4row <- matsbyname::Matrix(c(12, 15, 18), byrow = TRUE, nrow = 1, ncol = 3,
                              dimnames = list(c("new_row"), c("c2", "c1", "c1")), 
                              rowtype = "rows", coltype = "cols")
  e4col <- m3
  e4both <- matsbyname::Matrix(c(28, 11, 
                                 5, 1), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("r1", "r2"), c("c1", "c2")), 
                               rowtype = "rows", coltype = "cols")
  
  expect_equal(aggregate_byname(m3, aggregation_map = am, margin = 1), e4row)
  # The next call should fail, because we're 
  # trying to aggregate on columns, but
  # we're using an aggregation_map designed for rows.
  # The aggregation fails to produce any changes in the data frame.
  # When the aggregate_byname function tries to sort the columns, 
  # it encounters duplicated row names and fails.
  expect_error(aggregate_byname(m3, aggregation_map = am, margin = 2), "Row names not unique. Duplicated row names are: c1")
  # The next call should fail, because we're trying to aggregate on both rows and columns (margin = c(1, 2)), but
  # the aggregation_map only aggregates by rows.
  # When we try to sum across both margins, 
  # there is a duplicate name ("c1"), which causes a problem.
  expect_error(aggregate_byname(m3, aggregation_map = am, margin = c(1, 2)), "Row names not unique. Duplicated row names are: c1")
  
  # The next call should work.
  expect_equal(aggregate_byname(m3), e4both)
  
  DF2 <- tibble::tibble(
    m = list(m3, m3, m3), 
    margin = list(1, 2, c(1, 2)), 
    expected = list(e4row, e3col, e4both), 
    am = list(am, NULL, NULL)
  )
  res2 <- DF2 %>% 
    dplyr::mutate(
      actual = aggregate_byname(m, margin = margin, aggregation_map = am), 
      equal = all.equal(actual, expected)
    )
  expect_true(all(res2$equal))
})


test_that("aggregate_byname() works when removing multiple rows", {
  a <- matrix(1:4, nrow = 4, dimnames = list(c("a", "a", "b", "b"), "c1"))
  e <- matrix(c(3, 7), nrow = 2, dimnames = list(c("a", "b"), "c1"))
  expect_equal(aggregate_byname(a), e)
})


test_that("aggregate_byname() works when removing multiple rows for Matrix objects", {
  a <- matsbyname::Matrix(1:4, nrow = 4, ncol = 1, dimnames = list(c("a", "a", "b", "b"), "c1"))
  e <- matsbyname::Matrix(c(3, 7), nrow = 2, ncol = 1, dimnames = list(c("a", "b"), "c1"))
  expect_equal(aggregate_byname(a), e)
})


test_that("aggregate_byname() works when margin is NA", {
  Y <- matrix(1, nrow = 4, dimnames = list(c("a", "a", "b", "b"), "c1")) %>% 
    setrowtype("Product") %>% setcoltype("Unit")
  res <- Y %>% 
    aggregate_byname(margin = "Industry")
  expect_equal(res, Y)
})


test_that("aggregate_byname() works when margin is NA for Matrix objects", {
  Y <- matsbyname::Matrix(1, nrow = 4, ncol = 1, dimnames = list(c("a", "a", "b", "b"), "c1"), 
                          rowtype = "Product", coltype = "Unit")
  res <- Y %>% 
    aggregate_byname(margin = "Industry")
  expect_equal(res, Y)
})


test_that("aggregate_to_pref_suff_byname() works as expected", {
  m <- matrix((1:9), byrow = TRUE, nrow = 3, 
              dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")))
  res1 <- aggregate_to_pref_suff_byname(m, keep = "pref", 
                                        notation = RCLabels::arrow_notation)
  expected1 <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(res1, expected1)
  # Aggregate by suffixes should do a lot, because several prefixes are same.
  res2 <- aggregate_to_pref_suff_byname(m, keep = "suff", 
                                        notation = RCLabels::arrow_notation)
  expected2 <- m %>% 
    rename_to_pref_suff_byname(keep = "suff", notation = RCLabels::arrow_notation) %>% 
    aggregate_byname()
  expect_equal(res2, expected2)
})


test_that("aggregate_to_pref_suff_byname() works with Matrix objects", {
  m <- matsbyname::Matrix((1:9), byrow = TRUE, nrow = 3, ncol = 3,
                          dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")))
  res1 <- aggregate_to_pref_suff_byname(m, keep = "pref", 
                                        notation = RCLabels::arrow_notation)
  expected1 <- rename_to_pref_suff_byname(m, keep = "pref", notation = RCLabels::arrow_notation)
  expect_equal(res1, expected1)
  # Aggregate by suffixes should do a lot, because several prefixes are same.
  res2 <- aggregate_to_pref_suff_byname(m, keep = "suff", 
                                        notation = RCLabels::arrow_notation)
  expected2 <- m %>% 
    rename_to_pref_suff_byname(keep = "suff", notation = RCLabels::arrow_notation) %>% 
    aggregate_byname()
  expect_equal(res2, expected2)
})


test_that("aggregate_to_pref_suff_byname() works with a column vector", {
  # Ran into a bug where aggregating a column vector fails.
  # A column vector should aggregate to itself.
  # But instead, I get a "subscript out of bounds" error.
  # This test triggers that bug.
  #     -- MKH, 23 Nov 2020.
  m <- matrix(1:4, ncol = 1, dimnames = list(letters[1:4], "Product -> Industry"))
  # This aggregation should simply return m with renamed column
  res <- aggregate_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expected <- m %>% 
    magrittr::set_colnames("Industry")
  expect_equal(res, expected)
})


test_that("aggregate_to_pref_suff_byname() works with a column vector that is a Matrix", {
  # Ran into a bug where aggregating a column vector fails.
  # A column vector should aggregate to itself.
  # But instead, I get a "subscript out of bounds" error.
  # This test triggers that bug.
  #     -- MKH, 23 Nov 2020.
  m <- matsbyname::Matrix(1:4, nrow = 4, ncol = 1, dimnames = list(letters[1:4], "Product -> Industry"))
  # This aggregation should simply return m with renamed column
  res <- aggregate_to_pref_suff_byname(m, keep = "suff", margin = 2, notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(res))
  expected <- m %>% 
    magrittr::set_colnames("Industry")
  expect_equal(res, expected)
})


test_that("aggregate_to_pref_suff_byname() handles types correctly", {
  m <- matrix((1:9), byrow = TRUE, nrow = 3, 
              dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y"))) %>% 
    setrowtype("row -> letter") %>% setcoltype("col -> letter")
  
  res <- aggregate_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_equal(rowtype(res), "letter")
  expect_equal(coltype(res), "letter")
})


test_that("aggregate_to_pref_suff_byname() handles types correctly for Matrix objects", {
  m <- matsbyname::Matrix((1:9), byrow = TRUE, nrow = 3, ncol = 3,
                          dimnames = list(c("r1 -> b", "r2 -> b", "r3 -> a"), c("c1 -> z", "c2 -> y", "c3 -> y")), 
                          rowtype = "row -> letter", coltype = "col -> letter")
  
  res <- aggregate_to_pref_suff_byname(m, keep = "suff", notation = RCLabels::arrow_notation)
  expect_true(is.Matrix(res))
  expect_equal(rowtype(res), "letter")
  expect_equal(coltype(res), "letter")
})


test_that("aggregate_pieces_byname() works as expected", {
  m <- matrix(c(1, 2, 3, 
                4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
              dimnames = list(c("a [from b]", "c [from d]"), 
                              c("e [from f]", "g [from h]", "i [from j]")))
  
  res1 <- m %>%
    aggregate_pieces_byname(piece = "suff", 
                            notation = RCLabels::from_notation,
                            aggregation_map = list(rows = c("b", "d"), 
                                                   cols = c("h", "j")))
  
  expected1 <- matrix(c(16, 5), nrow = 1, ncol = 2, byrow = TRUE, 
                      dimnames = list("rows", c("cols", "f")))
  expect_equal(res1, expected1)
})


test_that("aggregate_pieces_byname() works as expected", {
  m <- matsbyname::Matrix(c(1, 2, 3, 
                            4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("a [from b]", "c [from d]"), 
                                          c("e [from f]", "g [from h]", "i [from j]")))
  
  res1 <- m %>%
    aggregate_pieces_byname(piece = "suff", 
                            notation = RCLabels::from_notation,
                            aggregation_map = list(rows = c("b", "d"), 
                                                   cols = c("h", "j")))
  expect_true(is.Matrix(res1))
  expected1 <- matsbyname::Matrix(c(16, 5), nrow = 1, ncol = 2, byrow = TRUE, 
                                  dimnames = list("rows", c("cols", "f")))
  expect_equal(res1, expected1)
})


test_that("aggregate_pieces_byname() works with aggregation by type", {
  m <- matrix(c(1, 0, 0, 
                0, 1, 1, 
                0, 1, 1), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("Gasoline [from Oil refineries]", 
                                "Electricity [from Main activity producer electricity plants]", 
                                "Electricity [from Hydro]"),
                              c("Automobiles", "LED lamps", "CFL lamps"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  actual1 <- aggregate_pieces_byname(m, piece = "noun", margin = "Product",
                                     notation = RCLabels::bracket_notation)
  expected1 <- matrix(c(0, 2, 2, 
                        1, 0, 0), nrow = 2, ncol = 3, byrow = TRUE, 
                      dimnames = list(c("Electricity", "Gasoline"),
                                      c("Automobiles", "LED lamps", "CFL lamps"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  expect_equal(actual1, expected1)
  
  # Try transposed
  mT <- transpose_byname(m)
  actual2 <- aggregate_pieces_byname(mT, piece = "noun", margin = "Product", 
                                     notation = RCLabels::bracket_notation)
  expected2 <- transpose_byname(expected1)
  expect_equal(actual2, expected2)
  
  # Try in a list
  actual3 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     notation = RCLabels::bracket_notation)
  expected3 <- list(expected1, expected2)
  expect_equal(actual3, expected3)
  
  # Try with an aggregation map
  actual4 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     aggregation_map = list(list(final = c("Electricity", "Gasoline")),
                                                            list(final = c("Electricity", "Gasoline"))),
                                     notation = RCLabels::bracket_notation)
  expected4 <- matrix(c(1, 2, 2), nrow = 1, ncol = 3, 
                      dimnames = list("final", c("Automobiles", "LED lamps", "CFL lamps"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  expect_equal(actual4, list(expected4, transpose_byname(expected4)))
  
  # Try with a single aggregation map that is spread to both items in the list.
  actual5 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
                                     notation = RCLabels::bracket_notation)
  expect_equal(actual5, list(expected4, transpose_byname(expected4)))
  
  # Try in a data frame.
  df <- tibble::tibble(m = list(m, mT)) %>%
    dplyr::mutate(
      agg = aggregate_pieces_byname(a = m, 
                                    piece = "noun",
                                    margin = "Product", 
                                    aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
                                    notation = RCLabels::bracket_notation)
    )
  expect_equal(df$agg, list(expected4, transpose_byname(expected4)))
  
  
  # Try in a data frame using columns for arguments.
  df <- tibble::tibble(m = list(m, mT), 
                       pce = "noun",
                       mgn = "Product",
                       agg_map = list(list(final = c("Electricity", "Gasoline"))),
                       notn = list(RCLabels::bracket_notation)) %>%
    dplyr::mutate(
      agg = aggregate_pieces_byname(a = m, 
                                    piece = pce, 
                                    margin = mgn, 
                                    aggregation_map = agg_map, 
                                    notation = notn)
    )
  expect_equal(df$agg, list(expected4, transpose_byname(expected4)))
})


test_that("aggregate_pieces_byname() works with aggregation by type in Matrix objects", {
  m <- matsbyname::Matrix(c(1, 0, 0, 
                            0, 1, 1, 
                            0, 1, 1), nrow = 3, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("Gasoline [from Oil refineries]", 
                                            "Electricity [from Main activity producer electricity plants]", 
                                            "Electricity [from Hydro]"),
                                          c("Automobiles", "LED lamps", "CFL lamps")), 
                          rowtype = "Product", coltype = "Industry")
  actual1 <- aggregate_pieces_byname(m, piece = "noun", margin = "Product",
                                     notation = RCLabels::bracket_notation)
  expect_true(is.Matrix(actual1))
  expected1 <- matsbyname::Matrix(c(0, 2, 2, 
                                    1, 0, 0), nrow = 2, ncol = 3, byrow = TRUE, 
                                  dimnames = list(c("Electricity", "Gasoline"),
                                                  c("Automobiles", "LED lamps", "CFL lamps")), 
                                  rowtype = "Product", coltype = "Industry")
  expect_equal(actual1, expected1)
  
  # Try transposed
  mT <- transpose_byname(m)
  actual2 <- aggregate_pieces_byname(mT, piece = "noun", margin = "Product", 
                                     notation = RCLabels::bracket_notation)
  expected2 <- transpose_byname(expected1)
  expect_equal(actual2, expected2)
  
  # Try in a list
  actual3 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     notation = RCLabels::bracket_notation)
  expected3 <- list(expected1, expected2)
  expect_equal(actual3, expected3)
  
  # Try with an aggregation map
  actual4 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     aggregation_map = list(list(final = c("Electricity", "Gasoline")),
                                                            list(final = c("Electricity", "Gasoline"))),
                                     notation = RCLabels::bracket_notation)
  expected4 <- matsbyname::Matrix(c(1, 2, 2), nrow = 1, ncol = 3, 
                                  dimnames = list("final", c("Automobiles", "LED lamps", "CFL lamps")), 
                                  rowtype = "Product", coltype = "Industry")
  expect_equal(actual4, list(expected4, transpose_byname(expected4)))
  
  # Try with a single aggregation map that is spread to both items in the list.
  actual5 <- aggregate_pieces_byname(a = list(m, mT), piece = "noun", 
                                     margin = "Product",
                                     aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
                                     notation = RCLabels::bracket_notation)
  expect_equal(actual5, list(expected4, transpose_byname(expected4)))
  
  # Try in a data frame.
  df <- tibble::tibble(m = list(m, mT)) %>%
    dplyr::mutate(
      agg = aggregate_pieces_byname(a = m, 
                                    piece = "noun",
                                    margin = "Product", 
                                    aggregation_map = list(list(final = c("Electricity", "Gasoline"))),
                                    notation = RCLabels::bracket_notation)
    )
  expect_equal(df$agg, list(expected4, transpose_byname(expected4)))
  
  
  # Try in a data frame using columns for arguments.
  df <- tibble::tibble(m = list(m, mT), 
                       pce = "noun",
                       mgn = "Product",
                       agg_map = list(list(final = c("Electricity", "Gasoline"))),
                       notn = list(RCLabels::bracket_notation)) %>%
    dplyr::mutate(
      agg = aggregate_pieces_byname(a = m, 
                                    piece = pce, 
                                    margin = mgn, 
                                    aggregation_map = agg_map, 
                                    notation = notn)
    )
  expect_equal(df$agg, list(expected4, transpose_byname(expected4)))
})


test_that("aggregate_pieces_byname() works with funny names", {
  m_pieces <- matrix(c(1, 2, 3,
                       4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                     dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                                     c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  
  actual1 <- rename_to_piece_byname(m_pieces, piece = "from", margin = 1, notation = RCLabels::bracket_notation)
  expected1 <- matrix(c(1, 2, 3,
                        4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                      dimnames = list(c("Coal", "Solar"), 
                                      c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  expect_equal(actual1, expected1)
  
  
  actual2 <- aggregate_pieces_byname(m_pieces, piece = "from", margin = 1, notation = RCLabels::bracket_notation, 
                                     aggregation_map = list(`All sources` = c("Coal", "Solar")))
  expected2 <- matrix(c(5, 7, 9), nrow = 1, ncol = 3, byrow = TRUE, 
                      dimnames = list(c("All sources"), 
                                      c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  expect_equal(actual2, expected2)  
})


test_that("aggregate_pieces_byname() works with funny names", {
  m_pieces <- matsbyname::Matrix(c(1, 2, 3,
                                   4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                                 dimnames = list(c("Electricity [from Coal]", "Electricity [from Solar]"), 
                                                 c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  
  actual1 <- rename_to_piece_byname(m_pieces, piece = "from", margin = 1, notation = RCLabels::bracket_notation)
  expect_true(is.Matrix(actual1))
  expected1 <- matsbyname::Matrix(c(1, 2, 3,
                                    4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE, 
                                  dimnames = list(c("Coal", "Solar"), 
                                                  c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  expect_equal(actual1, expected1)
  
  actual2 <- aggregate_pieces_byname(m_pieces, piece = "from", margin = 1, notation = RCLabels::bracket_notation, 
                                     aggregation_map = list(`All sources` = c("Coal", "Solar")))
  expected2 <- matsbyname::Matrix(c(5, 7, 9), nrow = 1, ncol = 3, byrow = TRUE, 
                                  dimnames = list(c("All sources"), 
                                                  c("Motors -> MD", "Cars -> MD", "LED lamps -> Light")))
  expect_equal(actual2, expected2)  
})


test_that("aggregate_pieces_byname() works when inferring notation", {
  m <- matrix(c(1, 2, 3, 
                4, 5, 6, 
                7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
              dimnames = list(c("a [from b]", "c [from d]", "e [from d]"), 
                              c("e [from f]", "g [from h]", "i [from j]")))
  
  res1 <- m %>%
    aggregate_pieces_byname(piece = "suff",
                            choose_most_specific = TRUE,
                            aggregation_map = list(rows = "d", 
                                                   cols = c("h", "j")))
  
  expected1 <- matrix(c(5, 1,
                        28, 11), nrow = 2, ncol = 2, byrow = TRUE, 
                      dimnames = list(c("b", "rows"), c("cols", "f")))
  expect_equal(res1, expected1)
})


test_that("aggregate_pieces_byname() works when inferring notation on Matric objects", {
  m <- matsbyname::Matrix(c(1, 2, 3, 
                            4, 5, 6, 
                            7, 8, 9), nrow = 3, ncol = 3, byrow = TRUE, 
                          dimnames = list(c("a [from b]", "c [from d]", "e [from d]"), 
                                          c("e [from f]", "g [from h]", "i [from j]")))
  
  res1 <- m %>%
    aggregate_pieces_byname(piece = "suff",
                            choose_most_specific = TRUE,
                            aggregation_map = list(rows = "d", 
                                                   cols = c("h", "j")))
  expect_true(is.Matrix(res1))
  expected1 <- matsbyname::Matrix(c(5, 1,
                                    28, 11), nrow = 2, ncol = 2, byrow = TRUE, 
                                  dimnames = list(c("b", "rows"), c("cols", "f")))
  expect_equal(res1, expected1)
})


test_that("aggregate_pieces_byname() works with repeated row labels not in aggregation_map", {
  # Ran into a bug where failure occurs if there are
  # repeated row (or column) names that are not included 
  # in the aggregation_map.
  a <- matrix(1:6, nrow = 3, ncol = 2, dimnames = list(c("a [from b]", "c [from d]", "c [from e]"), c("c1", "c2")))
  # This one already works, because we're aggregating the repeated row.
  res1 <- a %>% 
    aggregate_pieces_byname(piece = "noun",
                            margin = 1,
                            inf_notation = FALSE, 
                            notation = RCLabels::bracket_notation, 
                            aggregation_map = list(f = c("a", "c")))
  expected1 <- matrix(c(6, 15), nrow = 1, ncol = 2, dimnames = list("f", c("c1", "c2")))
  expect_equal(res1, expected1)
  # This one is failing, because we're not aggregating the repeated row.
  res2 <- a %>% 
    aggregate_pieces_byname(piece = "noun",
                            margin = 1,
                            inf_notation = FALSE, 
                            notation = RCLabels::bracket_notation, 
                            aggregation_map = list(f = c("a", "b")))
  expected2 <- matrix(c(5, 11,
                        1,  4), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("c", "f"), c("c1", "c2")))
  expect_equal(res2, expected2)
})


test_that("aggregate_pieces_byname() works with 2 notations", {
  # This works, when we are inferring notation
  a <- matrix(1:3, nrow = 3, ncol = 1, dimnames = list(c("a [from b]", "c -> d", "c -> e"), "c1"))
  res1 <- a %>% 
    aggregate_pieces_byname(piece = "noun")
  expected1 <- matrix(c(1, 5), nrow = 2, ncol = 1, dimnames = list(c("a", "c"), "c1"))
  expect_equal(res1, expected1)
  
  # However, this fails when we are specifying notation and not inferring.
  # The second notation (arrow notation) is not being renamed to the noun.
  res2 <- a %>% 
    aggregate_pieces_byname(piece = "noun", 
                            margin = 1,
                            notation = RCLabels::make_list(list(RCLabels::bracket_notation, RCLabels::arrow_notation), n = 1, lenx = 1), 
                            inf_notation = TRUE)
  expected2 <- expected1
  expect_equal(res2, expected2)
  
  # Try in a list
  res3 <- list(a, a) %>% 
    aggregate_pieces_byname(piece = "noun", 
                            margin = 1,
                            notation = RCLabels::make_list(list(RCLabels::bracket_notation, RCLabels::arrow_notation), n = 2, lenx = 1), 
                            inf_notation = TRUE)
  expected3 <- list(expected1, expected1)
  expect_equal(res3, expected3)
})
