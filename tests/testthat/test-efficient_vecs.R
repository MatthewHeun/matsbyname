test_that("kvec_from_template_byname() works as expected", {
  m <- matrix(42, nrow = 4, ncol = 2,
              dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2")))
  expect_equal(kvec_from_template_byname(m, colname = "mycol"), matrix(1, nrow = 4, ncol = 1, 
                                                                       dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  expect_equal(kvec_from_template_byname(m, colname = "myrow", column = FALSE), matrix(1, nrow = 1, ncol = 2, 
                                                                                       dimnames = list("myrow", c("c1", "c2"))))
  
  # Try in a data frame.
  df1 <- tibble::tibble(m = list(m, m), cnme = "mycol", rnme = "myrow", clmn = TRUE, k = c(42, 43), 
                        rtype = "rt", ctype = c("ct1", "ct2"))
  
  res1 <- df1 %>% 
    dplyr::mutate(
      irow = kvec_from_template_byname(m, colname = rnme, column = FALSE),
      icol = kvec_from_template_byname(m, colname = cnme, column = clmn), 
      kcol = kvec_from_template_byname(m, k = k, colname = cnme), 
      with_rt_ct = kvec_from_template_byname(m %>% setrowtype(rtype) %>% setcoltype(ctype), colname = cnme)
    )
  
  expect_equal(res1$irow[[1]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$irow[[2]], matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  expect_equal(res1$icol[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$icol[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  # Try with non-1 value for k
  expect_equal(res1$kcol[[1]], matrix(42, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  expect_equal(res1$kcol[[2]], matrix(43, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  # Test that row and column types are transferred correctly
  expect_equal(res1$with_rt_ct[[1]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct1"))
  expect_equal(res1$with_rt_ct[[2]], matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                 setrowtype("rt") %>% setcoltype("ct2"))
})


test_that("kvec_from_template_byname() works with Matrix objects", {
  m <- matsbyname::Matrix(42, nrow = 4, ncol = 2,
                          dimnames = list(c("r1", "r2", "r3", "r4"), c("c1", "c2")))
  res <- kvec_from_template_byname(m, colname = "mycol")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(1, nrow = 4, ncol = 1, 
                                                    dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  matsbyname:::expect_equal_matrix_or_Matrix(kvec_from_template_byname(m, colname = "myrow", column = FALSE), 
                                             matrix(1, nrow = 1, ncol = 2, 
                                                    dimnames = list("myrow", c("c1", "c2"))))
  
  # Try in a data frame.
  df1 <- tibble::tibble(m = list(m, m), cnme = "mycol", rnme = "myrow", clmn = TRUE, k = c(42, 43), 
                        rtype = "rt", ctype = c("ct1", "ct2"))
  
  res1 <- df1 %>% 
    dplyr::mutate(
      irow = kvec_from_template_byname(m, colname = rnme, column = FALSE),
      icol = kvec_from_template_byname(m, colname = cnme, column = clmn), 
      kcol = kvec_from_template_byname(m, k = k, colname = cnme), 
      with_rt_ct = kvec_from_template_byname(m %>% 
                                               setrowtype(rtype) %>% 
                                               setcoltype(ctype), 
                                             colname = cnme)
    )
  
  matsbyname:::expect_equal_matrix_or_Matrix(res1$irow[[1]],
                                             matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$irow[[2]], 
                                             matrix(1, nrow = 1, ncol = 2, dimnames = list("myrow", c("c1", "c2"))))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[1]],
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[2]],
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$icol[[2]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  # Try with non-1 value for k
  matsbyname:::expect_equal_matrix_or_Matrix(res1$kcol[[1]], 
                                             matrix(42, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$kcol[[2]],
                                             matrix(43, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")))
  
  # Test that row and column types are transferred correctly
  matsbyname:::expect_equal_matrix_or_Matrix(res1$with_rt_ct[[1]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                                               setrowtype("rt") %>% setcoltype("ct1"))
  matsbyname:::expect_equal_matrix_or_Matrix(res1$with_rt_ct[[2]], 
                                             matrix(1, nrow = 4, ncol = 1, dimnames = list(c("r1", "r2", "r3", "r4"), "mycol")) %>% 
                                               setrowtype("rt") %>% setcoltype("ct2"))
})


test_that("kvec_from_template_byname() passes old i_byname tests", {
  
  # First, test with single values.
  single_mat <- create_matrix_byname(1, nrow = 1, ncol = 1,
                                     dimnames = list("r1", "c1"))
  
  expect_equal(
    kvec_from_template_byname(single_mat, colname = "output_column"), 
    matrix(1, dimnames = list("r1", "output_column"))
  )
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"))
  expect_equal(
    kvec_from_template_byname(single_mat_2, colname = "output_column"), 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "output_column"))
  )
  
  # Second, test with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")))
  
  res_list <- kvec_from_template_byname(list_of_mats, colname = "output_column")
  
  expect_equal(
    res_list[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list[[2]], 
    matrix(1, dimnames = list("R1", "output_column"))
  )
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))))
  
  res_list_2 <- kvec_from_template_byname(list_of_mats_2, colname = "output_column")
  
  expect_equal(
    res_list_2[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  expect_equal(
    res_list_2[[2]], 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("R1", "R2"), "output_column"))
  )
  
  
  # Third test with data frames:
  
  # Creating data frame of matrices, with a year column and a matrix column:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matrix(1:4, ncol = 2, dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matrix(1:3, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matrix(1:4, ncol = length(industrynames), nrow = length(productnames), dimnames = list(productnames, industrynames)) %>% setrowtype("Products") %>% setcoltype("Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  # Now creating the unity vector
  res <- dfUs %>% 
    dplyr::mutate(
      unity_vec = kvec_from_template_byname(matrix_byname, colname = "Product")
    )
  
  # Checking number of coefficients in each vector
  expect_equal(nrow(res$unity_vec[[1]]), 2)
  expect_equal(nrow(res$unity_vec[[2]]), 2)
  expect_equal(nrow(res$unity_vec[[3]]), 3)
  
  # Checking rowtypes
  expect_equal(res$unity_vec[[1]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[2]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[3]] %>% rowtype(), "Products")
  
  # Checking coltypes
  expect_equal(res$unity_vec[[1]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[2]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[3]] %>% coltype(), "Industries")
  
  # Checking single coefficient values
  expect_equal(res$unity_vec[[1]][["p1", "Product"]], 1)
  expect_equal(res$unity_vec[[2]][["p2", "Product"]], 1)
  expect_equal(res$unity_vec[[3]][["p3", "Product"]], 1)
  
  # Checking sums
  res2 <- res %>%
    dplyr::mutate(
      sum_unity = matsbyname::sumall_byname(unity_vec)
    )
  # Check
  expect_equal(res2$sum_unity[[1]], 2)
  expect_equal(res2$sum_unity[[2]], 2)
  expect_equal(res2$sum_unity[[3]], 3)
})


test_that("kvec_from_template_byname() passes old i_byname tests with Matrix objects", {
  
  # First, test with single values.
  single_mat <- create_matrix_byname(1, nrow = 1, ncol = 1,
                                     dimnames = list("r1", "c1"), matrix_class = "Matrix")
  
  res <- kvec_from_template_byname(single_mat, colname = "output_column")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, matrix(1, dimnames = list("r1", "output_column")))
  
  single_mat_2 <- create_matrix_byname(c(1, 2), nrow = 2, ncol = 1,
                                       dimnames = list(c("r1", "r2"), "c1"), matrix_class = "Matrix")
  matsbyname:::expect_equal_matrix_or_Matrix(
    kvec_from_template_byname(single_mat_2, colname = "output_column"), 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("r1", "r2"), "output_column"))
  )
  
  # Second, test with a list
  list_of_mats <- create_matrix_byname(list(1, 2), nrow = list(1, 1), ncol = list(1,1), 
                                       dimnames = list(list("r1", "c1"), list("R1", "C1")), 
                                       matrix_class = "Matrix")
  
  res_list <- kvec_from_template_byname(list_of_mats, colname = "output_column")
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list[[2]], 
    matrix(1, dimnames = list("R1", "output_column"))
  )
  
  # Test with a list of different dimensions
  list_of_mats_2 <- create_matrix_byname(list(1, c(2, 3, 4, 5)), nrow = list(1, 2), ncol = list(1,2), 
                                         dimnames = list(list("r1", "c1"), list(c("R1", "R2"), c("C1", "C2"))), 
                                         matrix_class = "Matrix")
  
  res_list_2 <- kvec_from_template_byname(list_of_mats_2, colname = "output_column")
  
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list_2[[1]],
    matrix(1, dimnames = list("r1", "output_column"))
  )
  matsbyname:::expect_equal_matrix_or_Matrix(
    res_list_2[[2]], 
    matrix(1, nrow = 2, ncol = 1, dimnames = list(c("R1", "R2"), "output_column"))
  )
  
  
  # Third test with data frames:
  
  # Creating data frame of matrices, with a year column and a matrix column:
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2")
  U <- matsbyname::Matrix(1:4, nrow = 2, ncol = 2, dimnames = list(productnames, industrynames), 
                          rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2")
  industrynames <- c("i1", "i2", "i3")
  U2 <- matsbyname::Matrix(1:3, ncol = length(industrynames), nrow = length(productnames), 
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  
  productnames <- c("p1", "p2", "p3")
  industrynames <- c("i1", "i2", "i3", "i4")
  U3 <- matsbyname::Matrix(1:4, ncol = length(industrynames), nrow = length(productnames), 
                           dimnames = list(productnames, industrynames), 
                           rowtype = "Products", coltype = "Industries")
  
  dfUs <- data.frame(
    year = numeric(),
    matrix_byname = I(list())
  )
  
  dfUs[[1, "matrix_byname"]] <- U
  dfUs[[2, "matrix_byname"]] <- U2
  dfUs[[3, "matrix_byname"]] <- U3
  
  dfUs[[1, "year"]] <- 2000
  dfUs[[2, "year"]] <- 2001
  dfUs[[3, "year"]] <- 2002
  
  # Now creating the unity vector
  res <- dfUs %>% 
    dplyr::mutate(
      unity_vec = kvec_from_template_byname(matrix_byname, colname = "Product")
    )
  
  # Checking number of coefficients in each vector
  expect_equal(nrow(res$unity_vec[[1]]), 2)
  expect_equal(nrow(res$unity_vec[[2]]), 2)
  expect_equal(nrow(res$unity_vec[[3]]), 3)
  
  # Checking rowtypes
  expect_equal(res$unity_vec[[1]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[2]] %>% rowtype(), "Products")
  expect_equal(res$unity_vec[[3]] %>% rowtype(), "Products")
  
  # Checking coltypes
  expect_equal(res$unity_vec[[1]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[2]] %>% coltype(), "Industries")
  expect_equal(res$unity_vec[[3]] %>% coltype(), "Industries")
  
  # Checking single coefficient values
  expect_equal(res$unity_vec[[1]]["p1", "Product"], 1)
  expect_equal(res$unity_vec[[2]]["p2", "Product"], 1)
  expect_equal(res$unity_vec[[3]]["p3", "Product"], 1)
  
  # Checking sums
  res2 <- res %>%
    dplyr::mutate(
      sum_unity = matsbyname::sumall_byname(unity_vec)
    )
  # Check
  expect_equal(res2$sum_unity[[1]], 2)
  expect_equal(res2$sum_unity[[2]], 2)
  expect_equal(res2$sum_unity[[3]], 3)
})


test_that("vec_from_store_byname() works as expected with single matrices", {
  a <- matrix(42, nrow = 2, ncol = 3, 
              dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  v <- matrix(1:10, nrow = 10, ncol = 1, 
              dimnames = list(paste0("r", 1:10) %>% rev(), "c1")) %>%
    setrowtype("rt") %>% setcoltype("ct")
  expect_equal(vec_from_store_byname(a = a, v = v), 
               matrix(c(10, 9), nrow = 2, ncol = 1, 
                      dimnames = list(c("r1", "r2"), "c1")) %>%
                 setrowtype("rt") %>% setcoltype("ct"))
})


test_that("vec_from_store_byname() works with single Matrix objects", {
  a <- matsbyname::Matrix(42, nrow = 2, ncol = 3, 
                          dimnames = list(c("r1", "r2"), c("c1", "c2", "c3")))
  v <- matsbyname::Matrix(1:10, nrow = 10, ncol = 1, 
                          dimnames = list(paste0("r", 1:10) %>% rev(), "c1"), 
                          rowtype = "rt", coltype = "ct")
  matsbyname:::expect_equal_matrix_or_Matrix(vec_from_store_byname(a = a, v = v), 
                                             matrix(c(10, 9), nrow = 2, ncol = 1, 
                                                    dimnames = list(c("r1", "r2"), "c1")) %>%
                                               setrowtype("rt") %>% setcoltype("ct"))
})


test_that("vec_from_store_byname() works as expected with single matrices and nouns", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v, a_piece = "noun"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and nouns", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                          dimnames = list(c("Electricity", 
                                            "Peat", 
                                            "Hydro", 
                                            "Crude oil",
                                            "Coal", 
                                            "Hard coal (if no detail)", 
                                            "Brown coal"), 
                                          "phi"), 
                          rowtype = "Product", coltype = "phi")
  res <- vec_from_store_byname(a, v, a_piece = "noun")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and pref suff", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Hydro", 
                                "Crude oil",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "Brown coal"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  # Try with prefixes
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref"), 
               matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try with suffixes
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "from e in f", 
                                 "Hydro", 
                                 "Crude oil",
                                 "from b in c", 
                                 "Hard coal (if no detail)", 
                                 "from Production in USA"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "suff"), 
               matrix(c(5, 2, 7), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and pref suff", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                          dimnames = list(c("Electricity", 
                                            "Peat", 
                                            "Hydro", 
                                            "Crude oil",
                                            "Coal", 
                                            "Hard coal (if no detail)", 
                                            "Brown coal"), 
                                          "phi"), 
                          rowtype = "Product", coltype = "phi")
  
  # Try with prefixes
  res <- vec_from_store_byname(a, v, a_piece = "pref")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(1, 5, 4), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
  # Try with suffixes
  v2 <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                           dimnames = list(c("Electricity", 
                                             "from e in f", 
                                             "Hydro", 
                                             "Crude oil",
                                             "from b in c", 
                                             "Hard coal (if no detail)", 
                                             "from Production in USA"), 
                                           "phi"), 
                           rowtype = "Product", coltype = "phi")
  matsbyname:::expect_equal_matrix_or_Matrix(vec_from_store_byname(a, v2, a_piece = "suff"), 
                                             matrix(c(5, 2, 7), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single matrices and prepositions", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "USA", 
                                "c",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "in"), 
               matrix(c(4, 7, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  
  
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail)", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  expect_equal(vec_from_store_byname(a, v2, a_piece = "from"), 
               matrix(c(7, 4, 3), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  # Try when the preposition (in this case "to") is not present in a.
  expect_equal(vec_from_store_byname(a, v, a_piece = "to"), 
               matrix(c(NA_real_, NA_real_, NA_real_), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in c]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
  
  # Try when we use different pieces of a and v.
  a3 <- matrix(42, nrow = 3, ncol = 5, 
               dimnames = list(c("Electricity [from b in GBR]", 
                                 "Coal [from e in f]", 
                                 "Crude oil [from Production in USA]"), 
                               c("Main activity producer electricity plants", 
                                 "Wind turbines", 
                                 "Oil refineries", 
                                 "Coal mines", 
                                 "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v3 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_error(vec_from_store_byname(a3, v3, a_piece = "in", v_piece = "from"), 
               "v_pieces must be unique in vec_from_store_byname")
  
  
  v4 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat [from nowhere]", 
                                 "Production [from GHA]", 
                                 "e [from ZAF]",
                                 "Coal [from AUS]", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b [from Nebraska]"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  expect_equal(vec_from_store_byname(a3, v4, a_piece = "in", v_piece = "from"), 
               matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                      dimnames = list(c("Electricity [from b in GBR]", 
                                        "Coal [from e in f]", 
                                        "Crude oil [from Production in USA]"), 
                                      "phi")) %>%
                 setrowtype("Product") %>% setcoltype("phi"))
})


test_that("vec_from_store_byname() works as expected with single Matrix objects and prepositions", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  # Keep v as a matrix
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "USA", 
                                "c",
                                "Coal", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  res <- vec_from_store_byname(a, v, a_piece = "in")
  expect_true(is.Matrix(res))
  matsbyname:::expect_equal_matrix_or_Matrix(res, 
                                             matrix(c(4, 7, 3), nrow = 3, ncol = 1, 
                                                    dimnames = list(c("Electricity [from b in c]", 
                                                                      "Coal [from e in f]", 
                                                                      "Crude oil [from Production in USA]"), 
                                                                    "phi")) %>%
                                               setrowtype("Product") %>% setcoltype("phi"))
  
  # Again, leave v2 as a matrix
  v2 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail)", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  matsbyname:::expect_equal_matrix_or_Matrix(
    vec_from_store_byname(a, v2, a_piece = "from"), 
    matrix(c(7, 4, 3), nrow = 3, ncol = 1, 
           dimnames = list(c("Electricity [from b in c]", 
                             "Coal [from e in f]", 
                             "Crude oil [from Production in USA]"), 
                           "phi")) %>%
      setrowtype("Product") %>% setcoltype("phi"))
  # Try when the preposition (in this case "to") is not present in a.
  res_null <- vec_from_store_byname(a, v, a_piece = "to")
  expect_true(is.na(res_null[1, 1]))
  expect_true(is.na(res_null[2, 1]))
  expect_true(is.na(res_null[3, 1]))
  
  # Try when we use different pieces of a and v.
  a3 <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                           dimnames = list(c("Electricity [from b in GBR]", 
                                             "Coal [from e in f]", 
                                             "Crude oil [from Production in USA]"), 
                                           c("Main activity producer electricity plants", 
                                             "Wind turbines", 
                                             "Oil refineries", 
                                             "Coal mines", 
                                             "Automobiles")), 
                           rowtype = "Product", coltype = "Industry")
  v3 <- matrix(1:7, nrow = 7, ncol = 1, 
               dimnames = list(c("Electricity [from USA]", 
                                 "Peat", 
                                 "Production", 
                                 "e",
                                 "Coal", 
                                 "Hard coal (if no detail) [from GBR]", 
                                 "b"), 
                               "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expect_error(vec_from_store_byname(a3, v3, a_piece = "in", v_piece = "from"), 
               "v_pieces must be unique in vec_from_store_byname")
  
  
  v4 <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                           dimnames = list(c("Electricity [from USA]", 
                                             "Peat [from nowhere]", 
                                             "Production [from GHA]", 
                                             "e [from ZAF]",
                                             "Coal [from AUS]", 
                                             "Hard coal (if no detail) [from GBR]", 
                                             "b [from Nebraska]"), 
                                           "phi"), 
                           rowtype = "Product", coltype = "phi")
  res4 <- vec_from_store_byname(a3, v4, a_piece = "in", v_piece = "from")
  expect_equal(res4[1, 1], 6)
  expect_true(is.na(res4[2, 1]))
  expect_equal(res4[3, 1], 1)
})


test_that("vec_from_store_byname() works when a row vector is desired.", {
  a <- matrix(42, nrow = 3, ncol = 2, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Wind turbines", 
                                "Oil wells"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta")) %>%
    setrowtype("Industry") %>% setcoltype("eta")
  
  expect_equal(vec_from_store_byname(a, v, a_piece = "pref", margin = 2), 
               matrix(c(3, 
                        5), nrow = 2, ncol = 1, 
                      dimnames = list(c("Wind turbines", 
                                        "Oil wells"), 
                                      "eta")) %>%
                 setrowtype("Industry") |> setcoltype("eta"))
  
  # See if it works with a row vector for v.
  v_row <- matrix(1:7, nrow = 1, ncol = 7, 
                  dimnames = list("eta", 
                                  c("Electricity", 
                                    "Peat", 
                                    "Wind turbines", 
                                    "c",
                                    "Oil wells", 
                                    "Hard coal (if no detail)", 
                                    "f"))) %>%
    setrowtype("eta") %>% setcoltype("Industry")
  expect_equal(vec_from_store_byname(a, v_row, a_piece = "pref", margin = 2), 
               matrix(c(3, 
                        5), nrow = 2, ncol = 1, 
                      dimnames = list(c("Wind turbines", 
                                        "Oil wells"), 
                                      "eta")) %>%
                 setrowtype("Industry") |> setcoltype("eta"))
})


test_that("vec_from_store_byname() works when a row vector Matrix object is desired.", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 2, 
                          dimnames = list(c("Electricity [from b in c]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Wind turbines", 
                                            "Oil wells")), 
                          rowtype = "Product", coltype = "Industry")
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity", 
                                "Peat", 
                                "Wind turbines", 
                                "c",
                                "Oil wells", 
                                "Hard coal (if no detail)", 
                                "f"), 
                              "eta")) %>%
    setrowtype("Industry") %>% setcoltype("eta")
  
  res <- vec_from_store_byname(a, v, a_piece = "pref", margin = 2)
  matsbyname:::expect_equal_matrix_or_Matrix(
    res, 
    matrix(c(3, 
             5), nrow = 2, ncol = 1, 
           dimnames = list(c("Wind turbines", 
                             "Oil wells"), 
                           "eta")) %>%
      setrowtype("Industry") |> setcoltype("eta"))
  
  # See if it works with a row vector for v.
  v_row <- matrix(1:7, nrow = 1, ncol = 7, 
                  dimnames = list("eta", 
                                  c("Electricity", 
                                    "Peat", 
                                    "Wind turbines", 
                                    "c",
                                    "Oil wells", 
                                    "Hard coal (if no detail)", 
                                    "f"))) %>%
    setrowtype("eta") %>% setcoltype("Industry")
  matsbyname:::expect_equal_matrix_or_Matrix(
    vec_from_store_byname(a, v_row, a_piece = "pref", margin = 2), 
    matrix(c(3, 
             5), nrow = 2, ncol = 1, 
           dimnames = list(c("Wind turbines", 
                             "Oil wells"), 
                           "eta")) %>%
      setrowtype("Industry") |> setcoltype("eta"))
})


test_that("vec_from_store_byname() works when a is a Matrix and v is a matrix.", {
  a <- matrix(42, nrow = 3, ncol = 2, 
              dimnames = list(c("Electricity [from b in c]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Wind turbines", 
                                "Oil wells"))) %>% 
    setrowtype("Product") %>% setcoltype("Industry")
  v <- matsbyname::Matrix(1:7, nrow = 7, ncol = 1, 
                          dimnames = list(c("Electricity", 
                                            "Peat", 
                                            "Wind turbines", 
                                            "c",
                                            "Oil wells", 
                                            "Hard coal (if no detail)", 
                                            "f"), 
                                          "eta"), 
                          rowtype = "Industry", coltype = "eta")
  
  res <- vec_from_store_byname(a, v, a_piece = "pref", margin = 2)
  expect_equal(
    res, 
    matrix(c(3, 
             5), nrow = 2, ncol = 1, 
           dimnames = list(c("Wind turbines", 
                             "Oil wells"), 
                           "eta")) %>%
      setrowtype("Industry") |> setcoltype("eta"))
  
  # See if it works with a row vector for v.
  v_row <- matsbyname::Matrix(1:7, nrow = 1, ncol = 7, 
                              dimnames = list("eta", 
                                              c("Electricity", 
                                                "Peat", 
                                                "Wind turbines", 
                                                "c",
                                                "Oil wells", 
                                                "Hard coal (if no detail)", 
                                                "f")), 
                              rowtype = "eta", coltype = "Industry")
  expect_equal(
    vec_from_store_byname(a, v_row, a_piece = "pref", margin = 2), 
    matrix(c(3, 
             5), nrow = 2, ncol = 1, 
           dimnames = list(c("Wind turbines", 
                             "Oil wells"), 
                           "eta")) %>%
      setrowtype("Industry") |> setcoltype("eta"))
})


test_that("vec_from_store_byname() works with lists", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  a_list <- list(a, a, a)
  v_list <- list(v, v, v)
  expected_list <- list(expected, expected, expected)
  
  # Try with notation and prepositions already wrapped in lists.
  res <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                               notation = list(RCLabels::bracket_notation), 
                               prepositions = list(RCLabels::prepositions_list))  
  expect_equal(res, expected_list)
  
  # Try with notation and prepositions not already wrapped in lists.
  
  res2 <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from")
  expect_equal(res2, expected_list)
})


test_that("vec_from_store_byname() works with Matrix objects in lists", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in GBR]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Indsutry")
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, -9999, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  
  a_list <- list(a, a, a)
  v_list <- list(v, v, v)
  expected_list <- list(expected, expected, expected)
  
  # Try with notation and prepositions already wrapped in lists.
  res <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                               notation = list(RCLabels::bracket_notation), 
                               prepositions = list(RCLabels::prepositions_list), 
                               missing = -9999)  
  expect_true(all(mapply(matsbyname:::equal_matrix_or_Matrix, res, expected_list)))
  
  # Try with notation and prepositions not already wrapped in lists.
  
  res2 <- vec_from_store_byname(a_list, v_list, a_piece = "in", v_piece = "from", 
                                missing = -9999)
  expect_true(all(mapply(matsbyname:::equal_matrix_or_Matrix, res2, expected_list)))
})


test_that("vec_from_store_byname() works in a data frame", {
  a <- matrix(42, nrow = 3, ncol = 5, 
              dimnames = list(c("Electricity [from b in GBR]", 
                                "Coal [from e in f]", 
                                "Crude oil [from Production in USA]"), 
                              c("Main activity producer electricity plants", 
                                "Wind turbines", 
                                "Oil refineries", 
                                "Coal mines", 
                                "Automobiles"))) %>%
    setrowtype("Product") %>% setcoltype("Industry")
  
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, NA_real_, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  df <- tibble::tibble(a = list(a, a, a), 
                       v = list(v, v, v), 
                       expected = list(expected, expected, expected))
  
  with_res <- df %>%
    dplyr::mutate(
      actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from")
    )
  expect_equal(with_res$actual, with_res$expected)
})


test_that("vec_from_store_byname() works in a data frame with Matrix objects", {
  a <- matsbyname::Matrix(42, nrow = 3, ncol = 5, 
                          dimnames = list(c("Electricity [from b in GBR]", 
                                            "Coal [from e in f]", 
                                            "Crude oil [from Production in USA]"), 
                                          c("Main activity producer electricity plants", 
                                            "Wind turbines", 
                                            "Oil refineries", 
                                            "Coal mines", 
                                            "Automobiles")), 
                          rowtype = "Product", coltype = "Industry")
  
  v <- matrix(1:7, nrow = 7, ncol = 1, 
              dimnames = list(c("Electricity [from USA]", 
                                "Peat [from nowhere]", 
                                "Production [from GHA]", 
                                "e [from ZAF]",
                                "Coal [from AUS]", 
                                "Hard coal (if no detail) [from GBR]", 
                                "b [from Nebraska]"), 
                              "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  expected <- matrix(c(6, -9999, 1), nrow = 3, ncol = 1, 
                     dimnames = list(c("Electricity [from b in GBR]", 
                                       "Coal [from e in f]", 
                                       "Crude oil [from Production in USA]"), 
                                     "phi")) %>%
    setrowtype("Product") %>% setcoltype("phi")
  
  df <- tibble::tibble(a = list(a, a, a), 
                       v = list(v, v, v), 
                       expected = list(expected, expected, expected))
  
  with_res <- df %>%
    dplyr::mutate(
      actual = vec_from_store_byname(a = a, v = v, a_piece = "in", v_piece = "from", 
                                     missing = -9999)
    )
  expect_true(all(mapply(matsbyname:::expect_equal_matrix_or_Matrix, with_res$actual, with_res$expected)))
})


test_that("vec_from_store_byname() works with multiple matches", {
  a <- matrix(42, nrow = 4, ncol = 2, dimnames = list(c("r1p -> r1s", "r2p -> r2s", "r3p -> r3s", "r1p -> r3s"), 
                                                      c("c1p -> c1s", "c2p -> c2s")))
  vec <- matrix(1:6, nrow = 6, ncol = 1, dimnames = list(c("r1p", "r2p", "r3p", "r1s", "r2s", "r3s"), "col"))
  expect_equal(vec_from_store_byname(a = a, v = vec, a_piece = "pref", notation = RCLabels::arrow_notation), 
               matrix(c(1, 2, 3, 1), ncol = 1, dimnames = list(c("r1p -> r1s", "r2p -> r2s", "r3p -> r3s", "r1p -> r3s"), "col")))
})
  