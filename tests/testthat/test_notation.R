# Contains tests for notation functions in the byname package.

###########################################################
context("Arrow notation")
###########################################################

test_that("arrow notation is created properly", {
  an <- arrow_notation()
  expect_equal(an[["pref_start"]], "")
  expect_equal(an[["pref_end"]], " -> ")
  expect_equal(an[["suff_start"]], " -> ")
  expect_equal(an[["suff_end"]], "")
})


test_that("paren notation is created properly", {
  pn <- paren_notation()
  expect_equal(pn[["pref_start"]], "")
  expect_equal(pn[["pref_end"]], " (")
  expect_equal(pn[["suff_start"]], " (")
  expect_equal(pn[["suff_end"]], ")")
})


test_that("bracket notation is created properly", {
  bn <- bracket_notation()
  expect_equal(bn[["pref_start"]], "")
  expect_equal(bn[["pref_end"]], " [")
  expect_equal(bn[["suff_start"]], " [")
  expect_equal(bn[["suff_end"]], "]")
})


test_that("split_pref_suff works properly", {
  expect_equal(split_pref_suff("a -> b", notation = arrow_notation()), list(pref = "a", suff = "b"))
  expect_equal(split_pref_suff("b [a]", notation = bracket_notation()), list(pref = "b", suff = "a"))
  
  # See if it works with a vector of strings
  expect_equal(split_pref_suff(c("a -> b", "a -> b"), notation = arrow_notation()), 
               list(list(pref = "a", suff = "b"), list(pref = "a", suff = "b")))
  # See if it works with a list of strings
  expect_equal(split_pref_suff(list("a -> b", "a -> b"), notation = arrow_notation()), 
               list(list(pref = "a", suff = "b"), list(pref = "a", suff = "b")))
  
  # Try with unusual prefixes and suffixes
  nl <- notation_vec(pref_start = " {", pref_end = "} ", suff_start = "} ", suff_end = NA_character_)
  expect_equal(split_pref_suff(" {a} bcd", notation = nl), list(pref = "a", suff = "bcd"))

  nl2 <- notation_vec(pref_start = "  {", pref_end = "} ", suff_start = "[ ", suff_end = "]  ")
  expect_equal(split_pref_suff("  {a} [ b]  ", notation = nl2), list(pref = "a", suff = "b"))
  
  expect_equal(split_pref_suff("a [ [b]]", notation = bracket_notation()), list(pref = "a", suff = " [b]"))
  
  # Try with degenerate cases
  nl3 <- notation_vec(sep = "{{}}")
  expect_equal(split_pref_suff("abc {{} def", notation = nl3), list(pref = "abc {{} def", suff = NULL))
  expect_equal(split_pref_suff("abc {{}} def", notation = nl3), list(pref = "abc ", suff = " def"))
  
  # Try with weird parentheses
  nl4 <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "(", suff_end = ")")
  expect_equal(split_pref_suff("(a)(b)", notation = nl4), list(pref = "a", suff = "b"))
  
  expect_equal(split_pref_suff("a b", notation = nl4), list(pref = "a b", suff = NULL))
})


test_that("join_pref_suff works properly", {
  ps <- list(pref = "a", suff = "b")
  expect_equal(paste_pref_suff(ps, notation = arrow_notation()), "a -> b")
  # Make sure that they are the inverse of each other
  expect_equal(paste_pref_suff(ps, notation = arrow_notation()) %>% 
                 split_pref_suff(notation = arrow_notation()), ps)
  # Try with paren notation list
  expect_equal(paste_pref_suff(ps, notation = bracket_notation()) %>% 
                 split_pref_suff(notation = bracket_notation()), ps)
  # Try with a wacky notation list
  amp_nl <- notation_vec(sep = "&&&&&&&&")
  expect_equal(paste_pref_suff(ps, notation = amp_nl) %>% 
                 split_pref_suff(notation = amp_nl), ps)
  paren_nl <- notation_vec(pref_start = "(", pref_end = ")", 
                                    suff_start = "(", suff_end = ")")
  expect_equal(paste_pref_suff(ps, notation = paren_nl) %>% 
                 split_pref_suff(notation = paren_nl), ps)
  # Try to join lists
  expect_equal(paste_pref_suff(list(list(pref = "a", suff = "b"), list(pref = "c", suff = "d"))), list("a -> b", "c -> d"))
  # Try to split then join lists
  joined <- list("a -> b", "c -> d")
  expect_equal(split_pref_suff(joined, notation = arrow_notation()) %>% 
                 paste_pref_suff(notation = arrow_notation()), 
               joined)
  
  # Try with lists in the pref and suff arguments.
  expect_equal(paste_pref_suff(pref = "a", suff = "b", notation = arrow_notation()), "a -> b")
  joined <- paste_pref_suff(pref = list("a", "c"), suff = list("b", "d"), notation = arrow_notation())
  expect_equal(joined, c("a -> b","c -> d"))
})


test_that("flip_pref_suff works as expected", {
  expect_equal(flip_pref_suff("a -> b", notation = arrow_notation()), "b -> a")
  expect_equal(flip_pref_suff("a [b]", notation = bracket_notation()), "b [a]")
  
  # Make sure it works for lists
  expect_equal(flip_pref_suff(list("a -> b", "a -> b"), notation = arrow_notation()), 
               list("b -> a", "b -> a"))
  expect_equal(flip_pref_suff(list("a [b]", "a [b]"), notation = bracket_notation()), 
               list("b [a]", "b [a]"))
  
  # Try a case where prefix and suffix notation is different.
  nl <- notation_vec(pref_start = "(", pref_end = ")", suff_start = "[", suff_end = "]")
  expect_equal(flip_pref_suff("(a)[b]", notation = nl), "(b)[a]")

  # Try with nested suffixes
  expect_equal(flip_pref_suff("a [b [c]]", notation = bracket_notation()), "b [c] [a]")  
})


test_that("switch_notation works as expected", {
  # Start with a degenerate case
  expect_equal(switch_notation("a", from = arrow_notation(), to = bracket_notation()), "a")
  expect_equal(switch_notation("a", from = bracket_notation(), to = arrow_notation()), "a")
  
  # Now try "real" cases
  expect_equal(switch_notation("a -> b", from = arrow_notation(), to = bracket_notation()), 
               "a [b]")
  expect_equal(switch_notation("a -> b", from = arrow_notation(), to = bracket_notation(), flip = TRUE), 
               "b [a]")
  expect_equal(switch_notation("a [b]", from = bracket_notation(), to = arrow_notation()), 
               "a -> b")
  expect_equal(switch_notation("a [b]", from = bracket_notation(), to = arrow_notation(), flip = TRUE), 
               "b -> a")

  # Try with a list
  expect_equal(switch_notation(list("a -> b", "c -> d"), from = arrow_notation(), to = bracket_notation()), 
               list("a [b]", "c [d]"))
  expect_equal(switch_notation(list("a -> b", "c -> d"), from = arrow_notation(), to = bracket_notation(), flip = TRUE), 
               list("b [a]", "d [c]"))
  expect_equal(switch_notation(list("a [b]", "c [d]"), from = bracket_notation(), to = arrow_notation()), 
               list("a -> b", "c -> d"))
})


test_that("switch_notation_byname works as expected", {
  # Switch row names
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a [b]", "c [d]"), c("e", "f"))) %>% 
    setrowtype("Industries [Products]") %>% setcoltype("cols")
  e <- m
  rownames(e) <- c("a -> b", "c -> d")
  e <- e %>% setrowtype("Industries -> Products")
  expect_equal(switch_notation_byname(m, margin = 1, from = bracket_notation(), to = arrow_notation()), e)
  
  # Switch column names
  m2 <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e2 <- m2
  colnames(e2) <- c("d -> c", "f -> e")
  e2 <- e2 %>% setcoltype("Industries -> Products")
  expect_equal(switch_notation_byname(m2, margin = 2, from = bracket_notation(), to = arrow_notation()), e2)

  # Also flip the prefix and suffix. Verify that changes are made in the coltype, too.
  m3 <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e3 <- m3
  colnames(e3) <- c("c -> d", "e -> f")
  e3 <- e3 %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m3, from = bracket_notation(), to = arrow_notation(), flip = TRUE), e3)
  
  # Switch both row and column names
  m4 <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("b [a]", "d [c]"), c("f [e]", "h [g]"))) %>% 
    setrowtype("Products [Industries]") %>% setcoltype("Industries [Products]")
  e4 <- m4
  rownames(e4) <- c("a -> b", "c -> d")
  colnames(e4) <- c("e -> f", "g -> h")
  e4 <- e4 %>% setrowtype("Industries -> Products") %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m4, 
                                      from = bracket_notation(), 
                                      to = arrow_notation(), 
                                      flip = TRUE), 
               e4)
  
  # Try with a list
  expect_equal(switch_notation_byname(list(m4, m4), margin = list(c(1, 2)), 
                                              from = list(bracket_notation()), 
                                              to = list(arrow_notation()), 
                                              flip = TRUE), 
               list(e4, e4))
  
  # Try degenerate case of no row and column types.
  m5 <- matrix(c(1, 2, 
                 3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a -> b", "c -> d"), c("c1", "c2")))
  
  expected <- m5
  rownames(expected) <- c("b [a]", "d [c]")
  actual <- switch_notation_byname(m5, from = arrow_notation(), to = bracket_notation(), flip = TRUE)
  expect_equal(rownames(actual), rownames(expected))
  expect_null(rowtype(actual))
  expect_null(coltype(actual))
  expect_equal(actual, expected)
})


test_that("switch_notation_byname works well when flip is a list", {
  m <- matrix(c(1, 2, 
                3, 4), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("a", "b"), c("d [c]", "f [e]"))) %>% 
    setrowtype("rows") %>% setcoltype("Industries [Products]")
  e <- m
  colnames(e) <- c("c -> d", "e -> f")
  e <- e %>% setcoltype("Products -> Industries")
  expect_equal(switch_notation_byname(m, from = bracket_notation(), to = arrow_notation(), flip = list(TRUE)), e)
  
})
