# Contains tests for notation functions in the byname package.

###########################################################
context("Arrow notation")
###########################################################

test_that("arrow notation is created properly", {
  an <- arrow_notation_list()
  expect_null(an$pref_start)
  expect_equal(an$pref_end, " -> ")
  expect_equal(an$suff_start, " -> ")
  expect_null(an$suff_end)
})


test_that("paren notation is created properly", {
  pn <- paren_notation_list()
  expect_null(pn$pref_start)
  expect_equal(pn$pref_end, " [")
  expect_equal(pn$suff_start, " [")
  expect_equal(pn$suff_end, "]")
})


test_that("split_pref_suff works properly", {
  expect_equal(split_pref_suff("a -> b", notation_list = arrow_notation_list()), list(pref = "a", suff = "b"))
  expect_equal(split_pref_suff("b [a]", notation_list = paren_notation_list()), list(pref = "b", suff = "a"))
  
  # See if it works with a vector of strings
  expect_equal(split_pref_suff(c("a -> b", "a -> b"), notation_list = arrow_notation_list()), 
               list(list(pref = "a", suff = "b"), list(pref = "a", suff = "b")))
  # See if it works with a list of strings
  expect_equal(split_pref_suff(list("a -> b", "a -> b"), notation_list = arrow_notation_list()), 
               list(list(pref = "a", suff = "b"), list(pref = "a", suff = "b")))
  
  # Try with unusual prefixes and suffixes
  nl <- row_col_notation_list(pref_start = " {", pref_end = "} ", suff_start = "} ", suff_end = NULL)
  expect_equal(split_pref_suff(" {a} bcd", notation_list = nl), list(pref = "a", suff = "bcd"))

  nl2 <- row_col_notation_list(pref_start = "  {", pref_end = "} ", suff_start = "[ ", suff_end = "]  ")
  expect_equal(split_pref_suff("  {a} [ b]  ", notation_list = nl2), list(pref = "a", suff = "b"))
  
  expect_equal(split_pref_suff("a [ [b]]", notation_list = paren_notation_list()), list(pref = "a", suff = " [b]"))
  
  # Try with degenerate cases
  nl3 <- row_col_notation_list(sep = "{{}}")
  expect_equal(split_pref_suff("abc {{} def", notation_list = nl3), list(pref = "abc {{} def", suff = NULL))
  expect_equal(split_pref_suff("abc {{}} def", notation_list = nl3), list(pref = "abc ", suff = " def"))
  
  # Try with weird parentheses
  nl4 <- row_col_notation_list(pref_start = "(", pref_end = ")", suff_start = "(", suff_end = ")")
  expect_equal(split_pref_suff("(a)(b)", notation_list = nl4), list(pref = "a", suff = "b"))
  
  expect_equal(split_pref_suff("a b", notation_list = nl4), list(pref = "a b", suff = NULL))
})


test_that("join_pref_suff works properly", {
  ps <- list(pref = "a", suff = "b")
  expect_equal(join_pref_suff(ps, notation_list = arrow_notation_list()), "a -> b")
  # Make sure that they are the inverse of each other
  expect_equal(join_pref_suff(ps, notation_list = arrow_notation_list()) %>% 
                 split_pref_suff(notation_list = arrow_notation_list()), ps)
  # Try with paren notation list
  expect_equal(join_pref_suff(ps, notation_list = paren_notation_list()) %>% 
                 split_pref_suff(notation_list = paren_notation_list()), ps)
  # Try with a wacky notation list
  amp_nl <- row_col_notation_list(sep = "&&&&&&&&")
  expect_equal(join_pref_suff(ps, notation_list = amp_nl) %>% 
                 split_pref_suff(notation_list = amp_nl), ps)
  paren_nl <- row_col_notation_list(pref_start = "(", pref_end = ")", 
                                    suff_start = "(", suff_end = ")")
  expect_equal(join_pref_suff(ps, notation_list = paren_nl) %>% 
                 split_pref_suff(notation_list = paren_nl), ps)
  
})


test_that("switch_pref_suff works as expected", {
  expect_equal(switch_pref_suff("a -> b", notation_list = arrow_notation_list()), "b -> a")
  expect_equal(switch_pref_suff("a [b]", notation_list = paren_notation_list()), "b [a]")
  
  # Make sure it works for lists
  expect_equal(switch_pref_suff(list("a -> b", "a -> b"), notation_list = arrow_notation_list()), 
               list("b -> a", "b -> a"))
  expect_equal(switch_pref_suff(list("a [b]", "a [b]"), notation_list = paren_notation_list()), 
               list("b [a]", "b [a]"))
  
  # Try a case where prefix and suffix notation is different.
  nl <- row_col_notation_list(pref_start = "(", pref_end = ")", suff_start = "[", suff_end = "]")
  expect_equal(switch_pref_suff("(a)[b]", notation_list = nl), "(b)[a]")

  # Try with nested suffixes
  expect_equal(switch_pref_suff("a [b [c]]", notation_list = paren_notation_list()), "b [c] [a]")  
})


test_that("switch_row_col_notation works as expected", {
  switch_row_col_notation("a -> b", from = arrow_notation_list(), to = paren_notation_list())
})