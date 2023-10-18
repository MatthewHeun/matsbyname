## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(magrittr)
library(RCLabels)

## -----------------------------------------------------------------------------
# Create a notation object.
my_notation <- notation_vec(pref_start = "(", pref_end = ") ",
                            suff_start = "[", suff_end = "]")

# Notation objects are character vectors.
my_notation

## -----------------------------------------------------------------------------
arrow_notation
paren_notation
bracket_notation
first_dot_notation
from_notation
of_notation
to_notation
bracket_arrow_notation

## -----------------------------------------------------------------------------
my_label <- paste_pref_suff(pref = "Coal", suff = "from Coal mines in USA", 
                            notation = my_notation)
my_label

## -----------------------------------------------------------------------------
# Split the prefix from the suffix to obtain a named list of strings.
split_pref_suff(my_label, notation = my_notation)

# Flip the prefix and suffix, maintaining the same notation.
flip_pref_suff(my_label, notation = my_notation)

# Change the notation.
switch_notation(my_label, from = my_notation, to = paren_notation)

# Change the notation and flip the prefix and suffix.
switch_notation(my_label, from = my_notation, to = paren_notation, flip = TRUE)

## -----------------------------------------------------------------------------
get_pref_suff(my_label, which = "pref", notation = my_notation)
get_pref_suff(my_label, which = "suff", notation = my_notation)

## -----------------------------------------------------------------------------
labels <- c("a [of b in c]", "d [of e in f]", "g [of h in i]")
labels

split_pref_suff(labels, notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

df <- tibble::tibble(labels = labels)
result <- df %>% 
  dplyr::mutate(
    split = split_pref_suff(labels, notation = bracket_notation, transpose = TRUE)
  )
result$split[[1]]
result$split[[2]]
result$split[[3]]

## -----------------------------------------------------------------------------
prepositions_list

## -----------------------------------------------------------------------------
labels

# Extract the nouns.
get_nouns(labels, notation = bracket_notation)

# Extract the prepositional phrases.
get_pps(labels, notation = bracket_notation)

# Extract the prepositions themselves.
get_prepositions(labels, notation = bracket_notation)

# Extract the objects of the prepositions.
# Objects are named by the preposition of their phrase.
get_objects(labels, notation = bracket_notation)

# The get_piece() function is a convenience function
# that extracts just what you want.
get_piece(labels, piece = "noun", notation = bracket_notation)
get_piece(labels, piece = "pref")
get_piece(labels, piece = "suff")
get_piece(labels, piece = "of")
get_piece(labels, piece = "in")
# An empty string is returned when the preposition is missing.
get_piece(labels, piece = "bogus")

## -----------------------------------------------------------------------------
labels
# Split the labels into pieces, named by "noun" and prepositions.
split_labels <- split_noun_pp(labels, 
                             prepositions = prepositions_list, 
                             notation = bracket_notation)
split_labels

# Recombine split labels.
paste_noun_pp(split_labels, notation = bracket_notation)

# Recombine with a new notation.
paste_noun_pp(split_labels, notation = paren_notation)

## -----------------------------------------------------------------------------
labels

# Set new values for nouns.
modify_nouns(labels, 
             new_nouns = c("Coal", "Oil", "Natural gas"), 
             notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

# Change nouns in several labels to "Production" and "Manufacture",
# as indicated by the modification map.
modify_label_pieces(labels, 
                    piece = "noun", 
                    mod_map = list(Production = c("a", "b", "c", "d"),
                                   Manufacture = c("g", "h", "i", "j")), 
                    notation = bracket_notation)

# Change the objects of the "in" preposition, 
# according to the modification map.
modify_label_pieces(labels, 
                    piece = "in", 
                    mod_map = list(GHA = "c", ZAF = c("f", "i")), 
                    notation = bracket_notation)

# Change the objects of "of" prepositions,
# according to the modification map.
modify_label_pieces(labels, 
                    piece = "of", 
                    mod_map = list(Coal = "b", `Crude oil` = c("e", "h")), 
                    notation = bracket_notation)

## -----------------------------------------------------------------------------
labels

# Eliminate all of the prepositional phrases that begin with "in".
remove_label_pieces(labels, 
                    piece = "in", 
                    notation = bracket_notation)

# Eliminate all of the prepositional phrases that begin with "of" and "in".
# Note that some spaces remain.
remove_label_pieces(labels, 
                    piece = c("of", "in"), 
                    notation = bracket_notation)

## -----------------------------------------------------------------------------
labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")

# With default `pieces` argument, matching is done for whole labels.
match_by_pattern(labels, regex_pattern = "Production")
match_by_pattern(labels, regex_pattern = "Coal")
match_by_pattern(labels, regex_pattern = "USA")

# Check beginnings of labels: match!
match_by_pattern(labels, regex_pattern = "^Production")
# Check at ends of labels: no match!
match_by_pattern(labels, regex_pattern = "Production$")

# Search by prefix or suffix.
match_by_pattern(labels, regex_pattern = "Production", pieces = "pref")
match_by_pattern(labels, regex_pattern = "Production", pieces = "suff")
# When pieces is "pref" or "suff", only one can be specified.
# The following function call gives an error.
# match_by_pattern(labels, regex_pattern = "Production", pieces = c("pref", "to"))

# Search by noun or preposition.
match_by_pattern(labels, regex_pattern = "Production", pieces = "noun")
match_by_pattern(labels, regex_pattern = "Production", pieces = "in")
# Searching can be done with complicated regex patterns.
match_by_pattern(labels, 
                 regex_pattern = make_or_pattern(c("c", "f")),
                 pieces = "in")
match_by_pattern(labels,
                 regex_pattern = make_or_pattern(c("b", "Coal", "USA")),
                 pieces = "in")
match_by_pattern(labels,
                 regex_pattern = make_or_pattern(c("b", "Coal", "USA")),
                 pieces = c("of", "in"))
# Works with custom lists of prepositions.
match_by_pattern(labels,
                 regex_pattern = make_or_pattern(c("b", "Coal", "GBR", "USA")),
                 pieces = c("noun", "of", "in", "to"),
                 prepositions = c("of", "to", "in"))

## -----------------------------------------------------------------------------
labels <- c("Production [of b in c]", "d [of Coal in f]", "g [of h in USA]")
labels

# If `pieces = "all"` (the default), the entire label is available for replacements.
replace_by_pattern(labels,
                   regex_pattern = "Production",
                   replacement = "Manufacture")
replace_by_pattern(labels,
                   regex_pattern = "Coal",
                   replacement = "Oil")
replace_by_pattern(labels,
                   regex_pattern = "USA",
                   replacement = "GHA")

# Replace by prefix and suffix.
replace_by_pattern(labels,
                   regex_pattern = "Production",
                   replacement = "Manufacture",
                   pieces = "pref")
replace_by_pattern(labels,
                   regex_pattern = "Coa",
                   replacement = "Bow",
                   pieces = "suff")
# Nothing should change, because USA is in the suffix.
replace_by_pattern(labels,
                   regex_pattern = "SA",
                   replacement = "SSR",
                   pieces = "pref")
# Now USA --> USSR, because USA is in the suffix.
replace_by_pattern(labels,
                   regex_pattern = "SA",
                   replacement = "SSR",
                   pieces = "suff")
# This will throw an error, because only "pref" or "suff" can be specified.
# replace_by_pattern(labels,
#                    regex_pattern = "SA",
#                    replacement = "SSR",
#                    pieces = c("pref", "suff")

# Replace by noun or preposition.
replace_by_pattern(labels,
                   regex_pattern = "Production",
                   replacement = "Manufacture",
                   pieces = "noun")
replace_by_pattern(labels,
                   regex_pattern = "^Pro",
                   replacement = "Con",
                   pieces = "noun")
# Won't match: wrong side of string.
replace_by_pattern(labels,
                   regex_pattern = "Pro$",
                   replacement = "Con",
                   pieces = "noun")
# No change, because "Production" is a noun.
replace_by_pattern(labels,
                   regex_pattern = "Production",
                   replacement = "Manufacture",
                   pieces = "of")
# Now try with "of".
replace_by_pattern(labels,
                   regex_pattern = "Coal",
                   replacement = "Oil",
                   pieces = "of")
# No change, because "Coal" is not "in" anything.
replace_by_pattern(labels,
                   regex_pattern = "Coal",
                   replacement = "Oil",
                   pieces = "in")
# Now try in "in".
replace_by_pattern(labels,
                   regex_pattern = "USA",
                   replacement = "GBR",
                   pieces = "in")
replace_by_pattern(labels,
                   regex_pattern = "A$",
                   replacement = "upercalifragilisticexpialidocious",
                   pieces = "in")


