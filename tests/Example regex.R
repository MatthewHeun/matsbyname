x <- c("a1", "a2", "b1", "b2", "sd;lfkasjdl;fkj a1as;dkfsa;ldkfja2")

grepl(pattern = "a1$|^a2", x = x)

grepl(pattern = "\\<a1", x = x)

grepl(pattern = "^a1", x = x)

grepl(pattern = "^a1$|^a2$", x = x)

# Construct pattern from ^ and $

# If we want to match whole names, prepend ^ and append $ to all names.

# If we want to match at beginnings, only prepend ^ (don't append $)

# If we want to match anywhere, don't add ^ or $.

# Then, paste0 with sep = "|" to form the pattern

# Then, grepl on the vector of row or column names


