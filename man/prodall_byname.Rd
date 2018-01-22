% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Byname.R
\name{prodall_byname}
\alias{prodall_byname}
\title{Product of all elements in a matrix}
\usage{
prodall_byname(m)
}
\arguments{
\item{m}{the matrix whose elements are to be multiplied}
}
\value{
the product of all elements in \code{m} as a numeric.
}
\description{
This function is equivalent to \code{m \%>\% rowprods_byname() \%>\% colprods_byname()},
but returns a single numeric value instead of a 1x1 matrix.
}
\examples{
library(magrittr)
library(dplyr)
m <- matrix(2, nrow=2, ncol=2, dimnames = list(paste0("i", 1:2), paste0("c", 1:2))) \%>\%
  setrowtype("Industry") \%>\% setcoltype("Product")
prodall_byname(m)
rowprods_byname(m) \%>\% colprods_byname
# Also works for lists
prodall_byname(list(m,m))
DF <- data.frame(m = I(list()))
DF[[1,"m"]] <- m
DF[[2,"m"]] <- m
prodall_byname(DF$m[[1]])
prodall_byname(DF$m)
res <- DF \%>\% mutate(
  prods = prodall_byname(m)
)
res$prods
}