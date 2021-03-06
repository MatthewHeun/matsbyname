% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Utilities.R
\name{setcolnames_byname}
\alias{setcolnames_byname}
\title{Sets column names}
\usage{
setcolnames_byname(a, colnames)
}
\arguments{
\item{a}{A matrix or a list of matrices in which column names are to be set}

\item{colnames}{A vector of new column names or a list of vectors of new column names}
}
\value{
a copy of \code{a} with new column names
}
\description{
Sets column names in a way that is amenable to use in piping operations in a functional programming way.
if \code{a} is \code{NULL}, \code{NULL} is returned.
If \code{a} is a constant, it is converted to a matrix and \code{colnames} are applied.
If \code{a} is a matrix, \code{colnames} should be a vector of new column names
that is as long as the number of columns in \code{a}.
If \code{a} is a list of matrices,
\code{colnames} can also be a list, and it should be as long as \code{a}.
Or \code{colnames} can be a vector of column names which will be applied to every matrix in
the list of \code{a}.
Each item in the list should be a vector containing column names for the corresponding
matrix in \code{a}.
}
\examples{
m <- matrix(c(1:6), nrow = 2, dimnames = list(paste0("i", 1:2), paste0("c", 1:3))) \%>\%
  setrowtype("Industries") \%>\% setcoltype("Commodities")
setcolnames_byname(m, c("a", "b", "c"))
}
