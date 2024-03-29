% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Unary.R
\name{eigenvectors_byname}
\alias{eigenvectors_byname}
\title{Calculate eigenvectors of a matrix}
\usage{
eigenvectors_byname(a)
}
\arguments{
\item{a}{A matrix or list of matrices.}
}
\value{
A matrix whose columns are the eigenvectors of \code{a}.
}
\description{
Calculate the eigenvectors of a matrix or a list of matrices.
}
\details{
This function pairs with \code{eigenvalues_byname()};
the first column of the resulting matrix is the eigenvector
for the first eigenvalue reported by \code{eigenvalues_byname()}.
The second column of the resulting matrix is the eigenvector
for the second eigenvalue reported by \code{eigenvalues_byname()}.
Etc.

Internally, this function uses \code{base::eigen()}.

\code{complete_rows_cols()} is called prior to calculating the eigenvectors.
}
\examples{
m <- matrix(c( 4,  6, 10, 
               3, 10, 13, 
              -2, -6, -8), byrow = TRUE, nrow = 3, ncol = 3, 
            dimnames = list(c("p1", "p2", "p3"), c("p1", "p2", "p3")))
m
eigenvectors_byname(m)
eigenvectors_byname(list(m, 2*m))
DF <- tibble::tibble(m_col = list(m, 2*m)) \%>\% 
  dplyr::mutate(
    eigen_col = eigenvectors_byname(m_col)
  )
DF$eigen_col[[1]]
DF$eigen_col[[2]]
}
