\name{byname-package}
\alias{byname-package}
\alias{byname}
\docType{package}
\title{
\packageTitle{byname}
}
\description{
\packageDescription{byname}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{byname}
\packageIndices{byname}
}
\author{
\packageAuthor{byname}

\strong{Maintainer}: \packageMaintainer{byname}
}
\keyword{
array
}
\seealso{
The
\code{\link[matsindf:matsindf-package]{matsindf}}
package is useful to create data frames containing 
columns of matrices.
Functions in this package
(\code{\link[byname:byname-package]{byname}}) 
can operate on data frames created by 
\code{\link[matsindf:matsindf-package]{matsindf}}.
}
\examples{
library(magrittr)
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, 
            dimnames = list(productnames, 
                            industrynames)) \%>\%
            setrowtype("Commodities") \%>\% 
            setcoltype("Industries")
G <- matrix(1:4, ncol = 2, 
            dimnames = list(rev(productnames), 
                            rev(industrynames))) \%>\%
            setrowtype("Commodities") \%>\% 
            setcoltype("Industries")
# Non-sensical.  Row and column names not respected.
U + G 
# Row and column names respected.
sum_byname(U, G)
}
