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

Maintainer: \packageMaintainer{byname}
}
\references{
}
\keyword{
}
\seealso{
The
\code{\link[byname:byname-package]{byname}} 
package is used extensively by the
\code{\link[matsindf:matsindf-package]{matsindf}}
package.
}
\examples{
productnames <- c("p1", "p2")
industrynames <- c("i1", "i2")
U <- matrix(1:4, ncol = 2, 
            dimnames = list(productnames, 
                            industrynames)) \%>\%
            setrowtype("Commodities") \%>\% 
            setcoltype("Industries")
G <- matrix(1:4, ncol = 2, 
            dimnames = list(rev(commoditynames), 
                            rev(industrynames))) \%>\%
            setrowtype("Commodities") \%>\% 
            setcoltype("Industries")
# Non-sensical.  Row and column names not respected.
U + G 
# Row and column names respected.
sum_byname(U, G)
}
