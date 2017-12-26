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
    This package has three helpful characteristics:

    \enumerate{
      \item provides and respects names for matrix rows and columns, 
      
      \item provides "types" for data in matrix rows and columns, and
      
      \item works equally well with individual matrices and with lists of matrices.
    }
    
    The corresponding benefits of these characteristics are:
    
    \enumerate{
      \item users of \code{\link[byname:byname-package]{byname}}
            functions do not need to keep track of matrix rows and columns or their order;
            e.g., if two matrices with different row and column names are summed, 
            the result contains 
            named rows, which are the the union of rows of the addend and augend matrices and 
            named columns, which are the the union of columns of the addend and augend matrices,
            with missing rows and columns inserted as zeroes prior to addition.

      \item operations involving wrong "types" of data are flagged as errors;
            e.g., a matrix with rows containing \code{industries} and 
            columns containing \code{products} cannot be added to a matrix
            with rows containing \code{products} and columns containing \code{industries}.

      \item a data frame containing columns of matrices can be manipulated
            as easily as a data frame containing columns of reals with 
            \code{\link[dplyr:dplyr-package]{dplyr}}-style programming
            and \code{\link[magrittr:magrittr-package]{magrittr}}'s
            pipe operator (\code{\%>\%}).
    }
}
\author{
\packageAuthor{byname}

\strong{Maintainer}: \packageMaintainer{byname}
}
\keyword{
array
}
\seealso{
Functions in this package
(\code{\link[byname:byname-package]{byname}}) 
can operate on data frames 
with columns of matrices created by 
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
Y <- matrix(1:4, ncol = 2, 
            dimnames = list(rev(productnames), 
                            rev(industrynames))) \%>\%
            setrowtype("Commodities") \%>\% 
            setcoltype("Industries")
# Non-sensical.  Row and column names not respected.
U + Y
# Row and column names respected.
sum_byname(U, Y)
}
