\name{twWhichColsEqual}
\alias{twWhichColsEqual}
\title{twWhichColsEqual}
\description{compares each column of X to column of Z and returns indices of equal columns   }
\usage{
twWhichColsEqual(X, Z = matrix(rep(z, ncol(X)), nrow = length(z)), z = NA)
}
\arguments{
  \item{X}{matrix}
  \item{Z}{Matrix with same numer of rows. }
  \item{z}{alternatively, specify only one vector, which each row of X is compared against}
}
\details{If both vectors contain NA's at the same position the vectors are regarded equal.
This is different from which with ==, where any NA leeds to a FALSE }


\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





