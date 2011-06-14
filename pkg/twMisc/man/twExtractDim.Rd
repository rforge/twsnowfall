\name{twExtractDim}
\alias{twExtractDim}
\title{twExtractDim}
\description{Extract A[...,i,...] from the iDim dimension}
\usage{
twExtractDim(A, i = 1, iDim = length(dim(A)))
}
\arguments{
  \item{A}{the array to extract values from}
  \item{i}{the index to extract}
  \item{iDim}{the dimension to extract from}
}
\details{If the size of the array is unknown, then the A[,i,] notation 
is not applicable. One way would construct and parse the R-code.
This function calculates the indices to extract by using \code{\link{outer}}.}
\value{vector representing the }

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





