\name{twExtractDim}
\alias{twExtractDim}
\title{twExtractDim}
\description{Extract \code{A[...,i,...]} from dimension \code{iDim} of an array }
\usage{twExtractDim(A, i = 1, iDim = length(dim(A)))}
\arguments{
  \item{A}{the array to extract values from}
  \item{i}{the index to extract}
  \item{iDim}{the dimension to extract from}
}
\details{Sometimes the dimension of an array that is to be processed is 
only fixed during runtime, i.e. by a value of a variable.
However, with standard \code{[,,]} extraction the syntax is fixed.

\describe{\item{Array functionality of package \link{twMisc} }{
\itemize{
\item Extract \code{A[...,i,...]} from dimension \code{iDim} of an array: this method
\item Splitting an array by dimesnion and listing subarray: \code{\link{twListArrDim}} 
\item Extracting from last dimensions: \code{\link{twExtractFromLastDims}}
\item Stacking a dimension: \code{\link{twStackArrayDim}}
}
}}

If the dimensionality of the array is unknown, then the A[,i,] notation 
is not applicable. 
This function calculates the indices to extract by using \code{\link{outer}}.}
\value{vector representing the }

\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
A <- array( 1:(4*2*3), dim=c(4,2,3) )
str(A)

# extract from last dimension
(tmp1 <- twExtractDim(A,3))
identical( tmp1, A[,,3] ) 

# extract from second dimension
(tmp2 <- twExtractDim(A,2,iDim=2))
identical( tmp2, A[,2,] )

# list all subarrays of last dimension
(tmp3 <- twListArrDim(A))
identical( structure(abind::abind(tmp3,rev.along=0), dimnames=NULL), A )

# list all subarrays of second dimension
(tmp4 <- twListArrDim(A,2))

twExtractFromLastDims(A, 1:2)
}
