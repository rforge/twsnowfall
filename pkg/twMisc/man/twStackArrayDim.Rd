\name{twStackArrayDim}
\alias{twStackArrayDim}
\title{twStackArrayDim}
\description{stacks a dimension of an array, i.e. reduces to a lower dimension }
\usage{twStackArrayDim(x, sourceDim = length(dim(x)), destDim = length(dim(x)) - 
    1, sep = "_")}
\arguments{
  \item{x}{the array to process}
  \item{sourceDim}{the dimension that should be stacked}
  \item{destDim}{the dimension, along which to stack, referring to the resulting array}
  \item{sep}{character separating the parts of the new dimension names}
}

\value{Array of one dimension less than x.
Dimnames are concatenaed names of the sourceDim and the original destDim}

\author{Thomas Wutzler}



\seealso{\code{\link{twExtractDim}}, \link{twMisc}}
\examples{
#twStackArrayDim()
}
