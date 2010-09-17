\name{sfApplyMatrixLB}
\alias{sfApplyMatrixLB}
\title{sfApplyMatrixLB}
\description{Load balanced parallel application of FUN to rows or columns of matrix X }
\usage{
sfApplyMatrixLB(X, MARGIN = 1, FUN, ..., debugSequential = FALSE)
}
\arguments{
  \item{X}{matrix with rows corresponding to cases supplied to first argument of FUN}
  \item{MARGIN}{1 indicates rows, 2 indicates columns}
  \item{FUN}{function to be applied on each row}
  \item{\dots}{further arguments passed to FUN}
  \item{debugSequential}{use apply}
}
\details{if debugSequentail is TRUE, simple apply is used }
\value{a list with result of FUN for each row or columns of X respectively}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{sfRemoteWrapper}}}

