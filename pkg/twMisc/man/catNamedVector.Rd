\name{catNamedVector}
\alias{catNamedVector}
\title{catNamedVector}
\description{print numeric vector in form name=value,name2=value2 }
\usage{catNamedVector(x, digits = 3)}
\arguments{
  \item{x}{numeric vector}
  \item{digits}{number of digits passed to \code{\link{signif}}}
}



\author{Thomas Wutzler}




\examples{
x <- c(a=5, b=pi)
catHeadNamedVector(rep(x,5))
}
