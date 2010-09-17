\name{twDiffTS}
\alias{twDiffTS}
\title{twDiffTS}
\description{Approximation of the derivative dy/dx }
\usage{
twDiffTS(y, x, na.rm = TRUE, doSort = TRUE)
}
\arguments{
  \item{y}{
}
  \item{x}{
}
  \item{na.rm}{can be set to FALSE for performance reasons}
  \item{doSort}{can be set to FALSE for performance reasons}
}

\value{numeric vector of length( x[!is.na(x)] ) 
with two sided difference approximation (y_+1 - y_-1)/(x_+1 - x_-1)
and single sided approximations at the edges}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





