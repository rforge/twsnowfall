\name{twIntegrateTS}
\alias{twIntegrateTS}
\title{twIntegrateTS}
\description{Integrate dy between xl and xr.}
\usage{
twIntegrateTS(dy,
x,
xl = min(x,
na.rm = TRUE),
xr = max(x,
na.rm = TRUE),
na.rm = TRUE,
doSort = TRUE)
}
\arguments{
  \item{dy}{rate}
  \item{x}{times, rate dy refers to interval (x[i],x[i+1]), see \code{\link{twCalcObsScale}} to translate to this observation scale}
  \item{xl}{left edge of integration}
  \item{xr}{right edge of integration}
  \item{na.rm}{can be set to FALSE for performance reasons}
  \item{doSort}{can be set to FALSE for performance reasons}
}

\value{Each dy_i is giving the slope for succeeding time interval x_i to x_i+1
result vector is of length( sum(is.na(x)) )-1}

\author{Thomas Wutzler}





