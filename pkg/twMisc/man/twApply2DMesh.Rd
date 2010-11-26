\name{twApply2DMesh}
\alias{twApply2DMesh}
\title{twApply2DMesh}
\description{Applying FUN over cube-grid of x,y,z values}
\usage{
twApply2DMesh(x, y = NULL, FUN = "+", argsFUN = list(), dims = 20, knotSpacing = c(quantile = "quantile", all = "all", equidistant = "equidistant"), ...)
}
\arguments{
  \item{x}{range of x and y ordinate see \code{\link{xy.coords}}}
  \item{y}{
}
  \item{FUN}{
}
  \item{argsFUN}{
}
  \item{dims}{integer vector of length 1 or 3: number of points}
  \item{knotSpacing}{method of calculating knot positions\describe{
\item{quantile}{\code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points (default)}
\item{all}{take all the provided xyz coordinates (overwrites nKnots)}
\item{equidistant}{cover the range of dimension i by \code{dims[i]} equidistant points}
}}
  \item{\dots}{further arguments passed to FUN}
}

\value{two dimensional array of class twApply2DMesh with results of calling FUN. Attribute dimnames holds the arguments.}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.twApply2DMesh}}

\code{\link{twPairs}}, \link{twMisc}}

