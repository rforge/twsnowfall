\name{twApply2DMesh}
\alias{twApply2DMesh}
\title{twApply2DMesh}
\description{Applying FUN over cube-grid of x,y,z values}
\usage{
twApply2DMesh(x, y = NULL, FUN = "+", argsFUN = list(), dims = 20, knotSpacing = c(midquantile = "midquantile", quantile = "quantile", all = "all", equidistant = "equidistant"), label = deparse(substitute(FUN)), ...)
}
\arguments{
  \item{x}{range of x and y ordinate see \code{\link{xy.coords}}}
  \item{y}{
}
  \item{FUN}{
}
  \item{argsFUN}{
}
  \item{dims}{integer vector of length 1 or 2: number of points}
  \item{knotSpacing}{method for calulating the knots\describe{
\item{midquantile}{\code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points, by excluding the edges the sample is represented better (default)}
\item{quantile}{\code{\link{cutQuantiles}} for breaks of intervals holding about equal number of points, includes edges}
\item{all}{take all the provided xyz coordinates (overwrites nKnots)}
\item{equidistant}{cover the range of dimension i by \code{nKnots} equidistant points}
}}
  \item{label}{
}
  \item{\dots}{further arguments passed to FUN}
}
\details{note that knotSpacing default is "midquantile", so the grid does not cover the full range
but the grid spacing is representative of the marginal distributions}
\value{list of class twApply2DMesh with itmes \itemize{
\item mesh:matrix with each row one coordinate and two columns corresponding to x and y 
\item fval: the two dimensional array of evaluated function values
\item label: argument label describing fval
\item rangeOrig: list with items x, and y with the range of the original sample
}}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.twApply2DMesh}}

\code{\link{twPairs}}, \link{twMisc}}

