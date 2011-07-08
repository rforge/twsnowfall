\name{twApply3DMesh}
\alias{twApply3DMesh}
\title{twApply3DMesh}
\description{Applying FUN over cube-grid of x,y,z values}
\usage{
twApply3DMesh(x, y = NULL, z = NULL, FUN = "+", argsFUN = list(), dims = 5, knotSpacing = c(quantile = "quantile", midquantile = "midquantile", all = "all", equidistant = "equidistant"), nSample = 0, label = deparse(substitute(FUN)), ...)
}
\arguments{
  \item{x}{range of x,y and z ordinate see \code{\link{xyz.coords}}}
  \item{y}{
}
  \item{z}{
}
  \item{FUN}{
}
  \item{argsFUN}{
}
  \item{dims}{integer vector of length 1 or 3: number of points}
  \item{knotSpacing}{method for calulating the knots\describe{
\item{quantile}{\code{\link{cutQuantiles}} for breaks of intervals holding about equal number of points, includes edges (default)}
\item{midquantile}{\code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points, by excluding the edges the sample is represented better}
\item{all}{take all the provided xyz coordinates (overwrites nKnots)}
\item{equidistant}{cover the range of dimension i by \code{nKnots} equidistant points}
}}
  \item{nSample}{number of points to sample from xyz in addition to grid
Results will be provided in dataframe of four coloumns with attribute "sample".}
  \item{label}{label of the z-variable, stored in attribute label}
  \item{\dots}{further arguments passed to FUN}
}
\details{note that knotSpacing default is "quantile", which differs from the 2D version
the edges are not representative of the sample, but the grid spans the full space of the sample}
\value{list of class twApply2DMesh with itmes \itemize{
\item mesh:matrix with each row one coordinate and two columns corresponding to x and y 
\item fval: the two dimensional array of evaluated function values
\item sample: data frame with four columns corresponding to points additional to the grid
\item label: argument label describing fval
\item rangeOrig: list with items x, and y with the range of the original sample
}}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.twApply3DMesh}}

\code{\link{twPairs}}, \link{twMisc}}

