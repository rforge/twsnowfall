\name{twApply3DMesh}
\alias{twApply3DMesh}
\title{twApply3DMesh}
\description{Applying FUN over cube-grid of x,y,z values}
\usage{
twApply3DMesh(x, y = NULL, z = NULL, FUN = "+", argsFUN = list(), dims = 5, knotSpacing = c(quantile = "quantile", all = "all", equidistant = "equidistant"), nSample = 0, ...)
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
  \item{knotSpacing}{method for calulating the knots.\describe{
\item{quantile}{\code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points (default)}
\item{all}{take all the provided xyz coordinates (overwrites nKnots)}
\item{equidistant}{cover the range of dimension i by \code{dims[i]} equidistant points}
}}
  \item{nSample}{number of points to sample from xyz in addition to grid
Results will be provided in dataframe of four coloumns with attribute "sample".}
  \item{\dots}{further arguments passed to FUN}
}

\value{three dimensional array of class twApply3DMesh with results of calling FUN. Attribute dimnames holds the mesh of respective arguments.}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{plot.twApply3DMesh}}

\code{\link{twPairs}}, \link{twMisc}}

