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
\examples{
#Example: Nested contours of mixture of three tri-variate normal densities
nmix3 <- function(x, y, z, m, s) {
	0.4 * dnorm(x, m, s) * dnorm(y, m, s) * dnorm(z, m, s) +
	0.3 * dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
	0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) * dnorm(z, m, s)
}
f <- function(x,y,z) nmix3(x,y,z,.5,.5)

n <- 250
x <- rnorm(n,.5,.5)
y <- c(rnorm(n/2,.5,.5), rnorm(n/2,-.5,.5)) 
zz <- rnorm(n,.5,.5)

#mtrace(twApply3DMesh)
par3d(r3dDefaults[-match(c("bg","material"),names(r3dDefaults))])

plot(tmp <- twApply3DMesh(x,y,zz,f, nSample=200, dims=10))	# just the points
#mtrace(plot.twApply3DMesh)
plot( tmp, col=rev(heat.colors(22))[-(1:5)]) # avoiding near white colors
plot( tmp, levels=seq(0.05, 0.14, len=3) )	# specifying contour levels directly at function value scale 
plot( tmp, probs=seq(0.5, 0.95, len=4), nDrawPoints=0)		# specifying quantiles of FUN results
}
