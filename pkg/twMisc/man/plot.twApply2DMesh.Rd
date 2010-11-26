\name{plot.twApply2DMesh}
\alias{plot.twApply2DMesh}
\title{plot twApply2DMesh}
\description{Creating an image or contour plot of a three-dimensional array.}
\usage{
plot.twApply2DMesh(x, ...)
}
\arguments{
  \item{x}{object of class twApply2DMesh, a result of \code{\link{twApply2DMesh}}}
  \item{\dots}{further arguments passed to \code{\link{twPlot2D}}}
}



\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{twPairs}}, \link{twMisc}}
\examples{
#Example: Nested contours of mixture of three tri-variate normal densities
nmix3 <- function(x, y, m, s) {
	0.4 * dnorm(x, m, s) * dnorm(y, m, s)  +
		0.3 * dnorm(x, -m, s) * dnorm(y, -m, s)  +
		0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) 
}
f <- function(x,y) nmix3(x,y,.5,.5)

n <- 50
x <- rnorm(n,.5,.7)
y <- rnorm(n,.5,.8)
#mtrace(twApply2DMesh)
#mtrace(twPlot2DFun)
plot( tmp <- twApply2DMesh(x,y,f,dims=30))
plot( tmp, contour=TRUE, zlab="density")
}
