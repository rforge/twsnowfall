\name{plot.twApply2DMesh}
\alias{plot.twApply2DMesh}
\title{plot twApply2DMesh}
\description{Creating an image or contour plot of a three-dimensional array.}
\usage{
plot.twApply2DMesh(x, xlab = NULL, ylab = NULL, zlab = NULL, key.title, key.axes, axes = TRUE, las = 1, contour = FALSE, col = rev(heat.colors(20)), color.palette = function(n) {
    rev(heat.colors(n))
}, ...)
}
\arguments{
  \item{x}{object of class twApply2DMesh, a result of \code{\link{twApply2DMesh}}}
  \item{xlab}{labels, default to variable names or column names in x}
  \item{ylab}{
}
  \item{zlab}{
}
  \item{key.title}{see \code{\link{filled.contour}}}
  \item{key.axes}{
}
  \item{axes}{
}
  \item{las}{
}
  \item{contour}{if TRUE then \code{\link{filled.contour}} is used for plotting. Otherwisee \code{\link{image}}}
  \item{col}{colors for using image}
  \item{color.palette}{colors for using filled.contour}
  \item{\dots}{further arguments passed to \code{\link{filled.contour}} or \code{\link{image}}}
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
