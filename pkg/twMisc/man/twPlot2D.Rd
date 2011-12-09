\name{twPlot2D}
\alias{twPlot2D}
\title{twPlot2D}
\description{Creating an image or contour plot of a three-dimensional array.}
\usage{
twPlot2D(x,
y = NULL,
z = NULL,
xlab = NULL,
ylab = NULL,
zlab = NULL,
key.title,
key.axes,
axes = TRUE,
las = 1,
contour = FALSE,
col = rev(heat.colors(20)),
color.palette = function(n) {
    rev(heat.colors(n))
}, ...)
}
\arguments{
  \item{x}{locations of grid lines at which the values in z are measured. These must be in ascending order. By default, equally spaced values from 0 to 1 are used. If x is a list, its components x$x and x$y are used for x and y, respectively. If the list has component z this is used for z.}
  \item{y}{
}
  \item{z}{a matrix containing the values to be plotted (NAs are allowed). Note that x can be used instead of z for convenience.}
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



\author{Thomas Wutzler}



\seealso{\code{\link{twPairs}}, \link{twMisc}}
\examples{
	nmix3 <- function(x, y, m, s) {
		0.4 * dnorm(x, m, s) * dnorm(y, m, s)  +
			0.3 * dnorm(x, -m, s) * dnorm(y, -m, s)  +
			0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) 
	}
	f <- function(x,y) nmix3(x,y,.5,.5)
	
	n <- 30
	x <- sort(rnorm(n,.5,.7))
	y <- sort(rnorm(n,.5,.8))
#mtrace(twApply2DMesh)
#mtrace(twPlot2DFun)
	tmp <- twApply2DMesh(x,y,f,knotSpacing="all")
	twPlot2D( x,y,tmp,zlab="density")
	
}
