\name{plot.twApply3DMesh}
\alias{plot.twApply3DMesh}
\alias{twMiscRgl}
\title{plot twApply3DMesh}
\description{Creates an rgl scene with contours from calculated mesh.}
\usage{
plot.twApply3DMesh(x, probs = NULL, levels = NULL, xlab = NULL, ylab = NULL, zlab = NULL, sample = attr(x, "sample"), nDrawPoints = if (0 < length(sample)) nrow(sample) else 200, col = rev(heat.colors(100)), alo = 0.1, ahi = 0.5, cmap = heat.colors, box = TRUE, axes = TRUE, ...)
}
\arguments{
  \item{x}{results of \code{\link{twApply3DMesh}}}
  \item{probs}{numeric vector: levels will be calculated with quantiles of the results of the sample}
  \item{levels}{numeric vector: values of FUN return values where contour surface should be drawn}
  \item{xlab}{labels, default to variable names or column names in x}
  \item{ylab}{
}
  \item{zlab}{
}
  \item{sample}{dataframe of four columns: sample points to draw}
  \item{nDrawPoints}{number of points in xyz to draw. Set to 0 if no points should be drawn}
  \item{col}{colour of the points}
  \item{alo}{minimum and maximum transparency for contour levels}
  \item{ahi}{
}
  \item{cmap}{
}
  \item{box}{whether to draw box and axes}
  \item{axes}{
}
  \item{\dots}{further arguments to \code{\link{plot3d}}}
}
\details{\describe{\item{Further plotting Functionality of package twMiscRgl}{
\itemize{
\item 3D scatter and contour plots: this method 
}
}}

If argument \code{level} is supplied, the argument \code{probs} is ignored. 
Together with a small \code{nDrawPoints} this saves calculation time.
If a sample was provided the quantiles of surface levels are calculated from
the sample, otherwise from the mesh x.}
\value{If contourLevles was given then the 3D array of function evaluations }

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




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

plot(tmp <- twApply3DMesh(x,y,zz,f, nSample=200, dims=10))	# just the points
#mtrace(plot.twApply3DMesh)
plot( tmp, col=rev(heat.colors(22))[-(1:5)]) # avoiding near white colors
plot( tmp, levels=seq(0.05, 0.14, len=3) )	# specifying contour levels directly at function value scale 
plot( tmp, probs=seq(0.5, 0.95, len=4), nDrawPoints=0)		# specifying quantiles of FUN results
}
