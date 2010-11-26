\name{plot.twApply3DMesh}
\alias{plot.twApply3DMesh}
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
\details{If argument \code{level} is supplied, the argument \code{probs} is ignored. 
Together with a small \code{nDrawPoints} this saves calculation time.
If a sample was provided the quantiles of surface levels are calculated from
the sample, otherwise from the mesh x.}
\value{If contourLevles was given then the 3D array of function evaluations }

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{twPairs}}, \link{twMisc}}

