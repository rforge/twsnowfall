\name{twPlot2DFun}
\alias{twPlot2DFun}
\title{twPlot2DFun}
\description{Applying FUN over grid of x and y values plot as an image.}
\usage{
twPlot2DFun(x, y = NULL, FUN = "+", argsFUN = list(), xdiv = 20, ydiv = xdiv, xlab = NULL, ylab = NULL, col = heat.colors(20), key.title, key.axes, axes = TRUE, las = 1, contour = FALSE, ...)
}
\arguments{
  \item{x}{range of x and y ordinate see \code{\link{xy.coords}}}
  \item{y}{
}
  \item{FUN}{function and further arguments to be applied, first two arguemnts must be numeric vectors}
  \item{argsFUN}{
}
  \item{xdiv}{scalars: number of points, set to NULL to use original x and y instead over a grid across the range}
  \item{ydiv}{
}
  \item{xlab}{labels, default to variable names or column names in x}
  \item{ylab}{
}
  \item{col}{
}
  \item{key.title}{
}
  \item{key.axes}{
}
  \item{axes}{
}
  \item{las}{
}
  \item{contour}{
}
  \item{\dots}{further arguments passed to \code{\link{filled.contour}}}
}



\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





