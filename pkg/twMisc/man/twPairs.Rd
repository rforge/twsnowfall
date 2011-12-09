\name{twPairs}
\alias{twPairs}
\title{twPairs}
\description{Pairs-plot with scatterplots in upper part and histogram in diagonal}
\usage{
twPairs(x,
nsample = 200,
...)
}
\arguments{
  \item{x}{numeric matrix}
  \item{nsample}{maximum number of points in scatterplot, set to NULL to show all rows}
  \item{\dots}{further arguments to \code{\link{pairs}}}
}
\details{\describe{\item{Further plotting Functionality of package twMisc}{
\itemize{
\item Pairs-plot with scatterplots in upper part and histogram in diagonal: this method 
\item 2D image and contour plots: \code{\link{plot.twApply2DMesh}}
\item 3D scatter and contour plots: \code{plot.twApply3DMesh} has been moved to package twMiscRgl because of package dependencies. 
\item TODO: link functions: \code{\link{twDf2wikiTable}} 
}
}}

based on pairs.modCost from package FME}


\author{Thomas Wutzler}



\seealso{\link{twMisc}}

