\name{cooksDistance.loess}
\alias{cooksDistance.loess}
\title{cooksDistance loess}
\description{calculate cooksDistance for loess models}
\usage{
cooksDistance.loess(formula, data, ...)
}
\arguments{
  \item{formula}{parameters for \code{\link{loess}}}
  \item{data}{
}
  \item{\dots}{
}
}

\value{list with elements
\item{mod}{the fitted loess model}
\item{cdist}{numeric vector of cooks distance}}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




\examples{
tmp <- seq(1,350,length.out=9)
nPar <-  
data <- data.frame( 
	time = tmp
	,fluo = log(tmp) + rnorm(length(tmp), sd=max(log(tmp)/5))
)
plot(fluo ~ time, data)
res <- cooksDistance.loess( fluo ~ time, data, span=0.9)
plot( res ~ time, dss)
}
