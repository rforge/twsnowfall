\name{parLapplySave}
\alias{parLapplySave}
\title{parLapplySave}
\description{Wrapper around parLapply that calls lapply on improper cluster}
\usage{parLapplySave(cl, ...)}
\arguments{
  \item{cl}{
}
  \item{\dots}{
}
}
\details{lapply is called if \code{\link{isParallel}} returns FALSE}


\author{Thomas Wutzler}



\seealso{\code{\link{isParallel}}, \code{\link{isClusterRunning}}, \code{\link{twMisc}}}
\examples{
# next will work on cluster, if default cluster is defined
# else will use lapply
parLapplySave(NULL, 1:2, function(i){i*2})  
}
