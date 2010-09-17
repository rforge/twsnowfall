\name{sfPar}
\alias{sfPar}
\title{sfPar}
\description{Executing a list of functions distributed via sfApply.}
\usage{
sfPar(fList, sfParParallel = TRUE, sfParArgsList = NULL, ...)
}
\arguments{
  \item{fList}{the list of functions}
  \item{sfParParallel}{if FALSE, then apply is used instead of sfClusterApplyLB - good for debugging}
  \item{sfParArgsList}{an optional list of arguments passed to each function}
  \item{\dots}{passed to each function}
}
\details{If argsList is a name it is evaluated on the node before calling the function
this allows to distribute variables by sfExport before rather than transferring them with each call
which is advantages when the call is issued very often and the arguments are large}
\value{a list of returns for each function}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{sfRemoteWrapper}}}
\examples{
#sfInit(parallel=TRUE,cpus=4)
fList <- list(
	function(...){ "Hello world!" } # dots are important
	,function(a){ a }
)
# each function is executed on a different node
(res <- sfPar( fList, sfParArgsList=list(a="aha") ))	
}
