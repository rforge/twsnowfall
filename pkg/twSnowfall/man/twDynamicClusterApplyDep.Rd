\name{twDynamicClusterApplyDep}
\alias{twDynamicClusterApplyDep}
\title{twDynamicClusterApplyDep}
\description{Modification of \code{dynamicClusterApply} (snow-internal) to provide result of previous step.}
\usage{twDynamicClusterApplyDep(cl, fun, n, argfun, dependsStep, initVal = vector("list", 
    dependsStep), freeMasterNode = FALSE)}
\arguments{
  \item{cl}{a function returning a list of arguments passed to fun}
  \item{fun}{
}
  \item{n}{
}
  \item{argfun}{
}
  \item{dependsStep}{dependencies in 1..n}
  \item{initVal}{results presented to argfun for the first 1..dependsStep results}
  \item{freeMasterNode}{if set to TRUE, no job is submitted to node 1, so that this node can dispatch jobs without waiting}
}
\details{\code{argfun=function(i,prevRes) list( <args(i,prevRes)> )} 
provides arguments to fun where \describe{
\item{i}{index for which to generate arguments}
\item{prevRes}{result of FUN for case i-dependsStep.}}
can be assume that prevResList[[i-useNCpus]] has been evaluted already

\code{dependsStep} }
\value{same (snow-internal) \code{dynamicClusterApply}}

\author{Thomas Wutzler}



\seealso{\code{\link{twSnowfall}}}

