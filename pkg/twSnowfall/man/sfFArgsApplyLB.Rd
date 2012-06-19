\name{sfFArgsApplyLB}
\alias{sfFArgsApplyLB}
\title{sfFArgsApplyLB}
\description{Load balanced application of F_APPLY to arguments supplied by function F_ARGS(i).}
\usage{
sfFArgsApplyLB(N_APPLYCASES,
F_ARGS,
F_APPLY,
...,
SFFARGSAPPLY_ADDARGS = list(),
debugSequential = FALSE)
}
\arguments{
  \item{N_APPLYCASES}{i=1..N_APPLYCASES}
  \item{F_ARGS}{function(i) returning a list of first arguments to F_APPLY}
  \item{F_APPLY}{function to be applied}
  \item{\dots}{further arguments passed to F_APPLY}
  \item{SFFARGSAPPLY_ADDARGS}{further arguments passed to F_APPLY}
  \item{debugSequential}{evaluate using lapply instead of dynamicClusterApply}
}

\value{a list of results of F_APPLY}

\author{Thomas Wutzler}



\seealso{\code{\link{twSnowfall}}}
\examples{
#sfInit(parallel=TRUE,cpus=2)

X <- matrix(0:9,nrow=5,ncol=2)
dimnames(X) <- list( rows=paste("r",1:nrow(X),sep=""), cols=paste("c",1:ncol(X),sep="")  )
Y <- X*10
# delivering row i of X and row i of Y as arguments to F_Apply
F_ARGS <- function(i){list(arg1=X[i,],arg2=Y[i,])}
F_APPLY <- paste
.res <- sfFArgsApplyLB( nrow(X), F_ARGS, F_APPLY, sep="-")
}
