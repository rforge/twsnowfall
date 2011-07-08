\name{sfFArgsApplyDep}
\alias{sfFArgsApplyDep}
\title{sfFArgsApplyDep}
\description{Load balanced application of F_APPLY with using result from a previous run to construct arguments.}
\usage{
sfFArgsApplyDep(N_APPLYCASES, F_ARGS, F_APPLY, SFFARGSAPPLY_initVal, SFFARGSAPPLY_dependsStep = length(SFFARGSAPPLY_initVal), ..., SFFARGSAPPLY_ADDARGS = list(), debugSequential = FALSE)
}
\arguments{
  \item{N_APPLYCASES}{number of cases to calculate}
  \item{F_ARGS}{function returning arguments for case i, see details}
  \item{F_APPLY}{function to calculate}
  \item{SFFARGSAPPLY_initVal}{intial values}
  \item{SFFARGSAPPLY_dependsStep}{dependencies in 1..n,}
  \item{\dots}{further arguments passed to F_APPLY}
  \item{SFFARGSAPPLY_ADDARGS}{results presented to argfun for the first 1..dependsStep results}
  \item{debugSequential}{the number of processors (might be smaller than cluster to assure that previous case i-useNCpus has been evaluated.)}
}



\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{sfRemoteWrapper}}
\code{\link{twDynamicClusterApplyDep}}}
\examples{
#sfInit(parallel=TRUE,cpus=2)

# using as many cpus as rows in Z
(Z<-matrix(letters[1:12],nrow=3))
F_APPLY <- function(x,z) paste(x,z,sep="");
F_ARGS <- function(i,prevRes){list(x=prevRes,z=Z[i])}	
.res0 <- rep("_",nrow(Z))	# dependStep will be length of .res0
resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0)
(res <- matrix(sfSimplifyLBResult(resSeq),nrow=nrow(Z)))

# Gives the same results as having one parallel call per column.
# However, in this implementation, the finished nodes do not need to 
# wait for possibly slower finishing of other rows, and can tackle
# already further columns.
}
