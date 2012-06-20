\name{sfRemoteWrapper}
\alias{sfRemoteWrapper}
\title{sfRemoteWrapper}
\description{Wrapper for remotely called function supporting error dumps and remotely resolving arguments.}
\usage{sfRemoteWrapper(..., remoteFunArgs = list(), remoteFun = NULL, 
    remoteDumpfileBasename = NULL)}
\arguments{
  \item{\dots}{Further arguments to remoteFun}
  \item{remoteFunArgs}{The Arguments to remoteFun.}
  \item{remoteFun}{The function to be called. Alternatively provided as an entry in remoteFunArgs.}
  \item{remoteDumpfileBasename}{The name of a dump on error. If null, no dump is created. Alternatively provided as an entry in remoteFunArgs.}
}
\details{If remoteFunArgs is a name, it is first evaulated in the parent frame.
This supports the usage of sfExport of the remoteFunArgs instead of passing much data in sfApply via \dots

If remoteFun is NULL, then remoteFunArgs$remoteFun is used, and cleared from remoteFunArgs. 
If dumpFileBaseName is NULL, then remoteFunArgs$dumpFileBaseName is used, and cleared from remoteFunArgs.

If dumpFileBaseName is not null, a dump is created in 
this file when an error occurs.
Trace the error then by \itemize{
\item{\code{load(paste(remoteDumpfileBasename,".rda",sep="")); debugger(get(remoteDumpfileBasename))}}
\item{\code{mtrace(remoteFun); try( eval(body) )}}
}}
\value{result of calling remoteFun }

\author{Thomas Wutzler}



\seealso{\code{\link{twSnowfall}} }
\examples{
#sfInit(parallel=TRUE,cpus=2)
#sfExport("sfRemoteWrapper",namespace="twSnowfall")

#--------- inspecting what went wrong in the remote process
suppressWarnings(dir.create("tmp"))	# will store to tmp subdirectory
.remoteDumpfileBasename=file.path("tmp","testDump2")
.remoteDumpfile <- paste(.remoteDumpfileBasename,".rda",sep="")
unlink(.remoteDumpfile)
# throwing an error on remote process 
fTestStop <- function(){ stop("test throwing an error") }
tmp <- try( sfClusterCall( sfRemoteWrapper, remoteFun=fTestStop, remoteDumpfileBasename=.remoteDumpfileBasename ) )
.tmp.f <- function(){	
	# inspecting what was wrong in interactive R-session
	load(.remoteDumpfile)
	debugger(get(.remoteDumpfileBasename))
	# choose last step (18)
	require(debug)
	mtrace(remoteFun)
	do.call(remoteFun, c(remoteFunArgs, list(...)))
}

#--------- exporting variables and passing arguments by name
fReturnArgs <- function(...){ list(...) } #returns the calling arguments
fArgs <- list(bla="fasel")	
sfExport("fArgs")
res <- sfClusterCall( sfRemoteWrapper, remoteFun=fReturnArgs, remoteFunArgs=substitute(fArgs) )
identical( list(bla="fasel"), res[[1]] )

#--------- passing remoteFun and/or DumpfileBasename as part of remoteFunArgs
fArgs <- list(bla="fasel",remoteFun=fReturnArgs, remoteDumpfileBasename=.remoteDumpfileBasename )	
sfExport("fArgs")
#mtrace(sfRemoteWrapper)
res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=substitute(fArgs) )
identical( list(bla="fasel"), res[[1]] )
}
