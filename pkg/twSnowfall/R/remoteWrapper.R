sfRemoteWrapper <- function(
	### Wrapper for remotely called function supporting error dumps and remotely resolving arguments.
	...,						##<< Further arguments to remoteFun
	remoteFunArgs = list(), 	##<< The Arguments to remoteFun. 
	remoteFun = NULL, 			##<< The function to be called. Alternatively provided as an entry in remoteFunArgs.
	remoteDumpfileBasename=NULL		##<< The name of a dump on error. If null, no dump is created. Alternatively provided as an entry in remoteFunArgs.
){
	##seealso<< \code{\link{twSnowfall}} 
	
	##details<< 
	## If remoteFunArgs is a name, it is first evaulated in the parent frame.
	## This supports the usage of sfExport of the remoteFunArgs instead of passing much data in sfApply via \dots
	if( is.name(remoteFunArgs) ) remoteFunArgs <- eval.parent(remoteFunArgs)
	
	##details<< 
	## If remoteFun is NULL, then remoteFunArgs$remoteFun is used, and cleared from remoteFunArgs. 
	## If dumpFileBaseName is NULL, then remoteFunArgs$dumpFileBaseName is used, and cleared from remoteFunArgs.
	if( is.null(remoteFun) && !is.null(remoteFunArgs$remoteFun) ){
		remoteFun <- remoteFunArgs$remoteFun
	}
	remoteFunArgs$remoteFun <- NULL #do not provide argument to remoteFun
	if( is.null(remoteDumpfileBasename) && !is.null(remoteFunArgs$remoteDumpfileBasename)){
		remoteDumpfileBasename <- remoteFunArgs$remoteDumpfileBasename; 
	}
	remoteFunArgs$remoteDumpfileBasename <- NULL	#do not provide argument to remoteFun
	#if( is.null(remoteFun) stop
	
	#dump.frames(remoteDumpfileBasename,TRUE)
	#stop(paste("dumped frames to",remoteDumpfileBasename) )
	
	#body <- expression( comp <- do.call(remoteFun, c(remoteFunArgs, list(...)), quote=TRUE) )
	##details<<  
	## If dumpFileBaseName is not null, a dump is created in 
	## this file when an error occurs.
	## Trace the error then by \itemize{
	## \item{\code{load(paste(remoteDumpfileBasename,".rda",sep="")); debugger(get(remoteDumpfileBasename))}}
	## \item{\code{mtrace(remoteFun); try( eval(body) )}}
	## }
	if( !is.null(remoteDumpfileBasename)){
		#comp <- try( eval(body), silent=TRUE )
		comp <- try( 
			comp <- do.call(remoteFun, c(remoteFunArgs, list(...)), quote=TRUE)
			, silent=TRUE )
		if( inherits(comp, "try-error") ){
			dump.frames(remoteDumpfileBasename,TRUE)
			stop(comp)
		}
		comp
	}else{
		comp <- do.call(remoteFun, c(remoteFunArgs, list(...)), quote=TRUE)
	}
	comp
	### result of calling remoteFun 
}
attr(sfRemoteWrapper,"ex") <- function(){
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

