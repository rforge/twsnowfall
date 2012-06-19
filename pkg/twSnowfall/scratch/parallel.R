#twUtestF("applyLB")

sfRemoteWrapper <- function(
	### Wrapper for remotely called function.
	...,						##<< Further arguments to remoteFun
	remoteFunArgs = list(), 	##<< The Arguments to remoteFun. 
	remoteFun = NULL, 			##<< The function to be called. Alternatively provided as an entry in remoteFunArgs.
	remoteDumpfileBasename=NULL		##<< The name of a dump on error. If null, no dump is created. Alternatively provided as an entry in remoteFunArgs.
){
	##details<<  
	## Further methods deal with \itemize{
	## \item{ providing more than one argument to sfClusterApply: \code{\link{sfFArgsApplyLB}}  } 
	## \item{ simplifying the list of results of sfClusterApply similar to apply: \code{\link{sfSimplifyLBResult}}  }
	## \item{ load balanced parallel application of FUN to rows or columns of matrix X: \code{\link{sfApplyMatrixLB}}  }
	## \item{ providing result from dependStep steps ago to FUN in load balancing: \code{\link{sfFArgsApplyDep}} }
	## \item{ executing a list of functions distributed via sfApply: \code{\link{sfPar}}  }
	## }
	
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
	remoteFunArgs$remoteFun <- NULL #do not provide argument to fLogLik
	if( is.null(remoteDumpfileBasename) && !is.null(remoteFunArgs$remoteDumpfileBasename)){
		remoteDumpfileBasename <- remoteFunArgs$remoteDumpfileBasename; 
	}
	remoteFunArgs$remoteDumpfileBasename <- NULL	#do not provide argument to fLogLik
	#if( is.null(remoteFun) stop
	
	body <- expression( comp <- do.call(remoteFun, c(remoteFunArgs, list(...)), quote=TRUE) )
	##details<<  
	## If dumpFileBaseName is not null, a dump is created in 
	## this file when an error occurs.
	if( !is.null(remoteDumpfileBasename)){
		comp <- try( eval(body) )
		if( inherits(comp, "try-error") ){
			dump.frames(remoteDumpfileBasename,TRUE)
			stop(comp)
		}
		comp
	}else{
		comp <- eval(body)
	}
	comp
	### result of calling remoteFun 
}


sfFArgsApplyLB <- function(
	### Load balanced application of F_APPLY to arguments F_ARGS(i).
	N_APPLYCASES, 	##<< i=1..N_APPLYCASES
	F_ARGS, 		##<< function(i) returning a list of first arguments to F_APPLY
	F_APPLY, 		##<< function to be applied
	...,			##<< further arguments passed to F_APPLY  
	SFFARGSAPPLY_ADDARGS = list(), ##<< further arguments passed to F_APPLY
	debugSequential=FALSE	##<< evaluate using lapply instead of dynamicClusterApply
){
	##seealso<< 
	## \code{\link{twSnowfall}}
	snowfall:::sfCheck()
	snowfall:::checkFunction(F_ARGS)
	addArgs = c(SFFARGSAPPLY_ADDARGS, list(...))
	argfun <- function(i) c(F_ARGS(i), addArgs)
	if (sfParallel() && !debugSequential) 
		return(dynamicClusterApply(sfGetCluster(), F_APPLY, N_APPLYCASES, argfun ))
	else {
		lapply(1:N_APPLYCASES, function(i){
				clArgs <- argfun(i)
				do.call( F_APPLY, clArgs)
			})
	}
	### a list of results of F_APPLY
}
#twUtestF("applyLB","test.sfFArgsApplyLB")


sfSimplifyLBResult <- function(
	### Transform the list resulting from \code{\link{sfClusterApplyLB}} to vector or matrix, similar as apply. 
	resl,
	caseNames=NULL	##<< list of column names, name corresponds to dimension name for columns, i.e as obtained from dimnames(X)[1]
){
	##seealso<< 
	## \code{\link{twSnowfall}}
	
	# assumes that all components in the list are of identical structure
	res <- 
		if( length(resl[[1]]) == 0) resl[[1]]	#includes NULL
		else if( length(resl[[1]]) == 1)	res <- structure( unlist(resl), names=as.vector(unlist(caseNames)) )
		else if ( is.list(resl[[1]]))		structure(resl,names=as.vector(unlist(caseNames)) )
		#else if( is.vector(resl[[1]]) | is.matrix(resl[[1]]) ){	#also works for matrix for which is.vector is TRUE
		else if( is.atomic(resl[[1]]) ){	#is.vector does not work for objects with attributes
			.dimnames <- c( list(names(resl[[1]])), caseNames ) 
			if( all(sapply(.dimnames,is.null))) .dimnames<-NULL
			res <- matrix( unlist(resl), byrow=FALSE, nrow=length(resl[[1]]), dimnames=.dimnames  )
		}else resl
	### Depending on components (result of FUN) \describe{
	### \item{scalar}{vector}
	### \item{vector}{matrix with cases in columns}
	### \item{matrix}{each column corresponds to as.vector(matrix), i.e. stacked to one vector }
	### \item{list}{list of lists. May transform to dataframe by: do.call(rbind, lapply(.res,data.frame)))}
	### \item{data.frame}{list of data.frames}}
}

sfApplyMatrixLB <- function(
	### Load balanced parallel application of FUN to rows or columns of matrix X 
	X,			##<< matrix with rows corresponding to cases supplied to first argument of FUN 
	MARGIN=1,	##<< 1 indicates rows, 2 indicates columns
	FUN,		##<< function to be applied on each row
	...,		##<< further arguments passed to FUN
	debugSequential = FALSE 	##<< use apply 
){
	##seealso<< 
	## \code{\link{twSnowfall}}
	
	if( !is.matrix(X) )
		stop("X must be matrix.")
	if( !(MARGIN %in% 1:2))
		stop("MARGIN must be 1 or 2.")
	if( nrow(X) == 0)
		return( list() )
	##details<< 
	## if debugSequentail is TRUE, simple apply is used 
	res <- if( debugSequential ){
			apply(X, MARGIN, FUN, ... )
		}else{
			caseNames <- dimnames(X)[MARGIN]
			nCases <- if(MARGIN==1) nrow(X) else ncol(X)
			F_ARGS <- if(MARGIN==1) function(i) list(X[i,]) else function(i) list(X[,i])
			resl <- sfFArgsApplyLB( nCases, F_ARGS, FUN, ...)
			res <- sfSimplifyLBResult(resl,caseNames)
		}#!debugSeqential
	### a list with result of FUN for each row or columns of X respectively
}
#twUtestF("","test.sfFArgsApplyDep")


twDynamicClusterApplyDep <- function (
	### Modification of \code{dynamicClusterApply} (snow-internal) to provide result of previous step.
	cl, fun, n, argfun, ##<< a function returning a list of arguments passed to fun
	dependsStep,		##<< dependencies in 1..n
	initVal=vector("list",dependsStep)	##<<  results presented to argfun for the first 1..dependsStep results
){
	##seealso<< 
	## \code{\link{twSnowfall}}
	
	##details<< 
	## \code{argfun=function(i,prevRes) list( <args(i,prevRes)> )} 
	## provides arguments to fun where \describe{
	## \item{i}{index for which to generate arguments}
	## \item{prevRes}{result of FUN for case i-dependsStep.}}
	## can be assume that prevResList[[i-useNCpus]] has been evaluted already
	
	##details<< 
	## \code{dependsStep} 
	checkCluster(cl)
	useNCpus <- length(cl)
	if (n > 0 && useNCpus > 0) {
		val <- vector("list", n)
		jobState <- rep( as.factor(c("depending","waiting","processing","completed"))[1], n+dependsStep)
		jobState[1:min(dependsStep,n)]<-"waiting"	#first dependsStep jobs are waiting
		submit <- function(node, job){
			#jobState[job] <- "processing" #only locally visible
			# do it outside and do not forget to call it
			dependVal <- if( job > dependsStep) val[[job-dependsStep]] else initVal[[job]] 
			sendCall(cl[[node]], fun, 
				argfun(job, dependVal), tag = job)
		}
		for (i in 1:min(n, useNCpus, dependsStep)){
			submit(i, i)
			jobState[i] <- "processing"
		}
		for (i in 1:n) {
			d <- recvOneResult(cl)
			val[d$tag] <- list(d$value)
			jobState[d$tag] <- "completed"
			jobState[d$tag+dependsStep] <- "waiting"
			# get next waiting result j for which j-dependsStep has been completed
			j <- match("waiting",jobState)
			if (j <= n){ 
				submit(d$node, j)
				jobState[j] <- "processing"
			}
		}
		#if( !all(jobState[1:n]=="completed")) recover()
		checkForRemoteErrors(val)
	}
	### same (snow-internal) \code{dynamicClusterApply}
}


sfFArgsApplyDep <- function (
	### Load balanced application of F_APPLY with arguments provided by F_ARGS.
	N_APPLYCASES, 
	F_ARGS, 
	F_APPLY, 
	SFFARGSAPPLY_initVal,	
	SFFARGSAPPLY_dependsStep=length(SFFARGSAPPLY_initVal),	##<< dependencies in 1..n
	...,							##<< further arguments passed to F_APPLY  
	SFFARGSAPPLY_ADDARGS=list(),
		###  results presented to argfun for the first 1..dependsStep results
	debugSequential=FALSE	##<< the number of processors (might be smaller than cluster to assure that previous case i-useNCpus has been evaluated.)
){
	### invokes
	if( !is.vector(SFFARGSAPPLY_initVal) || !(length(SFFARGSAPPLY_initVal)>=SFFARGSAPPLY_dependsStep) )
		stop("initVal must be vector of mode list with length dependsStep")
	if( mode(SFFARGSAPPLY_initVal)!= "list" )
		SFFARGSAPPLY_initVal <- as.vector(SFFARGSAPPLY_initVal, mode="list")
	ds <- SFFARGSAPPLY_dependsStep	#avoid long name, which is necessary to prevent partial agrument matching
	snowfall:::sfCheck()
	snowfall:::checkFunction(F_ARGS)
	addArgs = c(SFFARGSAPPLY_ADDARGS, list(...))
	argfun <- function(i,prevRes) c(F_ARGS(i,prevRes), addArgs)
	#argfun(1,SFFARGSAPPLY_initVal[[1]])
	if (sfParallel() && !debugSequential) 
		return(twDynamicClusterApplyDep(sfGetCluster(), F_APPLY, N_APPLYCASES, argfun,
				dependsStep=ds,initVal=SFFARGSAPPLY_initVal ))
	else {
		val <- c(SFFARGSAPPLY_initVal[1:ds], vector("list",N_APPLYCASES) ) #prepend with initial values
		for( i in 1:N_APPLYCASES){
			val[[i+ds]] <- do.call(F_APPLY, argfun(i,val[[i]]),quote=TRUE)
		}
		val[(1:ds)] <- NULL		#remove the initial values
		val
	}
	### a list of results of F_APPLY
	
	##seealso<< 
	## \code{\link{twSnowfall}}
	## \code{\link{twDynamicClusterApplyDep}}
}
#twUtestF("applyLB","test.sfFArgsApplyLB")


.df.iproc <- function(
	### subsetting a dataframe to assign to 1 out of nproc parts
	df, 	##<< data frame to subset
	iproc, 	##<< ith subset
	nproc	##<< total number of subsets
){
	ni <- ceiling(nrow(df)/nproc)
	df[ ((iproc-1)*ni+1):min( (iproc*ni), nrow(df)), ]
}

.tmp.f <- function(
	### interactive development code
){
	#mtrace(.df.iproc)
	#mtrace.off()
	tmp <- data.frame(a=1:7,b="bla")
	.df.iproc(tmp,1,3)	#1,2,3
	.df.iproc(tmp,2,3)	#4,5,6
	.df.iproc(tmp,3,3)   #7
}


.sfParInternal <- function( 
	### executing parralel code
	index, fList, sfParArgsList=NULL, ... 
){
	##details<<  used by \code{\link{sfPar}}, not to be called by the user
	## , for usage see tests/testSnowfall.R
	
	# .sfParInternal
	# executing function fList[[index]]
	# if argsList is supplied 
	if( is.null(sfParArgsList) )
		return( fList[[index]](...) )
	if( is.name(sfParArgsList) ) argsList=eval.parent(sfParArgsList)
	if( is.list(sfParArgsList) ){
		cl = c( list(fList[[index]]), sfParArgsList, list(...) )
		return( eval( as.call(cl) ) )
	} 
	traceback.curr()
	str(sfParArgsList)
	stop("sfParInt: evaluated argsList must be a list")
}

sfPar <- function(
	### executing a list of functions distributed via sfApply.
	fList, 				##<< the list of functions
	sfParParallel=TRUE, ##<< if FALSE, then apply is used instead of sfClusterApplyLB - good for debugging
	sfParArgsList=NULL,	##<< an optional list of arguments passed to each function
	...					##<< passed to each function
){
	# sfPar
	##seealso<< 
	## \code{\link{twSnowfall}}
	
	##details<<  
	## if argsList is a name it is evaluated on the node before calling the function
	## this allows to distribute variables by sfExport before rather than transferring them with each call
	## which is advantages when the call is issued very often and the arguments are large
	if( sfParParallel ) fApply="sfClusterApplyLB" else fApply="lapply"
	#sfClusterApplyLB(seq(along.with=fList), sfParInt, fList=fList, ...)
	clArgs = c( list(seq(along.with=fList)), list(.sfParInternal), list(fList=fList), list(...) )
	if( !is.null(sfParArgsList) ) clArgs = c( clArgs, list(sfParArgsList=sfParArgsList) )
	do.call( fApply, clArgs )	#quote=TRUE does not work with lapply
	### a list of returns for each function
}

.tmp.f <- function(){
	#library(snowfall)
	fList <- list(
		function(...){ "Hello world!" }
		,function(a){ a }
	)
	#mtrace(sfParInternal)
	.sfParInternal( 1, fList, a="ah")
	.sfParInternal( 2, fList, a="ah")
	.sfParInternal( 2, fList, sfParArgsList="ah")	#error message because is not a list
	.sfParInternal( 2, fList, sfParArgsList=list("ah") )	#error message because is not a list
	.sfParInternal( 1, fList, sfParArgsList=list("ah") )	#error message because is not a list
	
	sfPar( fList, sfParParallel=FALSE, a="aha" )
	sfPar( fList, sfParParallel=FALSE, "aha" )	#error
	sfPar( fList, sfParParallel=FALSE, sfParArgsList="aha" )	#error
	sfPar( fList, sfParParallel=FALSE, sfParArgsList=list(a="aha") )	
	sfPar( fList, sfParParallel=FALSE, sfParArgsList=list(a="aha"), a="ahb" )	
	
	sfPar( fList, sfParArgsList=list(a="aha") )	
	sfPar( fList, a="aha" )
	
	aPre = "aha"
	sfPar( fList, a=as.name("aPre") )	#should work, ... are evaluated before distribution
	sfPar( fList, sfParArgsList=list(a=as.name("aPre")) )	#should give an error because not yet exported
	sfExport("aPre")
	sfPar( fList, sfParArgsList=list(a=as.name("aPre")) )	#should work now
}

