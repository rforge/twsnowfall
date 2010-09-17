#twUtestF("applyLB")



sfFArgsApplyLB <- function(
	### Load balanced application of F_APPLY to arguments supplied by function F_ARGS(i).
	N_APPLYCASES, 	##<< i=1..N_APPLYCASES
	F_ARGS, 		##<< function(i) returning a list of first arguments to F_APPLY
	F_APPLY, 		##<< function to be applied
	...,			##<< further arguments passed to F_APPLY  
	SFFARGSAPPLY_ADDARGS = list(), ##<< further arguments passed to F_APPLY
	debugSequential=FALSE	##<< evaluate using lapply instead of dynamicClusterApply
){
	##seealso<< 
	## \code{\link{sfRemoteWrapper}}
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
attr(sfFArgsApplyLB,"ex") <- function(){
	#sfInit(parallel=TRUE,cpus=2)
	
	X <- matrix(0:9,nrow=5,ncol=2)
	dimnames(X) <- list( rows=paste("r",1:nrow(X),sep=""), cols=paste("c",1:ncol(X),sep="")  )
	Y <- X*10
	# delivering row i of X and row i of Y as arguments to F_Apply
	F_ARGS <- function(i){list(arg1=X[i,],arg2=Y[i,])}
	F_APPLY <- paste
	.res <- sfFArgsApplyLB( nrow(X), F_ARGS, F_APPLY, sep="-")
}


sfSimplifyLBResult <- function(
	### Transform the list resulting from \code{\link{sfClusterApplyLB}} to vector or matrix, similar as apply. 
	resl,
	caseNames=NULL	##<< list of column names, name corresponds to dimension name for columns, i.e as obtained from dimnames(X)[1]
){
	##seealso<< 
	## \code{\link{sfRemoteWrapper}}
	
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
	## \code{\link{sfRemoteWrapper}}
	
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
	## \code{\link{sfRemoteWrapper}}
	
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
	#cat(paste("s",job,"n",node,":",sep=""))
			#jobState[job] <- "processing" #only locally visible
			# do it outside and do not forget to call it
			dependVal <- if( job > dependsStep) 
					val[[job-dependsStep]] 
				else 
					initVal[[job]]
			args <- argfun(job, dependVal)
	#str(args)
			sendCall(cl[[node]], fun, args, tag = job)
	#cat(paste("--",sep=""))
	#when args was too big, sendCall did not return
		}
		#submit 2 jobs to each node, so that if one is finished there is already one in the queue
		for (i in 1:min(n, 2*useNCpus, dependsStep)){
			node = (i-1)%%useNCpus+1
			submit(node, i)
			jobState[i] <- "processing"
		}
		for (i in 1:n) {
			d <- recvOneResult(cl)
			val[d$tag] <- list(d$value)
		#cat(paste("r",d$tag,sep=""))
			jobState[d$tag] <- "completed"
			jobState[d$tag+dependsStep] <- "waiting"
			# get next waiting result j for which j-dependsStep has been completed
			j <- match("waiting",jobState)
		#cat(paste("_j",j,",",sep=""))
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
	### Load balanced application of F_APPLY with using result from a previous run to construct arguments.
	N_APPLYCASES,	##<< number of cases to calculate
	F_ARGS, 		##<< function returning arguments for case i, see details
	F_APPLY, 		##<< function to calculate
	SFFARGSAPPLY_initVal,	##<< intial values	
	SFFARGSAPPLY_dependsStep=length(SFFARGSAPPLY_initVal),	##<< dependencies in 1..n, 
	...,							##<< further arguments passed to F_APPLY  
	SFFARGSAPPLY_ADDARGS=list(),
		###  results presented to argfun for the first 1..dependsStep results
	debugSequential=FALSE	##<< the number of processors (might be smaller than cluster to assure that previous case i-useNCpus has been evaluated.)
){
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
	## \code{\link{sfRemoteWrapper}}
	## \code{\link{twDynamicClusterApplyDep}}
}
#twUtestF("applyLB","test.sfFArgsApplyLB")
attr(sfFArgsApplyDep,"ex") <- function(){
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



