
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
	stop(".sfParInternal: evaluated argsList must be a list")
}

sfPar <- function(
	### Executing a list of functions distributed via sfApply.
	fList, 				##<< the list of functions
	sfParParallel=TRUE, ##<< if FALSE, then apply is used instead of sfClusterApplyLB - good for debugging
	sfParArgsList=NULL,	##<< an optional list of arguments passed to each function
	...					##<< passed to each function
){
	# sfPar
	##seealso<< 
	## \code{\link{sfRemoteWrapper}}
	
	##details<<  
	## If argsList is a name it is evaluated on the node before calling the function
	## this allows to distribute variables by sfExport before rather than transferring them with each call
	## which is advantages when the call is issued very often and the arguments are large
	if( sfParParallel ) fApply="sfClusterApplyLB" else fApply="lapply"
	#sfClusterApplyLB(seq(along.with=fList), sfParInt, fList=fList, ...)
	clArgs = c( list(seq(along.with=fList)), list(.sfParInternal), list(fList=fList), list(...) )
	if( !is.null(sfParArgsList) ) clArgs = c( clArgs, list(sfParArgsList=sfParArgsList) )
	do.call( fApply, clArgs )	#quote=TRUE does not work with lapply
	### a list of returns for each function
}
attr(sfPar,"ex") <- function(){
	#sfInit(parallel=TRUE,cpus=4)
	fList <- list(
		function(...){ "Hello world!" } # dots are important
		,function(a){ a }
	)
	# each function is executed on a different node
	(res <- sfPar( fList, sfParArgsList=list(a="aha") ))	
}
