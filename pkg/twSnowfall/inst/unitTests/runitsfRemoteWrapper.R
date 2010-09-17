fReturnArgs <- function(...){ list(...) } #returns the calling arguments
fTestStop <- function(...){ stop("test throwing an error") }
fTestEvalParent <- function(...){ eval.parent(as.name("fArgs"))}

test.simpleSeq <- function(){
	## calling sfRemoteWrapper with just a function name and some arguments
	res <- sfRemoteWrapper( remoteFun=fReturnArgs )
	checkEquals( list(), res )
	res <- sfRemoteWrapper( remoteFun=fReturnArgs, foo="bar" )
	checkEquals( list(foo="bar"), res )
	res <- sfRemoteWrapper( remoteFun=fReturnArgs, remoteFunArgs=list(bla="fasel"), foo="bar" )
	checkEquals( list(bla="fasel", foo="bar"), res )
}

test.funInArgsSeq <- function(){
	## calling sfRemoteWrapper passing function name by remoteFunArgs
	checkException( res <- sfRemoteWrapper( ) )
	checkException( res <- sfRemoteWrapper( remoteFunArgs=list(bla="fasel"), foo="bar" ) )
	res <- sfRemoteWrapper( remoteFunArgs=list(bla="fasel",remoteFun=fReturnArgs), foo="bar" )
	checkEquals( list(bla="fasel", foo="bar"), res )
}

test.dumpSeq <- function(){
	.remoteDumpfileBasename="testDump"
	.remoteDumpfile <- paste(.remoteDumpfileBasename,".rda",sep="")
	unlink(.remoteDumpfile)
	checkTrue( !file.exists(.remoteDumpfile))
	checkException( res <- sfRemoteWrapper( remoteFunArgs=list(bla="fasel",remoteFun=fTestStop), foo="bar" ) )
	checkTrue( !file.exists(.remoteDumpfile))
	
	unlink(.remoteDumpfile)
	checkTrue( !file.exists(.remoteDumpfile))
	#mtrace(sfRemoteWrapper)
	checkException( res <- sfRemoteWrapper( remoteFunArgs=list(bla="fasel",remoteFun=fTestStop, remoteDumpfileBasename=.remoteDumpfileBasename), foo="bar" ) )
	checkTrue( file.exists(.remoteDumpfile))
}

test.simplePar <- function(){
	## calling sfRemoteWrapper with just a function name and some arguments
	res <- sfClusterCall( sfRemoteWrapper, remoteFun=fReturnArgs )
	checkEquals( sfCpus(), length(res) )
	checkEquals( list(), res[[1]] )
	res <- sfClusterCall( sfRemoteWrapper, remoteFun=fReturnArgs, foo="bar" )
	checkEquals( list(foo="bar"), res[[1]] )
	res <- sfClusterCall( sfRemoteWrapper, remoteFun=fReturnArgs, remoteFunArgs=list(bla="fasel"), foo="bar" )
	checkEquals( list(bla="fasel", foo="bar"), res[[1]] )
}

test.funInArgsPar <- function(){
	## calling sfRemoteWrapper with just a function name and some arguments
	checkException( res <- sfClusterCall( sfRemoteWrapper ) )	#missing function
	checkException( res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=list(bla="fasel"), foo="bar" ) ) #missing function
	res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=list(remoteFun=fReturnArgs, bla="fasel"), foo="bar" )
	checkEquals( list(bla="fasel", foo="bar"), res[[1]] )
}

test.dumpPar <- function(){
	.remoteDumpfileBasename="testDump"
	.remoteDumpfile <- paste(.remoteDumpfileBasename,".rda",sep="")
	unlink(.remoteDumpfile)
	checkTrue( !file.exists(.remoteDumpfile))
	checkException( res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=list(bla="fasel",remoteFun=fTestStop), foo="bar" ) )
	checkTrue( !file.exists(.remoteDumpfile))
	
	unlink(.remoteDumpfile)
	checkTrue( !file.exists(.remoteDumpfile))
	#mtrace(sfRemoteWrapper)
	checkException( res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=list(bla="fasel",remoteFun=fTestStop, remoteDumpfileBasename=.remoteDumpfileBasename), foo="bar" ) )
	checkTrue( file.exists(.remoteDumpfile))
	
	#load(.remoteDumpfile)
	#debugger(testDump)
}

test.namedArgs <- function(){
	## exporting argument and just calling with the name
	fArgs <- list(remoteFun=fReturnArgs, bla="fasel")
	#mtrace(sfRemoteWrapper)
	res <- sfRemoteWrapper( remoteFunArgs=as.name("fArgs"), foo="bar" )
	checkEquals( list(bla="fasel", foo="bar"), res )
	
	#sfClusterCall( fTestEvalParent )
	.remoteDumpfileBasename="testDump"
	.remoteDumpfile <- paste(.remoteDumpfileBasename,".rda",sep="")
	unlink(.remoteDumpfile)
	sfRemoveAll()
	sfExport("fArgs")
	checkException( res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=as.name("fArgs_nonext"), foo="bar", remoteDumpfileBasename=.remoteDumpfileBasename ))
	res <- sfClusterCall( sfRemoteWrapper, remoteFunArgs=as.name("fArgs"), foo="bar", remoteDumpfileBasename=.remoteDumpfileBasename )
	checkEquals( list(bla="fasel", foo="bar"), res[[1]] )
}



