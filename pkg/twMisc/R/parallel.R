dumpOnError <- function(
        ### Functional that creates a dump on error in function FUN
        FUN,                            ##<< function to be wrapped
        dumpFileBaseName="last.dump"	##<< baseName of the rda file to dump session to
){
    ##seealso<< 
    ## \code{\link{dump.frames}}
    localDumpFileBaseName <- dumpFileBaseName  
    function(...){
        # using withCallingHandlers instead of try or tryCatch, to get the call stack 
        # that includes the frame of FUN(...) 
        result <- withCallingHandlers( FUN(...), error = function(condition){
                    dump.frames(localDumpFileBaseName,TRUE)
                    stop(condition)
                })
        ##value<< result of call to FUN, or error thrown by FUN with side effect of a dump created
        result  
    }
}
attr(dumpOnError,"ex") <- function(){
    throwsError <- function(x){ stop("error in function throwsError.") }
    f2 <- function(...){throwsError(...)}
    dumpedFun <- dumpOnError(f2)
    if( FALSE ){ # do not run non-interactive
        result <- dumpedFun(1)  # throws an error
        # replace last.dump with value supplied to dumpFileBaseName
        load("last.dump.rda")  
        debugger(last.dump)
        # select item #3 FUN(...), i.e. f2, or #4 call to throwsError from f2
        # inspect value of list(...) in f2 or argument x supplied to throwsError
    }
}


.depr.dumpOnError <- function(
        ### Functional that creates a dump on error in function FUN
        FUN,                            ##<< function to be wrapped
        dumpFileBaseName="last.dump"	##<< baseName of the rda file to dump session to
){
    ##seealso<< 
    ## \code{\link{dump.frames}}
    localDumpFileBaseName <- dumpFileBaseName  
    localFUN <- FUN
    function(...){
        result <- try( localFUN(...), silent=TRUE )
        if( inherits(result, "try-error") ){
            ##details<<
            ## may reproduce and inspect the error by calling (see example) 
            ##
            ## \code{FUN(...)}
            FUN <- localFUN
            dump.frames(localDumpFileBaseName,TRUE)
            stop(result)
        }
        ##value<< result of call to FUN
        result  
    }
}
attr(.depr.dumpOnError,"ex") <- function(){
    throwsError <- function(x){ stop("error in function throwsError.") }
    dumpedFun <- dumpOnError(throwsError)
    if( FALSE ){ # do not run non-interactive
        result <- dumpedFun(1)
        options(error=recover)
        # replace last.dump with value supplied to dumpFileBaseName
        load("last.dump.rda")  
        debugger(last.dump)
        # in last stack type 
        # FUN(...)
        # inspect value of x supplied to throwsError
    }
}


parLapplySave <- function(
        ### Wrapper around parLapply that calls lapply on improper cluster
        cl, ...
){
    ##details<<
    ## lapply is called if \code{\link{isParallel}} returns FALSE
    #
    ##seealso<< \code{\link{isParallel}}, \code{\link{isClusterRunning}}, \code{\link{twMisc}}
    if( !isClusterRunning(cl) || length(cl)==1L){
        lapply(...)
    } else {
        parLapply(cl,...)
    }
}
attr(parLapplySave,"ex") <- function(){
    # next will work on cluster, if default cluster is defined
    # else will use lapply
    parLapplySave(NULL, 1:2, function(i){i*2})  
}

isParallel <- function(
        ### isClusterRunning and number of processors != 1 
        cl=NULL
) {
    ##seealso<< \code{\link{parLapplySave}}, \code{\link{isClusterRunning}}, \code{\link{twMisc}}
    isClusterRunning(cl) && length(cl) != 1
}


isClusterRunning <- function (
        ### test if given cluster is running
        cl  ##<< cluster to test
){
    ##seealso<< \code{\link{parLapplySave}}, \code{\link{isParallel}}, \code{\link{twMisc}}
    ##value<< TRUE if clusterEval worked, FALSE if not
    tryCatch(any(unlist(parallel::clusterEvalQ(cl, TRUE))), error = function(err) { FALSE })
}




