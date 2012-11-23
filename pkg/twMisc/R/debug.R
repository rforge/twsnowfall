#----------- access the call stack
traceback.curr <- function(
    ### prints the current function calls, with the deepest call on top
    collapse=NULL
    ### an optional character string to separate the results. Not NA_character_.
){
    tmp.lines <- sapply( (sys.nframe()-1):1, sys.call   )
    if( !is.character(collapse) )
        ##details<< if collapse is given, nothing is printed but instead a string returned      
        paste( tmp.lines, collapse=collapse )
    else{
        sapply( tmp.lines, print )
        tmp.lines
    }
    ### array of strings of stack trace, or single string if character collapse is given 
    ##examples<< 
    ## tmp.g1 <- function(f.g1,...){ f.g1(...) }
    ## tmp.g2 <- function(f.g2,...){ f.g2(...) }
    ## tmp.f1 <- function(){ msg=traceback.curr(collapse="\n"); stop(msg) }
    ## try(tmp <- tmp.g1( tmp.g2, f.g2=tmp.f1 )) # prints stack trace f.g2 f.g1 tmp.g1 try
}

.tmp.f <- function(
    ### interactive development code
){
    traceback.curr()
    mtrace(traceback.curr)
    tmp.g1 <- function(f.g1,...){ f.g1(...) }
    tmp.g2 <- function(f.g2,...){ f.g2(...) }
    tmp.f1 <- function(){ msg=traceback.curr(collapse="\n"); stop(msg) }
    #mtrace(tmp.f1)
    tmp <- try(tmp <- tmp.g1( tmp.g2, f.g2=tmp.f1 )) 
    tmp <- try( tmp.g1( tmp.g2, f.g2=tmp.f1 ) )
    tmp
}
#see DEMCzsp of.multiSumsArgsList for providing a dump within remote function


tw4 <- function(
    ### traceback(max.lines=4,...)
    ...
){ 
    traceback(max.lines=4,...)
}



