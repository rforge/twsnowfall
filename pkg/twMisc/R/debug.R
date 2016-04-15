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

#--------------- custom errors / signals
# http://adv-r.had.co.nz/Exceptions-Debugging.html#condition-handling
stopCustom <- function(
        ### Extension of \code{\link{stop}} to generates error of given subclass
        subClass = "simpleError" ##<< string  vector: subClasses of error, (with most specific first)
        ,...                     ##<< further arguments concatenated to a message using \code{\link{pasteHead}}
        , call = sys.call(-1)    ##<< frame where the error occured
) {
    message <- pasteHead(...)
    condition <- .newCondition(c(subClass, "error"), message=message, call = call)
    stop(condition)
}
attr(stopCustom,"ex") <- function(){
    myLog <- function(x) {
        if (x[1]=="uncaughtClass") stopCustom(c("uncaughtClass"),x)     # handled by "error"
        if (x[1]=="uncaughtTwMiscSublass") stopCustom(c("uncaughtSubClass","twMiscError"),x)    # handled by "twMiscError"
        if (!is.numeric(x))  stopCustom(c("invalidClass","twMiscError"), "myLog() needs numeric input, but input was",x)
        if (any(x < 0)) stopCustom(c("invalidValue","twMiscError"), "myLog() needs positive inputs, but input was",x)
        log(x)
    }
    tryCatch(
            #myLog(c("a","b"))
            #myLog("uncaughtTwMiscSublass")
            #myLog("uncaughtClass")
            ,invalidClass = function(c) "invalid class"
            ,invalidValue = function(c) "invalid value"
            # important to catch specialized classes first
            ,twMiscError = function(c) "subclass of twMiscError"
            ,error = function(c) "subClass of error"
    )    
}

pasteHead <- function(
        ###  Collapse each argument to at most nHead elements and apply fPasteItem to each one 
        ...             ##<< arguments to paste together
        ,nHead = 3L     ##<< number of elements to include
        ,fPasteItem=pasteCollapseNamed ##<< collapse function applied to each argument.
            ## Defaults to \code{\link{pasteCollapseNamed}}.
        #= function(item){ paste(item, collapse=",")}  
){
    fItem <- function(item){
        itemCollapsed <- fPasteItem(head(item,nHead))
        if( length(item) <= nHead ) return(itemCollapsed)
        paste0( itemCollapsed,"..",length(item))
    }
    items <- lapply( list(...), fItem )
    do.call( paste0, items )
}
attr(pasteHead,"ex") <- function(){
    pasteHead("short vector is: ",c(a=1,b=2)," by string.")
    pasteHead("longer vector is: ",1:5," by string.")
}

pasteCollapseNamed <- function(
        ###  paste arguments by name=value 
        x               ##<< object to be collapsed into string
        ,collapse=","	##<< delimited to separate items in vector x
        ,digits=4L      ##<< number of digits for numeric values
){
    ##seealso<< \code{\link{pasteHead}}
    ##details<<
    ## If x is a list, then \code{\link{pasteHead}} is applied to each item. 
    xs <- if( is.list(x) ){
        vapply(x,FUN.VALUE=character(1), function(item){
                    if(length(item) > 1L){
                        pasteHead(item,fPasteItem=pasteCollapseNamed)
                    } else {
                        pasteCollapseNamed(item)
                    }
                })
    } else x
    if( is.numeric(xs) ) xs <- signif(xs,digits)
    collapsed <- if( length(names(xs)) ){ 
        paste0(names(xs),"=",xs, collapse=collapse)
    } else {
        paste(xs, collapse=collapse)
    }
    if( length(xs) != 1 ){
        paste0("(",collapsed,")")
    } else {
        collapsed
    }
    ##value<< a scalar string, surrounded by () if x is not a scalar.
}
attr(pasteCollapseNamed,"ex") <- function(){
    pasteCollapseNamed( c(a=1) )
    pasteCollapseNamed( c(a=pi,b=2) )
    pasteCollapseNamed( c() )
    pasteCollapseNamed( tmp <- list(aSub=list(c(a1=pi,a2=2,a3=3,a4=4)),b="b",c=c()) )
}

.newCondition <- function(subclass, message, call = sys.call(-1), ...) {
    structure(
            list(message = message, call = call),
            class = c(subclass, "condition"),
            ...
    )
}

stopTwMisc <- function(
        ### Extension of \code{\link{stopCustom}} to generates error of given subclass of "twMiscError"
         ...                      ##<< further arguments concatenated to a message
        , subClass = character(0) ##<< string  vector: subClasses of error, (with most specific first)
        , call = sys.call(-1)     ##<< frame where the error occured 
) {
    ##details<<
    ## This function demonstrates how to write a custom error function to simplify raising package-specific errors
    ## Handlers can check of twMiscError to handle all those errors.
    ## With explicitly specifying \code{subClass=<mySubClass>}, specific conditions can be caught.
    ## See the source code by typing \code{stopTwMisc}
    stopCustom( c(subClass,"twMiscError"), call=call, ...)
}
attr(stopTwMisc,"ex") <- function(){
    myLog <- function(x) {
        if (!is.numeric(x))  stopTwMisc("myLog() needs numeric input, but input was ",x)
        if (any(x < 0)) stopTwMisc(subClass="invalidValue", "myLog() needs positive inputs, but input was ",x)
        log(x)
    }
    tryCatch(
            myLog(c(a=1,b=-3))
            #,invalidValue = function(condition) "invalid value"
            ,twMiscError = function(condition) paste("subclass of twMiscError:", condition$message)  
            ,error = function(condition) paste("general error:", condition$message)  
    )    
}


