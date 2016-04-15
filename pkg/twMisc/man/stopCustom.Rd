\name{stopCustom}
\alias{stopCustom}
\title{stopCustom}
\description{Extension of \code{\link{stop}} to generates error of given subclass}
\usage{stopCustom(subClass = "simpleError", ..., call = sys.call(-1))}
\arguments{
  \item{subClass}{string  vector: subClasses of error, (with most specific first)}
  \item{\dots}{further arguments concatenated to a message using \code{\link{pasteHead}}}
  \item{call}{frame where the error occured}
}



\author{Thomas Wutzler}




\examples{
myLog <- function(x) {
    # handled by "error"
    if (x[1]=="uncaughtClass") stopCustom(c("uncaughtClass"),x)     
    # handled by "twMiscError"
    if (x[1]=="uncaughtTwMiscSublass") stopCustom(c("uncaughtSubClass",
                "twMiscError"),x)    
    if (!is.numeric(x))  stopCustom(c("invalidClass","twMiscError"), 
                "myLog() needs numeric input, but input was",x)
    if (any(x < 0)) stopCustom(c("invalidValue","twMiscError"), 
                "myLog() needs positive inputs, but input was",x)
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
