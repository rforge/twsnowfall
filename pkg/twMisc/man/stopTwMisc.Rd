\name{stopTwMisc}
\alias{stopTwMisc}
\title{stopTwMisc}
\description{Extension of \code{\link{stopCustom}} to generates error of given subclass of "twMiscError"}
\usage{stopTwMisc(..., subClass = character(0), call = sys.call(-1))}
\arguments{
  \item{\dots}{further arguments concatenated to a message}
  \item{subClass}{string  vector: subClasses of error, (with most specific first)}
  \item{call}{frame where the error occured}
}
\details{This function demonstrates how to write a custom error function to simplify raising package-specific errors
Handlers can check of twMiscError to handle all those errors.
With explicitly specifying \code{subClass=<mySubClass>}, specific conditions can be caught.
See the source code by typing \code{stopTwMisc}}


\author{Thomas Wutzler}




\examples{
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
