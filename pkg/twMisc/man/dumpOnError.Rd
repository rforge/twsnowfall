\name{dumpOnError}
\alias{dumpOnError}
\title{dumpOnError}
\description{Functional that creates a dump on error in function FUN}
\usage{dumpOnError(FUN, dumpFileBaseName = "last.dump")}
\arguments{
  \item{FUN}{function to be wrapped}
  \item{dumpFileBaseName}{baseName of the rda file to dump session to}
}

\value{result of call to FUN, or error thrown by FUN with side effect of a dump created}

\author{Thomas Wutzler}



\seealso{\code{\link{dump.frames}}}
\examples{
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
