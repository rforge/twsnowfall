\name{evalCommandArgs}
\alias{evalCommandArgs}
\title{evalCommandArgs}
\description{evaluate args passed to a batch script }
\usage{evalCommandArgs(args = commandArgs(TRUE), envir = new.env(parent = baseenv()))}
\arguments{
  \item{args}{the commands to be evaluated}
  \item{envir}{the environment where args should be evaluated}
}



\author{Thomas Wutzler}




\examples{
system('Rscript -e "str(commandArgs(TRUE))" i=1 n=8')
as.list(evalCommandArgs(args=c("i=1","n=8")))
if( FALSE ){
  # do not run automatically, as it depends on twMisc installed
  system('Rscript -e "library(twMisc); str(as.list(evalCommandArgs())); ls()" i=1 n=8')
    }
}
