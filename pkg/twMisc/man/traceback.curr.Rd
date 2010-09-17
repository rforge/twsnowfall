\name{traceback.curr}
\alias{traceback.curr}
\title{traceback curr}
\description{prints the current function calls, with the deepest call on top}
\usage{
traceback.curr(collapse = NULL)
}
\arguments{
  \item{collapse}{an optional character string to separate the results. Not NA_character_.}
}
\details{if collapse is given, nothing is printed but instead a string returned		}
\value{array of strings of stack trace, or single string if character collapse is given }

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




\examples{
## tmp.g1 <- function(f.g1,...){ f.g1(...) }
## tmp.g2 <- function(f.g2,...){ f.g2(...) }
## tmp.f1 <- function(){ msg=traceback.curr(collapse="\n"); stop(msg) }
## try(tmp <- tmp.g1( tmp.g2, f.g2=tmp.f1 )) # prints stack trace f.g2 f.g1 tmp.g1 try
}
