\name{checkMagnitude}
\alias{checkMagnitude}
\title{checkMagnitude}
\description{checkTrue that current is within interval of +-magnitude around target}
\usage{checkMagnitude(target, current, orderOfMagnitudes = 1/2, 
    ...)}
\arguments{
  \item{target}{numeric vector}
  \item{current}{numeric vector (recyled) of the same length as target}
  \item{orderOfMagnitudes}{see details}
  \item{\dots}{further arguments passed to checkTrue}
}
\details{in range \code{10^{ log10(current)+-orderOfMagnitudes } }}
\value{TRUE or error}

\author{Thomas Wutzler}



\seealso{\code{\link{twMisc}} \code{\link{checkInterval}}}
\examples{
if( requireNamespace("RUnit") ){
    target=1e4
    checkMagnitude(1e4, 3e4)    #TRUE
    try(checkMagnitude(1e4, 6e4))   #error
    checkMagnitude(1e4, 5000)   #TRUE
    try(checkMagnitude(1e4, 2000))  #error
    
    checkMagnitude(1e-4, 3e-4)  #TRUE
    try(checkMagnitude(1e-4, 6e-4)) #error
    checkMagnitude(1e-4, 5e-5)  #TRUE
    try(checkMagnitude(1e-4, 2e-5)) #error
}
}
