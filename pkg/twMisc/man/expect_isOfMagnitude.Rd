\name{expect_isOfMagnitude}
\alias{expect_isOfMagnitude}
\title{expect isOfMagnitude}
\description{expect_true that current is within interval of +-magnitude around target}
\usage{expect_isOfMagnitude(target, current, orderOfMagnitudes = 1/2, 
    ...)}
\arguments{
  \item{target}{numeric vector}
  \item{current}{numeric vector (recyled) of the same length as target}
  \item{orderOfMagnitudes}{see details}
  \item{\dots}{further arguments passed to expect_true}
}
\details{in range \code{10^{ log10(current)+-orderOfMagnitudes } }}
\value{TRUE or error}

\author{Thomas Wutzler}



\seealso{\code{\link{twMisc}} \code{\link{expect_isInInterval}}}
\examples{
tmp <- if( requireNamespace("testthat") ){
    target=1e4
    expect_isOfMagnitude(1e4, 3e4)    #TRUE
    #try(expect_isOfMagnitude(1e4, 6e4))   #error
    expect_isOfMagnitude(1e4, 5000)   #TRUE
    #try(expect_isOfMagnitude(1e4, 2000))  #error
    
    expect_isOfMagnitude(1e-4, 3e-4)  #TRUE
    #try(expect_isOfMagnitude(1e-4, 6e-4)) #error
    expect_isOfMagnitude(1e-4, 5e-5)  #TRUE
    #try(expect_isOfMagnitude(1e-4, 2e-5)) #error
}
}
