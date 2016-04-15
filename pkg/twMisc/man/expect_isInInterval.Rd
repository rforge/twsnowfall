\name{expect_isInInterval}
\alias{expect_isInInterval}
\title{expect isInInterval}
\description{expect_true( all((current >= targetMin) && (current <= targetMax)) )}
\usage{expect_isInInterval(current, targetMin = 0.025, targetMax = 0.975, 
    ...)}
\arguments{
  \item{current}{
}
  \item{targetMin}{
}
  \item{targetMax}{
}
  \item{\dots}{further arguments passed to expect_true}
}
\details{all parameters will be recycled int the comparison
targetMin and targetMax default to two p of sided 95\% confidence interval}


\author{Thomas Wutzler}



\seealso{\code{\link{twMisc}} \code{\link{expect_isOfMagnitude}}}

