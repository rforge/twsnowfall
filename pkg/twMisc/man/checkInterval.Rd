\name{checkInterval}
\alias{checkInterval}
\title{checkInterval}
\description{checkTrue( all((current >= targetMin) && (current <= targetMax)) )}
\usage{checkInterval(current, targetMin = 0.025, targetMax = 0.975, 
    ...)}
\arguments{
  \item{current}{
}
  \item{targetMin}{
}
  \item{targetMax}{
}
  \item{\dots}{further arguments passed to checkTrue}
}
\details{all parameters will be recycled int the comparison
targetMin and targetMax default to two p of sided 95\% confidence interval }


\author{Thomas Wutzler}



\seealso{\code{\link{twMisc}} \code{\link{checkMagnitude}}}

