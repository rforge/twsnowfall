\name{roundSec}
\alias{roundSec}
\title{roundSec}
\description{round times to given time interval}
\usage{roundSec(x, sec = 180)}
\arguments{
  \item{x}{vector of POSIXct, times to round}
  \item{sec}{rounding interval, default each 180 seconds, i.e. 3 minutes}
}



\author{Thomas Wutzler}




\examples{
midnight <- as.POSIXct("2014-12-09 00:00:00")
testTimes <- midnight +60*c(-1.51,-1.5,0,1.49,1.5,1.51,4.49)
testTimes
(rTestTimes <- roundSec(testTimes))
# getDaysMinute(rTestTimes)
roundSec(testTimes, sec=5*60)
}
