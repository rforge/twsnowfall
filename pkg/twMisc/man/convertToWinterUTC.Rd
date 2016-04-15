\name{convertToWinterUTC}
\alias{convertToWinterUTC}
\title{convertToWinterUTC}
\description{deprecated: convert series to winter time, i.e. expressed as GMT}
\usage{convertToWinterUTC(x, hourOffset = -1)}
\arguments{
  \item{x}{POSIXct vector using summer time}
  \item{hourOffset}{offset of the original time zone winter time}
}
\details{R plots time zones with summer and winter time automatically switching 
to summer time. This is annoying in plots of hours across the time shift.
Remedy is to express hours in GMT, which has no summer time.
Note, however, that the time not comparable acress time zones any more and you
must compare only against times with time zone GMT.

Better switch session to a time zone without DTS (see \code{\link{setSessionCETWintertime}})}
\value{POSIXct vector in winter time (expressed as GMT, so actually offset to absolute world time)}

\author{Thomas Wutzler}



\seealso{\code{\link{strptimeNoSummer}}, \link{twMisc}}
\examples{
# this example was constructed for system time zone beeing "CET"
tmpd<- c("2010-03-28 00:00:00"
    ,"2010-03-28 01:00:00"
    ,"2010-03-28 02:00:00"
    ,"2010-03-28 03:00:00"
    ,"2010-03-28 04:00:00"
)
# note the shift from 1 to 3 in the first next line
(tmp.cet2 <- strptimeNoSummer(tmpd))
(tmp.winter <- convertToWinterUTC(tmp.cet2))
# note the wrong second- and third-last FALSE
tmp.winter < as.POSIXct("2010-03-28 04:00:00")
# correct is to compare to GMT based time
tmp.winter < as.POSIXct("2010-03-28 04:00:00",tz="GMT")
}
