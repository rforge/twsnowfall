\name{strptimeNoSummer}
\alias{strptimeNoSummer}
\title{strptimeNoSummer}
\description{deprecated: Parse time given as character with ignoring daylight savings.}
\usage{strptimeNoSummer(x, format, hourOffset = -1, tz = "CET", 
    ...)}
\arguments{
  \item{x}{character vector holing the times}
  \item{format}{format argument to \code{\link{strptime}} such as \code{format="\%d.\%m.\%y \%H:\%M"}}
  \item{hourOffset}{offset of measurement time zone from UTC in hours}
  \item{tz}{time zone attribute for printing}
  \item{\dots}{futher arguments to \code{\link{strptime}} such as \code{format="\%d.\%m.\%y \%H:\%M"}}
}
\details{\describe{\item{Further time Functionality of package twMisc}{
\itemize{
\item parsing a character that is all winter time: this method 
\item convert to all winter time (UTC): \code{\link{convertToWinterUTC}}
}
}}

The String is parsed using time zone UTC, where not daylight shifts occur. 
Next the hour offset is added, and the tzone attribute is changed.

Better uses a corresponding time zone withotu DTS (e.g. 'Etc/GMT-1' instead of CET)
(see example)}
\value{POSIXct vector, printing in summer time during summer. }

\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
# time series all recored in CET withoug switching to summer time between 2 and 3
tmpd<- c("2010-03-28 00:00:00"
    ,"2010-03-28 01:00:00"
    ,"2010-03-28 02:00:00"
    ,"2010-03-28 03:00:00"
    ,"2010-03-28 04:00:00"
)

# recommended solution: parse in UTC or 'Etc/GMT-1'
(tmp.GMT1 <- as.POSIXct(strptime(tmpd,'\%Y-\%m-\%d \%H:\%M:\%S',tz="Etc/GMT-1")))
plot(seq_along(tmp.GMT1) ~ tmp.GMT1)


# problems with daylight savings
# R automatically switches between CET and CEST 
(tmp.cet <- as.POSIXct(strptime(tmpd,'\%Y-\%m-\%d \%H:\%M:\%S',tz="CET")))

# printed in summer time 
(tmp.cet2 <- strptimeNoSummer(tmpd))
# note the missing hour 2, however time differences are preserved
plot(seq_along(tmp.cet2) ~ tmp.cet2)
diff(tmp.cet2)

# or use the time offset field in POSIXlt
tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'\%Y-\%m-\%d \%H:\%M:\%S  \%z'), tz="CET")
#tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'\%Y-\%m-\%d \%H:\%M:\%S  \%z'))
plot(seq_along(tmp.cetz) ~ tmp.cetz)
all(tmp.cetz == tmp.cet2)
}
