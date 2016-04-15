\name{format.POSIXctz}
\alias{format.POSIXctz}
\title{format POSIXctz}
\description{appends the gmt offset to the formatted string  }
\usage{\method{format}{POSIXctz}(x, ...)}
\arguments{
  \item{x}{vector of POSIXctz to be printed}
  \item{\dots}{further arguments to \code{\link{format.POSIXct}}}
}
\details{Formats the time according to \code{\link{format.POSIXct}}
and adds a space and the \code{$gmtoff} of POSIXlt
in a format that conforms to \%z in \code{\link{strptime}} }


\author{Thomas Wutzler}




\examples{
tmpd<- c("2010-03-28 00:00:00"
        ,"2010-03-28 01:00:00"
        ,"2010-03-28 02:00:00"
        ,"2010-03-28 03:00:00"
        ,"2010-03-28 04:00:00"
)
# third value is not defined when swithing tu CEST
tmp.cetz <- as.POSIXct(tmp.lt <- strptime(paste0(tmpd," +0100"),'\%Y-\%m-\%d \%H:\%M:\%S  \%z'), tz="CET")
tmp.cetz
as.POSIXctz(tmp.cetz)   # note how the gmt.offset is printed
(tmpDs <- data.frame( timestamp=as.POSIXctz(tmp.cetz) )) # note how the time offset is printed
#write.csv( tmpDs, "tmp/testTimestamp1.csv", row.names = FALSE)
# str(res <- read.csv("tmp/testTimestamp1.csv", colClasses=(timestamp="POSIXct")))
# res$timestamp
#
# test writing in a different time zone
(tmp.cet2 <- structure(tmp.cetz, tzone="EET"))
#trace(format.POSIXctz, recover)
as.POSIXctz(tmp.cet2)
(tmpDs2 <- data.frame( timestamp=as.POSIXctz(tmp.cet2) )) 
#write.csv( tmpDs2, "tmp/testTimestamp2.csv", row.names = FALSE)
# read.csv("tmp/testTimestamp2.csv", colClasses=(timestamp="POSIXct"))[1,1] # does not work
# res<-  read.csv("tmp/testTimestamp2.csv", colClasses=(timestamp="POSIXctz")); res$timestamp

}
