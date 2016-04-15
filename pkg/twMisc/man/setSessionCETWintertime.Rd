\name{setSessionCETWintertime}
\alias{setSessionCETWintertime}
\title{setSessionCETWintertime}
\description{switches session to Central European time, without daylight saving times (DTS)}
\usage{setSessionCETWintertime()}
\details{Often, logger data is recorded without notion of daylight time savings.
When reading csv-files, or querying DATETIME database columns, the dates get converted
to POSIXct using sessions time zone. If this is set to CET, then there are problems:
The summer times are one hour off from the logger
If a invalid time is encountered (lost hour during swith to summer time) then all 
times are reset to midnight.

So set session time zone to west african time (WAT, Africa/Kinshasa), that corresponds
to CET but does not observe DTS.}


\author{Thomas Wutzler}





