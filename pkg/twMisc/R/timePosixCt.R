setSessionCETWintertime <- function(
### switches session to Central European time, without daylight saving times (DTS)
){
    ##details<< 
    ## Often, logger data is recorded without notion of daylight time savings.
    ## When reading csv-files, or querying DATETIME database columns, the dates get converted
    ## to POSIXct using sessions time zone. If this is set to CET, then there are problems:
    ## The summer times are one hour off from the logger
    ## If a invalid time is encountered (lost hour during swith to summer time) then all 
    ## times are reset to midnight.
    ##
    ## So set session time zone to west african time (WAT, Africa/Kinshasa), that corresponds
    ## to CET but does not observe DTS.
    Sys.setenv(TZ='Africa/Kinshasa')
}


strptimeNoSummer <- function(
    ### deprecated: Parse time given as character with ignoring daylight savings.
    x       ##<< character vector holing the times
    ,format                         ##<< format argument to \code{\link{strptime}} such as \code{format="%d.%m.%y %H:%M"}
    , hourOffset=-1                 ##<< offset of measurement time zone from UTC in hours
    , tz="CET"                      ##<< time zone attribute for printing
    , ...                   ##<< futher arguments to \code{\link{strptime}} such as \code{format="%d.%m.%y %H:%M"}
){
    ##seealso<< \link{twMisc}
    
    ##details<< \describe{\item{Further time Functionality of package twMisc}{
    ## \itemize{
    ## \item parsing a character that is all winter time: this method 
    ## \item convert to all winter time (UTC): \code{\link{convertToWinterUTC}}
    ## }
    ##}}
    
    ##details<< 
    ## The String is parsed using time zone UTC, where not daylight shifts occur. 
    ## Next the hour offset is added, and the tzone attribute is changed.
    ##
    ## Better uses a corresponding time zone withotu DTS (e.g. 'Etc/GMT-1' instead of CET)
    ## (see example)
    if( missing(format) ) format<-'%Y-%m-%d %H:%M:%S'	# cannot put to default argument, because %sign in Rd
    tmp.UTC <- as.POSIXct(strptime(x,format=format,tz="UTC",...))
    tmp.cet <- tmp.UTC + 3600*hourOffset
    attr(tmp.cet,"tzone") <- tz
    tmp.cet
    ### POSIXct vector, printing in summer time during summer. 
}
attr(strptimeNoSummer,"ex") <- function(){
    # time series all recored in CET withoug switching to summer time between 2 and 3
    tmpd<- c("2010-03-28 00:00:00"
        ,"2010-03-28 01:00:00"
        ,"2010-03-28 02:00:00"
        ,"2010-03-28 03:00:00"
        ,"2010-03-28 04:00:00"
    )
    
    # recommended solution: parse in UTC or 'Etc/GMT-1'
    (tmp.GMT1 <- as.POSIXct(strptime(tmpd,'%Y-%m-%d %H:%M:%S',tz="Etc/GMT-1")))
    plot(seq_along(tmp.GMT1) ~ tmp.GMT1)
    
    
    # problems with daylight savings
    # R automatically switches between CET and CEST 
    (tmp.cet <- as.POSIXct(strptime(tmpd,'%Y-%m-%d %H:%M:%S',tz="CET")))
    
    # printed in summer time 
    (tmp.cet2 <- strptimeNoSummer(tmpd))
    # note the missing hour 2, however time differences are preserved
    plot(seq_along(tmp.cet2) ~ tmp.cet2)
    diff(tmp.cet2)
    
    # or use the time offset field in POSIXlt
    tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'), tz="CET")
    #tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'))
    plot(seq_along(tmp.cetz) ~ tmp.cetz)
    all(tmp.cetz == tmp.cet2)
}

convertToWinterUTC <- function(
    ### deprecated: convert series to winter time, i.e. expressed as GMT
    x               ##<< POSIXct vector using summer time
    ,hourOffset=-1  ##<< offset of the original time zone winter time
){
    ##seealso<< \code{\link{strptimeNoSummer}}, \link{twMisc}
    
    ##details<< 
    ## R plots time zones with summer and winter time automatically switching 
    ## to summer time. This is annoying in plots of hours across the time shift.
    ## Remedy is to express hours in GMT, which has no summer time.
    ## Note, however, that the time not comparable acress time zones any more and you
    ## must compare only against times with time zone GMT.
    ##
    ## Better switch session to a time zone without DTS (see \code{\link{setSessionCETWintertime}})
    attr(x,"tzone") <- "GMT"
    x - 3600*hourOffset
    ### POSIXct vector in winter time (expressed as GMT, so actually offset to absolute world time)
}
attr(convertToWinterUTC,"ex") <- function(){
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


roundSec <- function(
        ### round times to given time interval
        x           ##<< vector of POSIXct, times to round
        ,sec = 180  ##<< rounding interval, default each 180 seconds, i.e. 3 minutes 
        #,offset = as.numeric(as.POSIXct("2014-01-01 00:00:00")) ##<< substracted before doing numerics to avoid numerical errors
){
    .POSIXct(
            #floor((as.numeric(testTimes)-offset +90) / 180.0) * 180.0 +offset
            floor((as.numeric(x) +(sec/2)) / sec) * sec 
    )
}
attr(roundSec, "ex") <- function(){
    midnight <- as.POSIXct("2014-12-09 00:00:00")
    testTimes <- midnight +60*c(-1.51,-1.5,0,1.49,1.5,1.51,4.49)
    testTimes
    (rTestTimes <- roundSec(testTimes))
    # getDaysMinute(rTestTimes)
    roundSec(testTimes, sec=5*60)
}

.tmp.f <- function(){
    now = Sys.time()
    seq(0,60*24-3, by=3)         # all three minutes intervals
    (roundSec( as.POSIXct("2014-10-15 15:10:00") ) - as.POSIXct("2014-10-15 15:10:00")) / 60
}



as.POSIXctz <- function(
    ### convert to POSIXctz
    x           ##<< object that can be coerced by \code{\link{as.POSIXct}}
    ,...        ##<< further arguments to \code{\link{as.POSIXct}}
){
    ##details<< 
    ## calls \code{\link{as.POSIXct}} and prepends the "POSIXctz" attribute 
    y <- as.POSIXct(x,...)
    class(y) <- c("POSIXctz", class(y) )
    y
}

setClass("POSIXctz")
setAs("character","POSIXctz", function(
        ### convert character to POSIXctz
        from    ##<< object passed to \code{\link{as.POSIXctz}}
    ){
        as.POSIXctz(from, format="%Y-%m-%d %H:%M:%S %z")
    }  )


format.POSIXctz <- function(
    ### appends the gmt offset to the formatted string  
    x               ##<< vector of POSIXctz to be printed
    , ...           ##<< further arguments to \code{\link{format.POSIXct}}
){
    ##details<< 
    ## Formats the time according to \code{\link{format.POSIXct}}
    ## and adds a space and the \code{$gmtoff} of POSIXlt
    ## in a format that conforms to %z in \code{\link{strptime}} 
    o <- as.POSIXlt(x)$gmtoff
    if( is.null(o) ) o <- 0
    hourOffset <- ifelse( o >= 0
        , paste0( " +", sprintf("%02d", o/3600 ), "00" )
        , paste0( " ", sprintf("%03d", o/3600 ), "00" )
    )
    paste0( format.POSIXct(x, ...), hourOffset )
}
attr(format.POSIXctz, "ex") <- function(){
    tmpd<- c("2010-03-28 00:00:00"
            ,"2010-03-28 01:00:00"
            ,"2010-03-28 02:00:00"
            ,"2010-03-28 03:00:00"
            ,"2010-03-28 04:00:00"
    )
    # third value is not defined when swithing tu CEST
    tmp.cetz <- as.POSIXct(tmp.lt <- strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'), tz="CET")
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





