strptimeNoSummer <- function(
    ### Parse time given as character with ignoring daylight savings.
    x       ##<< character vector holing the times
    ,format='%Y-%m-%d %H:%M:%S'     ##<< format argument to \code{\link{strptime}} such as \code{format="%d.%m.%y %H:%M"}
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
    
    # problems with daylight savings
    # R automatically switches between CET and CEST 
    (tmp.cet <- as.POSIXct(strptime(tmpd,'%Y-%m-%d %H:%M:%S',tz="CET")))
    
    # printed in summer time 
    (tmp.cet2 <- strptimeNoSummer(tmpd))
    # note the missing hour 2, however time differences are preserved
    plot(seq_along(tmp.cet2) ~ tmp.cet2)
    diff(tmp.cet2)
    
    # if time bias is not an issue, one may parse in UTC 
    (tmp.UTC <- as.POSIXct(strptime(tmpd,'%Y-%m-%d %H:%M:%S',tz="UTC")))
    plot(seq_along(tmp.UTC) ~ tmp.UTC)
    
    # or use the time offset field in POSIXlt
    tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'), tz="CET")
    #tmp.cetz <- as.POSIXct(strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'))
    plot(seq_along(tmp.cetz) ~ tmp.cetz)
    all(tmp.cetz == tmp.cet2)
}

convertToWinterUTC <- function(
    ### convert series to winter time, i.e. expressed as GMT
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

tmp.f <- function(){
    now = Sys.time()
    seq(0,60*24-3, by=3)         # all three minutes intervals
    (roundSec( as.POSIXct("2014-10-15 15:10:00") ) - as.POSIXct("2014-10-15 15:10:00")) / 60
}


readCsvWintertime <- function(
        ### Read a data loggger .dat file into a data-frame		
        fName					##<< scalar string: file name
        ,...					##<< further arguments to read.csv
        ,colClasses = NA	##<< see \code{link{read.table}}
        ,colsTimeStamp=1		##<< integer vector: colums with time stamp column (will be set to POSIXct
        ,formatTS="%Y-%m-%d %H:%M:%S"	##<< format of the timestamp columns, see \code{\link{strptime}}, e.g. 
        ,tz="CET"				##<< specify a time zone when converting to POSIXct, default: current local e.g CET, UTC
        ,offsetUTC="+0100"	    ##<< String: Signed offset in hours and minutes from UTC
            ##<<, so -0800 is 8 hours behind UTC. see \code{\link{strptime}} 

){
    ##details<< 
    ## Assumes that there
    setClass("POXIXctz", where=globalenv())
    setAs("character","myDate", function(from) as.POSIXct(from, format=formatTS, tz=tz), where=globalenv() )
    fileInfo <- readLines(fName, n=nRowsFileInfo )
    colInfo <- read.table(fName, header=TRUE, skip=nRowsFileInfo, nrows=max(1,nRowsColInfo), sep=sep, na.strings=na.strings)
    colClasses[colsTimeStamp] <- "myDate"
    rawData <- read.table(fName, header=FALSE, skip=nRowsFileInfo+1+nRowsColInfo, sep=sep, na.strings=na.strings, ...
            ,colClasses=colClasses)
    colnames(rawData) <- colnames(colInfo)	
    #plot( CO2_Avg ~ TIMESTAMP, data=rawData )
    attr(rawData,"fileInfo") <- fileInfo
    attr(rawData,"colInfo") <- colInfo
    rawData
}
attr(readCsvWintertime,"ex") <- function(){
    # see http://pitfalls-r-us.blogspot.de/2012/07/time-zones.html
    tmpd<- c("2010-03-28 00:00:00"
            ,"2010-03-28 01:00:00"
            ,"2010-03-28 02:00:00"
            ,"2010-03-28 03:00:00"
            ,"2010-03-28 04:00:00"
    )
    # third value is not defined when swithing tu CEST
    tmp.cetz <- as.POSIXct(tmp.lt <- strptime(paste0(tmpd," +0100"),'%Y-%m-%d %H:%M:%S  %z'), tz="CET")
    tmp.lt$gmtoff
    as.POSIXlt(tmp.cetz)$gmtoff
    fName <- tempfile("readCsvWintertime.csv")
    #fName <- "tmp/readCsvWintertime.csv"
    tmp.cetz2 <- structure(tmp.cetz, tzone="UTC")
    
    write.csv(data.frame(timestamp=tmp.cetz), fName )
    
    
    
    ds <- read.csv( file="tmp/timestamps1.csv",as.is=TRUE)
    ds$ts2 <- strptime( format="%d-%m-%Y %H:%M:%S %z",
                    ds$timestamp, tz="UTC" )
    fName <- system.file("genData/chamberLoggerEx1_short.dat", package = "RespChamberProc")
    if( nzchar(fName) ){
        ds <- readDat(fName)
    }
}

as.POSIXctz <- function(
    ### convert to POSIXctz
    x
    ,...
){
    ##details<< 
    ## calls \code{\link{as.POSIXct}} and prepends the "POSIXctz" attribute 
    y <- as.POSIXct(x,...)
    class(y) <- c("POSIXctz", class(y) )
    y
}

setClass("POSIXctz")
setAs("character","POSIXctz", function(from){
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





