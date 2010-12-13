strptimeNoSummer <- function(
	### Parse time given as character with ignoring daylight savings.
	x		##<< character vector holing the times
	,format='%Y-%m-%d %H:%M:%S'		##<< format argument to \code{\link{strptime}} such as \code{format="%d.%m.%y %H:%M"}
	, hourOffset=-1					##<< offset of measurement time zone from GMT in hours
	, tz="CET"						##<< time zone attribute for printing
	, ... 					##<< futher arguments to \code{\link{strptime}} such as \code{format="%d.%m.%y %H:%M"}
){
	##seealso<< \link{twMisc}
	
	##details<< \describe{\item{Further time Functionality of package twMisc}{
	## \itemize{
	## \item parsing a character that is all winter time: this method 
	## \item convert to all winter time (GMT): \code{\link{convertToWinterGMT}}
	## }
	##}}
	
	##details<< 
	## The String is parsed using time zone GMT, where not daylight shifts occur. 
	## Next the hour offset is added, and the tzone attribute is changed.
	tmp.gmt <- as.POSIXct(strptime(x,format=format,tz="GMT",...))
	tmp.cet <- tmp.gmt + 3600*hourOffset
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
	
	# if time bias is not an issue, one may parse in GMT 
	(tmp.gmt <- as.POSIXct(strptime(tmpd,'%Y-%m-%d %H:%M:%S',tz="GMT")))
	plot(seq_along(tmp.gmt) ~ tmp.gmt)
}

convertToWinterGMT <- function(
	### convert series to winter time, i.e. expressed as GMT
	x				##<< POSIXct vector using summer time
	,hourOffset=-1	##<< offset of the original time zone winter time
){
	##seealso<< \code{\link{strptimeNoSummer}}, \link{twMisc}
	
	##details<< 
	## R plots time zones with summer and winter time automatically swithing 
	## to summer time. This is annoying in plots of hours across the time shift.
	## Remedy is to express hours in GMT, which has no summer time.
	## Note, however, that the time not comparable acress time zones any more and you
	## must compare only against times with time zone GMT. 
	attr(x,"tzone") <- "GMT"
	x - 3600*hourOffset
	### POSIXct vector in winter time (expressed as GMT, so actually offset to absolute world time)
}
attr(convertToWinterGMT,"ex") <- function(){
	# this example was constructed for system time zone beeing "CET"
	tmpd<- c("2010-03-28 00:00:00"
		,"2010-03-28 01:00:00"
		,"2010-03-28 02:00:00"
		,"2010-03-28 03:00:00"
		,"2010-03-28 04:00:00"
	)
	# note the shift from 1 to 3 in the first next line
	(tmp.cet2 <- strptimeNoSummer(tmpd))
	(tmp.winter <- convertToWinterGMT(tmp.cet2))
	# note the wrong second- and third-last FALSE
	tmp.winter < as.POSIXct("2010-03-28 04:00:00")
	# correct is to compare to GMT based time
	tmp.winter < as.POSIXct("2010-03-28 04:00:00",tz="GMT")
}

