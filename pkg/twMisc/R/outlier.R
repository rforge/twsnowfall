detectOutliersByHampel <- function (
		### detect unusual local values, i.e. outliers, in univariate time series by The Hampel Filter  
		x		##<< numeric vector representing a time series
		,k=3	##<< window length 2*k+1 in indices, default to window length of 5
		,t0=3	##<< threshold, default is 3 (Pearson's rule), see below.
		, minWidth=as.integer(1.5*k)	##<< minimum number of records in window, to apply detection
){
	##details<< Based on hampel function of the pracma package
	# see http://www.r-bloggers.com/cleaning-time-series-and-other-data-streams/
	##details<<
	## The 'median absolute deviation' computation is done in the [-k...k] vicinity of each point 
	## at least k steps away from the end points of the interval. 
	## At the lower and upper end the time series values are preserved.
	## A high threshold makes the filter more forgiving, a low one will declare 
	## more points to be outliers. t0<-3 (the default) corresponds 
	## to Ron Pearson's 3 sigma edit rule, t0<-0 to John Tukey's median filter. 
	n <- length(x)
	y <- x
	isOutliers <- logical(n)
	L <- 1.4826
	for (i in (k + 1):(n-k)) {
		xWin <- na.omit( x[(i-k):(i + k)] )
		##details<<
		## In each window, NA-values are omitted. If the time resulting series 
		## is too short, no outlier detection for this index is performed.
		if( length(xWin) < minWidth) next 
		x0 <- median(xWin, na.rm=TRUE)
		S0 <- L * median(abs(xWin-x0), na.rm=TRUE)	# median absolute deviation to median
		if (abs(x[i]-x0) > t0 * S0) {
			y[i] <- x0
			isOutliers[i] <- TRUE
		}
	}
	##value<< A list with entires
	list(y = y			##<< the corrected time series
		, ind = which(isOutliers))	##<< the indices of detected outliers
}
attr(detectOutliersByHampel,"ex") <- function(){
	set.seed(0815)
	x <- diffinv(rnorm(99)/5)	# autocorrelated time series
	x[20:25] <- NA
	x[10] <- x[10] + median(abs(x[7:13]-x[10]))*3
	x[30] <- x[30] - median(abs(x[27:33]-x[30]))*3
	(iOut <- (res <- detectOutliersByHampel(x, k=4))$ind)
	(iOut <- (res <- detectOutliersBySuperSmoother(x))$ind)
	plot( x )
	points( x[iOut] ~ iOut, col="blue")
	points( res$y[iOut] ~ iOut, col="red")
	
	
}


detectOutliersBySuperSmoother <- function (
		### detect unusual local values, i.e. outliers, in univariate time series by The Hampel Filter  
		x		##<< numeric vector representing a time series
		,maxPropResid=2 ##<< minimum factor of resid/(interquantile-range/2) to be classified as outlier (with quantiles=(0.1,0.9))
		,...	##<< further arguments to \code{\link{supsmu}}, such as \code{bass}
){
	n <- length(x)
	##details<< NA values are first filled by linear interpolation before applying the smoother
	xFilled <- x
	iNA <- which(is.na(x))
	if( length(iNA) ) xFilled[iNA] <- approx(1:n,x, iNA)$y
	mod <- supsmu(1:n,xFilled)
	resid <- x - mod$y
	# plot(1:n, xFilled); lines(mod$y ~ mod$x )
	quantResid <- quantile(resid, prob=c(0.1,0.9), na.rm=TRUE)
	interQuartileSpread <- diff(quantResid)
	minAbsResid <- maxPropResid * interQuartileSpread/2
	propResid <- resid/(interQuartileSpread/2)
	# plot(abs(propResid))
	# plot(resid); abline(h=quantResid); abline( h=c(-1,1)*minAbsResid, lty="dashed" )
	ind <- which( abs(propResid) > maxPropResid )
	y <- x
	y[ind] <- mod$y[ind]
	##value<< A list with entires
	list(
			y = y		##<< the corrected time series (outliers replaced by smoothed value)
			,ind=ind	##<< the indices of detected outliers
			,ySmooth=mod$y	##<< the filled and smoothed time series
			,propResid=propResid	##<< resid/(interQuartileRange/2)
		)
}

.tmp.f <- function(){
	with(cars, {
				plot(speed, dist)
				lines(supsmu(speed, dist))
				lines(supsmu(speed, dist, bass = 7), lty = 2)
			})	
}
