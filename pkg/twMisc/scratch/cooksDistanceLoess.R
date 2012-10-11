cooksDistance.loess <- function(
		### calculate cooksDistance for loess models
		formula		##<< parameters for \code{\link{loess}}
		,data
		,...
##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	mf <- model.frame(formula,data, na.action=NULL)
	i <- which( !apply(mf, 1, function(row) any(is.na(row)) ))
	mfComplete <- na.omit(mf) 
	x <- loess(formula, mfComplete,...)
	nP <- x$enp
	mse <- x$s
	denom <- nP*mse
	#iOut <- 1
	y <- fitted(x)
	cdist <- rep( NA, nrow(data) )
	cdist[i] <- sapply( 1:nrow(mfComplete), function(iOut){
				yi <- fitted(loess( formula, mfComplete[-iOut,], ...))
				sum( (y[-iOut] - yi)^2 ) / denom
			})
	##value<< list with elements
	list( mod=x				##<< the fitted loess model
			, cdist=cdist)		##<< numeric vector of cooks distance
	##end<<
}
attr(cooksDistance.loess,"ex") <- function(){
	tmp <- seq(1,350,length.out=9)
	nPar <-  
			data <- data.frame( 
					time = tmp
					,fluo = log(tmp) + rnorm(length(tmp), sd=max(log(tmp)/5))
			)
	plot(fluo ~ time, data)
	res <- cooksDistance.loess( fluo ~ time, data, span=0.9)
	lines( fitted(res$mod) ~ time, data)
	plot(res$cdist, ylab="cooks distance")
}
