#twUtest(twBinOptimize)

setMethodS3("twBinOptimize","numeric", function( 
	### Finding the index of a sorted numeric vector whose element is closest to target. 
	x			##<< numeric vector to be optimized. The term abs(x[i])-target) is minimized.
	,target=0	##<< Target value for \code{x}. Defaults to 0.
	, ...		##<< not used
	, interval=c(1,length(x)) 	##<< a vector containing the end-points of the interval to be searched for target.
	,lower=ceiling(min(interval))	##<< the lower end point of the interval to be searched.
	,upper=floor(max(interval))		##<< the upper end point of the interval to be searched.
	,maxiter=100		##<< Maximum number of search iterations. Defaults to 100.
	,showiter=FALSE		##<< Boolean flag indicating whether the algorithm state should be printed at each iteration. Defaults to FALSE.
){
	#twBinOptimize.numeric
		
	##seealso<< \code{\link{twBinOptimize.function}}, \code{\link{twMisc}}
	
	##details<< 
	## This function can be applied similar to \code{\link{optimize}} for cases, where the argument
	## to be optimized is an index instead of a contnuous variable. 
	## A binary search over the index is performed.
	## \cr The function \code{x} must be monotonic. If it is not strictly monotonic the returned
	## index (\code{$where}) can be any of the indices with equal values. 
	## \cr The code is based on binsearch of the gtools package. In difference to the original code, 
	## it returns always only one best estimate.
	
	# initialize
	#retval <- list( call=match.call(), numiter=0, flag="not initialized", where=as.integer(NA), value=as.numeric(NA) )
	retval <- list( numiter=0, flag="not initialized", where=as.integer(NA), value=as.numeric(NA) )
	if( 0==length(x)){
		retval$flag<-"Empty vector to search."
		warning("twBinOptimize: Empty vector to search.")
		return(retval)
	}
	if( upper < lower){
		retval$flag<-"Upper boundary of interval is lower than lower boundary"
		warning("twBinOptimize: Upper boundary of interval is lower than lower boundary.")
		return(retval)
	} 
	lo <- lower
	hi <- upper
	counter <- 0
	val.lo <- x[lo]
	val.hi <- x[hi]
	
	# check whether function is increasing or decreasing, & set sign
	# appropriately.
	if( val.lo > val.hi )
		sign <- -1
	else
		sign <- 1
	
	# check if value is outside specified range
	if(target * sign < val.lo * sign)
		outside.range <- TRUE
	else if(target * sign >  val.hi * sign)
		outside.range <- TRUE
	else
		outside.range <- FALSE
	
	# iteratively move lo & high closer together until we run out of
	# iterations, or they are adjacent, or they are identical
	while(counter < maxiter && !outside.range ){
		counter <- counter+1
		if(hi-lo<=1 || lo<lower || hi>upper) break;
		if((hi-lo)<=1e4){
			retval$where <- lo-1+which.min(abs(x[lo:hi]-target))
			retval$value <- x[retval$where] 
			retval$numiter <- counter
			return(retval)
		}
		center <- round((hi - lo)/2 + lo ,0  )
		val <- x[center]
		
		if(showiter){
			cat("--------------\n")
			cat("Iteration #", counter, "\n")
			cat("lo=",lo,"\n")
			cat("hi=",hi,"\n")
			cat("center=",center,"\n")
			cat("x(lo)=",val.lo,"\n")
			cat("x(hi)=",val.hi,"\n")
			cat("x(center)=",val,"\n")
		}
		
		if( val==target ){
			val.lo <- val.hi <- val
			lo <- hi <- center
			break;
		}else if( sign*val < sign*target ){
			lo <- center
			val.lo <- val
		}else{ #( val > target )
			hi <- center
			val.hi <- val
		}

		if(showiter){
			cat("new lo=",lo,"\n")
			cat("new hi=",hi,"\n")
			cat("--------------\n")
		}
	}# while
	
	# Create return value
	retval$numiter <- counter
	
	if( outside.range ){
		if(target * sign < val.lo * sign){
			#warning("Reached lower boundary")
			retval$flag="Lower Boundary"
			retval$where=lo
			retval$value=val.lo
		}else{ #(target * sign >  val.hi * sign)	
			#warning("Reached upper boundary")
			retval$flag="Upper Boundary"
			retval$where=hi
			retval$value=val.hi
		}
	}
	else if( val.lo==target ){
		retval$flag="Found"
		retval$where=lo
		retval$value=val.lo
	}else if( val.hi==target ){
		retval$flag="Found"
		retval$where=lo
		retval$value=val.lo
	}else{
		retval$flag="Between Elements"
		if( abs(val.lo-target) <= abs(val.hi-target) ){
			retval$where=lo
			retval$value=val.lo
		}else{
			retval$where=hi
			retval$value=val.hi
		}
	}
	
	if( counter >= maxiter ){
		warning("Maximum number of iterations reached")
		retval$flag="Maximum number of iterations reached"
	}
	
	return(retval)
	### A list containing:
	### \item{call}{How the function was called.}
	### \item{numiter}{The number of iterations performed}
	### \item{flag }{One of the strings,  "Found", "Between Elements",
	###     "Maximum number of iterations reached", "Reached lower boundary", or
	###     "Reached upper boundary."}
	### \item{where}{One or two values indicating where the search terminated.}
	### \item{value}{Value of the \code{x} at the index of \code{where}.}
	### If vector is empty or upper boundary is lower than lower boundary, \code{where} and \code{value} are NA
})
attr(twBinOptimize.numeric,"ex") <- function(){
	# linear search is faster up with vectors to about 1e4 entries 
	x <- exp(seq(-10,100,length.out=1e4))
	# with longer vectors, the binary search is superior
	x <- exp(seq(-10,100,length.out=1e6))
	
	# generate some sample indices that will be found by optimization
	n <- 1e2
	.where <- sample.int(length(x),n, replace=TRUE)
	.val <- x[.where]*1.00001	# add some error, for exact matches use the faster function match
	system.time( .where2 <- sapply(.val, function(vali){ which.min(abs(x-vali))  } ))	
	system.time( .where3 <- sapply(.val, function(vali){ twBinOptimize(x,vali)$where} ))
	#Rprof();  .numiter3 <- sapply(.val, function(vali){ twBinOptimize(x,vali)$numiter} ); Rprof(NULL);	summaryRprof()
}		



setMethodS3("twBinOptimize","function", function( 
		### Optimizing a function where first argument is an index. 
		x			##<< the monotonic vectorized function to be optimized. The term abs(x(i,...)-target) is minimized.
		,target=0	##<< Target value for \code{x}. Defaults to 0.
		, ...		##<< additional named or unnamed arguments to be passed to f.
		, interval 	##<< a vector containing the end-points of the interval to be searched for target.
		,lower=ceiling(min(interval))	##<< the lower end point of the interval to be searched.
		,upper=floor(max(interval))		##<< the upper end point of the interval to be searched.
		,maxiter=100		##<< Maximum number of search iterations. Defaults to 100.
		,showiter=FALSE		##<< Boolean flag indicating whether the algorithm state should be printed at each iteration. Defaults to FALSE.
	){
		# twBinOptimize.function
		##seealso<< twMisc
		
		##details<< 
		## This function can be applied similar to \code{\link{optimize}} for cases, where the argument
		## to be optimized is an index instead of a contnuous variable. 
		## A binary search over the index is performed.
		## \cr The function \code{x} must be monotonic. If it is not strictly monotonic the returned
		## index (\code{$where}) can be any of the indices with equal values. 
		## \cr The code is based on binsearch of the gtools package. In difference to the original code, 
		## it returns always only one best estimate.
		
		# initialize
		retval <- list( call=match.call(), numiter=0, flag="not initialized", where=as.integer(NA), value=as.numeric(NA) )
		if( upper < lower){
			retval$flag<-"Upper boundary of interval is lower than lower boundary"
			warning("twBinOptimize: Upper boundary of interval is lower than lower boundary.")
			return(retval)
		} 
		
		lo <- lower
		hi <- upper
		counter <- 0
		val.lo <- x(lo,...)
		val.hi <- x(hi,...)
		
		# check whether function is increasing or decreasing, & set sign
		# appropriately.
		if( val.lo > val.hi )
			sign <- -1
		else
			sign <- 1
		
		# check if value is outside specified range
		if(target * sign < val.lo * sign)
			outside.range <- TRUE
		else if(target * sign >  val.hi * sign)
			outside.range <- TRUE
		else
			outside.range <- FALSE
		
		# iteratively move lo & high closer together until we run out of
		# iterations, or they are adjacent, or they are identical
		while(counter < maxiter && !outside.range ){
			counter <- counter+1
			if(hi-lo<=1 || lo<lower || hi>upper) break;
			if((hi-lo)<=1e4){
				retval$where <- lo-1+which.min(abs(x(lo:hi,...)-target))
				retval$value <- x(retval$where,...) 
				retval$numiter <- counter
				return(retval)
			}
			center <- round((hi - lo)/2 + lo ,0  )
			val <- x(center, ...)
			
			if(showiter){
				cat("--------------\n")
				cat("Iteration #", counter, "\n")
				cat("lo=",lo,"\n")
				cat("hi=",hi,"\n")
				cat("center=",center,"\n")
				cat("x(lo)=",val.lo,"\n")
				cat("x(hi)=",val.hi,"\n")
				cat("x(center)=",val,"\n")
			}
			
			if( val==target ){
				val.lo <- val.hi <- val
				lo <- hi <- center
				break;
			}else if( sign*val < sign*target ){
				lo <- center
				val.lo <- val
			}else{ #( val > target )
				hi <- center
				val.hi <- val
			}
			
			if(showiter){
				cat("new lo=",lo,"\n")
				cat("new hi=",hi,"\n")
				cat("--------------\n")
			}
		}# while
		
		# Create return value
		retval$numiter <- counter
		
		if( outside.range ){
			if(target * sign < val.lo * sign){
				warning("Reached lower boundary")
				retval$flag="Lower Boundary"
				retval$where=lo
				retval$value=val.lo
			}else{ #(target * sign >  val.hi * sign)	
				warning("Reached upper boundary")
				retval$flag="Upper Boundary"
				retval$where=hi
				retval$value=val.hi
			}
		}
		else if( val.lo==target ){
			retval$flag="Found"
			retval$where=lo
			retval$value=val.lo
		}else if( val.hi==target ){
			retval$flag="Found"
			retval$where=lo
			retval$value=val.lo
		}else{
			retval$flag="Between Elements"
			if( abs(val.lo-target) <= abs(val.hi-target) ){
				retval$where=lo
				retval$value=val.lo
			}else{
				retval$where=hi
				retval$value=val.hi
			}
		}
		
		if( counter >= maxiter ){
			warning("twBinOptimize: Maximum number of iterations reached")
			retval$flag="Maximum number of iterations reached"
		}
		
		return(retval)
		### A list containing:
		### \item{call}{How the function was called.}
		### \item{numiter}{The number of iterations performed}
		### \item{flag }{One of the strings,  "Found", "Between Elements",
		###     "Maximum number of iterations reached", "Reached lower boundary", or
		###     "Reached upper boundary."}
		### \item{where}{One or two values indicating where the search terminated.}
		### \item{value}{Value of the function \code{x} at the values of \code{where}.}
	})





