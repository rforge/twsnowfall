matchClosest <- function(
	### Find index in b that is closest to a.
	a					##<< numeric vector 
	,b					##<< numeric vector
	,maxAbsDiff=Inf		##<< if maximum difference between best b and a[i] is larger than this, return NA for index i
	,isSortedA=FALSE	##<< if TRUE assures that A is sorted to speed up
	,isSortedB=FALSE	##<< if TRUE assures that B is sorted to speed up
	,allFiniteB=FALSE	##<< if TRUE assures that B contains only finite entries to speed up
	,chunkSize=round(length(b)/length(a)*1.6)	##<< length of the vector to calculate difference for each point in a
){
	##seealso<< \link{twMisc}
	
	##details<< 
	## For each a_i the c_i=j will be selected so that b_j is closest to a_i among all b
	## If a_i is not finite, then c_i is NA
	
	# first reduces to the case of ordered a and b having only finite values
	# then determine j by increasing j until difference of b[j]-a[i] is not larger than absolute difference for b[j+1] 
	
	# for matching only one value 
	# see http://markmail.org/search/list:r-project?q=match+closest#query:list%3Ar-project%20match%20closest+page:1+mid:mvuhpgkawy5ndgvl+state:results
	
	chunkSize <- max(3,chunkSize)		# must be at least two in order to compare next element, make it 3 to be faster for a and b of similar length
	if( !allFiniteB ){
		bOrig <- b
		bo <- is.finite(bOrig)
		b <- b[bo]
		finBCum <- cumsum(!bo)[bo]	# for each entry in reduced b, how much NA have been before
	}
	if(!isSortedA){
		orderA <- order(a)
		a <- a[orderA]
	} 
	if(!isSortedB){
		orderB <- order(b)
		b <- b[orderB]
	} 
	c <- a; c[] <- NA	# initialize the the result to NA
	j <- 1
	b <- c(b,b[length(b)])
	for( i in which(is.finite(a)) ){
		# if the next element is closer than discard the current one
		#while( (j < length(b)) && ({d0<-abs(b[j]-a[i]);d1<-abs(b[j+1]-a[i]);d1 < d0 }) ){
		#	j=j+1
		#}
		js <- j:min(length(b),j+chunkSize-1)
		absd <- abs(b[js]-a[i])
		jstep <- which.min(absd)
		# if jstep is the last element, look further else use the step			
		while( jstep == length(js) ){
			j=j+jstep-1
			js <- j:min(length(b),j+chunkSize-1)
			absd <- abs(b[js]-a[i])
			jstep <- which.min(absd)
		}
		j=j+jstep-1
		c[i] <- j
		if( absd[jstep] > maxAbsDiff ) c[i] = NA	#indicate non-found value
	}
	if(!isSortedB){
		c0 <- c
		c <- orderB[c0]
	} 
	if(!isSortedA){
		c0 <- c
		c[orderA] <- c0
	} 
	if( !allFiniteB ){
		c0 <- c
		c <- c0+finBCum[c0]
	} 
	c
	### vector of length of a, with each element holding an index from b
}


attr(matchClosest,"ex") <- function(){
	a = seq(0,1,length.out=11)
	b = sort(runif(length(a)))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
}

.back.matchClosest <- function(
	### Find index in b that is closest to a.
	a					##<< numeric vector 
	,b					##<< numeric vector
	,isSortedA=FALSE	##<< if TRUE assures that A is sorted to speed up
	,isSortedB=FALSE	##<< if TRUE assures that B is sorted to speed up
){
	##details<< 
	## For each a_i the c_i=j will be selected so that b_j is closest to a_i among all b
	## If a_i is not finite, then c_i is NA
	
	# for matching only one value 
	# see http://markmail.org/search/list:r-project?q=match+closest#query:list%3Ar-project%20match%20closest+page:1+mid:mvuhpgkawy5ndgvl+state:results
	
	if(!isSortedA){
		orderA <- order(a)
		a <- a[orderA]
	} 
	b <- b[is.finite(b)]
	if(!isSortedB) b <- sort(b)
	c <- a; c[] <- NA	# initialize the the result to NA
	j <- 1
	b <- c(b,NA)
	for( i in seq_along(a) ){
		if( is.finite(a[i]) ){
			# if the next element is closer than discard the current one
			while( (j < length(b)) && (
					!is.finite(d0<-abs(b[j]-a[i])) || (
						is.finite(d1<-abs(b[j+1]-a[i])) && (d1 < d0)
						)
					)
				){
				j=j+1
			}
			c[i] <- j
		}
	}
	c
	### vector of length of a, with each element holding an element from b
}







