checkAllClosest <- function(a,b,c,ind=seq_along(a)){
	i=3
	for( i in ind ){
		checkEquals( which.min( abs(b-a[i]) ), c[i] )
	}
	TRUE
}

test.exampleCase <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	b = sort(runif(length(a)))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c)
}

test.exampleCaseNLarge <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	b = sort(runif(length(a)*100))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~rep(a,each=100))
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c)
}

test.exampleCaseNSmall <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	b = sort(runif(length(a) %/% 2))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~rep(a[seq_along(a)%%2==0]))
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c)
}

test.missingMatch <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	b = sort(runif(length(a) %/% 2))
	b[1] <- b[2]
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE, maxAbsDiff=0.99*b[1] )
	
	plot(b~rep(a[seq_along(a)%%2==0]))
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkTrue(is.na(c[1]))
	checkAllClosest(a[is.finite(c)],b,c[is.finite(c)])
}


test.NAinA <- function(){
	a = seq(0,1,length.out=11)
	naPos <- c(1,3:5,11)
	a[naPos] <- NA
	set.seed(0815)
	b = sort(runif(length(a)))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkTrue( all(is.na(c[naPos])) )
	checkAllClosest(a,b,c,ind=which(is.finite(a) ))
}

test.NAinB <- function(){
	a = seq(0,1,length.out=11)
	naPos <- c(1,3:5,11)
	set.seed(0815)
	b = sort(runif(length(a)))
	b[naPos] <- NA
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c,ind=which(is.finite(a) ))
}

test.nonSortedA <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	a = sample(a)	# random permuation
	set.seed(0815)
	b = sort(runif(length(a)))
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=FALSE,isSortedB=TRUE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c)
}

test.nonSortedB <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	b = runif(length(a))	# without sort
	#mtrace(matchClosest)
	c = matchClosest(a,b,isSortedA=FALSE,isSortedB=FALSE)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a,b,c)
}

test.nonSortedAB <- function(){
	a = seq(0,1,length.out=11)
	set.seed(0815)
	a = sample(a)	# random permuation
	set.seed(0815)
	b = runif(length(a))	# without sort
	b[c(3,5)] <- NA
	#mtrace(matchClosest)
	c = matchClosest(a,b,maxAbsDiff=0.07)
	
	plot(b~a)
	points(b[c]~a, col="red", pch=2)
	abline(0,1)
	
	checkAllClosest(a[is.finite(c)],b,c[is.finite(c)])
}



