test.twWhichColsEqual <- function(){
	X <- matrix(rep(1:4,each=3),nrow=3)	
	X[,4] = X[,1]
	z <- rep(2,3)
	res <- twWhichColsEqual(X,z)
	checkEquals(2, res )
	z <- rep(1,3)
	res <- twWhichColsEqual(X,z)
	checkEquals(c(1,4), res )
	Z <- X
	Z[2,4] <- NA
	res <- twWhichColsEqual(X,Z)
	checkEquals(c(1,2,3), res )
	
	res <- twWhichColsEqual(Z,Z)
	checkEquals(1:4,res )
}

