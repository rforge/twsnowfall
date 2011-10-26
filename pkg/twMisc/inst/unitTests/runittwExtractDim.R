test.one <- function(){
	A0 <- matrix(1:8,ncol=2) 
	A <- abind( lapply(1:4, function(i){(i)*10+A0}), rev.along=0 )
	A3 <- abind( lapply(1:3, function(i){(i)*100+A}), rev.along=0 )
	(res <- twExtractDim(A,2) )
	checkEquals( A[,,2], res )
	#mtrace(twExtractDim)
	(res <- twExtractDim(A,2,iDim=2) )
	checkEquals( A[,2,], res )
	(res <- twExtractDim(A3,2,iDim=2) )
	checkEquals( A3[,2,,], res )
	(res <- twExtractDim(A3,2,iDim=3) )
	checkEquals( A3[,,2,], res )
	
	resl <- twListArrDim(A)
	#str(resl)
	checkEquals(4, length(resl) )
	checkEquals( A[,,3], resl[[3]])
	checkEquals( A[,,4], resl[[4]])
	
	resl <- twListArrDim(A,2)
	#str(resl)
	checkEquals(2, length(resl) )
	checkEquals( A[,1,], resl[[1]])
	checkEquals( A[,2,], resl[[2]])
}

ztest.extractLastDims <- function(){
	# example of extracting from a matrix 
	(A <- matrix(1:6, ncol=2))
	(Aext <- abind( lapply(1:4, function(i){(i)/10+A}), along=0 ))
	# Note that the second and third dimension of Aext correspond to A 
	
	# Now we whish to extract from Aext based on 
	# a criterion for the second and third dimension 
	(B <- diag(3)[,1:2])
	i <- which(B != 0)
	(A[i])
	# how to index Aext to obtain those indices?
	resExp <- matrix(0,nrow=nrow(Aext),ncol=length(i) )
	for( ii in seq(along.with=i) )
		resExp[,ii] <- Aext[,(i[ii]-1) %% 3+1,(i[ii]-1) %/% 3+1]
	(res <- twExtractFromLastDims(Aext,i))
	checkEquals( resExp, structure(res, dimnames=NULL) )
}