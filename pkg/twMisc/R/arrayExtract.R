twExtractDim <- function(
    ### Extract \code{A[...,i,...]} from dimension \code{iDim} of an array 
    A           ##<< the array to extract values from
    ,i=1        ##<< the index to extract
    ,iDim=length(dim(A))    ##<< the dimension to extract from 
){
    ##details<< 
    ## Sometimes the dimension of an array that is to be processed is 
    ## only fixed during runtime, i.e. by a value of a variable.
    ## However, with standard \code{[,,]} extraction the syntax is fixed.
    
    ##seealso<< \link{twMisc}
    
    ##details<< \describe{\item{Array functionality of package \link{twMisc} }{
    ## \itemize{
    ## \item Extract \code{A[...,i,...]} from dimension \code{iDim} of an array: this method
    ## \item Splitting an array by dimesnion and listing subarray: \code{\link{twListArrDim}} 
    ## \item Extracting from last dimensions: \code{\link{twExtractFromLastDims}}
    ## \item Stacking a dimension: \code{\link{twStackArrayDim}}
    ## }
    ##}}
    
    # see http://tolstoy.newcastle.edu.au/R/help/01c/2197.html
    ##details<< 
    ## If the dimensionality of the array is unknown, then the A[,i,] notation 
    ## is not applicable. 
    ## This function calculates the indices to extract by using \code{\link{outer}}.
    if( length(i) != 1 ) stop("i must be and integer of length 1")
    if( length(iDim) != 1 ) stop("iDim must be and integer of length 1")
    dims<-dim(A)
    if( iDim<0 | iDim>length(dims) ) stop("wrong dimension iDim")
    if( is.null(dims))
        return( A[i] )
    D<-length(dims)
    skipBefore <- if(iDim>1) prod(dims[1:(iDim-1)]) else 1
    #skipAfter <- if(iDim<D) prod(dims[(iDim+1):D]) else 1
    #skip<-prod(dims[seq(length=D-1)])
    slice0<- sliceJ <-(i-1)*skipBefore+(1:skipBefore)
    if( iDim<D)
        for( jDim in (iDim+1):D ){ 
            #jDim <- iDim+1
            pDim <- prod(dims[1:(jDim-1)])
            sliceJ <- as.vector( outer(sliceJ, (0:(dims[jDim]-1))*(pDim), "+" ) )
        }
    slice <- A[sliceJ]
    dim(slice)<-dims[-iDim]
    dimnames(slice) <- dimnames(A)[-iDim]
    return(slice)
    ### vector representing the 
} 
attr(twExtractDim,"ex") <- function(){
    A <- array( 1:(4*2*3), dim=c(4,2,3) )
    str(A)
    
    # extract from last dimension
    (tmp1 <- twExtractDim(A,3))
    identical( tmp1, A[,,3] ) 

    # extract from second dimension
    (tmp2 <- twExtractDim(A,2,iDim=2))
    identical( tmp2, A[,2,] )
    
    # list all subarrays of last dimension
    (tmp3 <- twListArrDim(A))
    identical( structure(abind::abind(tmp3,rev.along=0), dimnames=NULL), A )
    
    # list all subarrays of second dimension
    (tmp4 <- twListArrDim(A,2))
    
    twExtractFromLastDims(A, 1:2)
}


twListArrDim <- function(
    ### Splits a dimension of an array to a list. (useful for do.call and apply)
    x                       ##<< the array to split
    ,iDim=length(dim(x))    ##<< the dimension to split along, defautls to last dimension
){
    ##seealso<< \link{twExtractDim}, \link{twMisc}
    #lapply(1:(dim(x)[iDim]),function(i){ twExtractDim(x,i,iDim) })
    #i <- 1 
    res <- structure(
        lapply(1:(dim(x)[iDim]),function(i){ twExtractDim(x,i,iDim) })
        ,names=dimnames(x)[[iDim]]
    )
}

twExtractFromLastDims <- function(
    ### Extract slices i from array Aext keeping the all first dimensions.
    Aext        ##<< array to extract from
    ,i          ##<< indices in matrix of last dimensions 
    ,dPrev=c(1) ##<< dimensions in front to keep, defaults to rows
){
    ##seealso<< \link{twExtractDim}, \link{twMisc}
    nl=prod(dim(Aext)[dPrev])
    res <- Aext[ as.numeric(t(outer((i-1)*nl,(1:nl),"+"))) ] 
    dim(res)=c(dim(Aext)[dPrev],length(i)) 
    dimnames(res) = c(dimnames(Aext)[dPrev], list(i=NULL) )
    res
}
attr(twExtractFromLastDims,"ex") <- function(){
    (A <- matrix(1:6, ncol=2))
    (Aext <- abind::abind( lapply(1:4, function(i){(i)/10+A}), along=0 ))
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
    identical( resExp, structure(res, dimnames=NULL) )
}

twStackArrayDim <- function(
        ### stacks a dimension of an array, i.e. reduces to a lower dimension 
        x                               ##<< the array to process
        ,sourceDim=length(dim(x))       ##<< the dimension that should be stacked
        ,destDim=  length(dim(x))-1     ##<< the dimension, along which to stack, referring to the resulting array
        ,sep="_"                        ##<< character separating the parts of the new dimension names
){
    ##seealso<< 
    ## \code{\link{twExtractDim}}, \link{twMisc}
    xl <- twListArrDim(x,sourceDim)
    xr <- abind::abind( xl, along=destDim)
    dimnames(xr)[[destDim]] <- outer( dimnames(xl[[1]])[[destDim]], names(xl), paste, sep=sep  )
    xr
    ### Array of one dimension less than x.
    ### Dimnames are concatenaed names of the sourceDim and the original destDim
}
attr(twStackArrayDim,"ex") <- function(){
    #twStackArrayDim()
}



