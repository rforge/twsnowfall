abind3DAlong2 <- function(
   ### bind a list of 3D arrays along the second dimension
   Xs   ##<< list with matrices
){
    nCols <- sapply(Xs,ncol)
    csumNCols <- c(0,cumsum(nCols))
    X <- Xs[[1]]
    dimAns <- dim(X); dimAns[2] <- sum(nCols)
    ##details<< The dimensionnames correspond to that of Xs[[1]], except that the columnnames are NULL
    dimNamesAns <- if( length(dimnames(X))){
        dimNamesAns <- dimnames(X)
        dimNamesAns[[2]] <- NULL
        dimNamesAns
    } else NULL
    ans <- array(dim=dimAns, dimnames=dimNamesAns)
    #i <- 1L
    for( i in seq_along(Xs) ){
        Y <- Xs[[i]]
        ans[,csumNCols[i]+(1L:nCols[i]),] <- Y
    }
    ans
}

abind4DAlong3 <- function(
        ### bind a list of 4D arrays along the third dimension
        Xs   ##<< list with matrices
){
    nCols <- sapply(Xs,function(Xi){dim(Xi)[3]})
    csumNCols <- c(0,cumsum(nCols))
    X <- Xs[[1]]
    dimAns <- dim(X); dimAns[3] <- sum(nCols)
    ##details<< The dimensionnames correspond to that of Xs[[1]], except the names of the third dimension are NULL
    dimNamesAns <- if( length(dimnames(X))){
                dimNamesAns <- dimnames(X)
                dimNamesAns[[3]] <- NULL
                dimNamesAns
            } else NULL
    ans <- array(dim=dimAns, dimnames=dimNamesAns)
    #i <- 1L
    for( i in seq_along(Xs) ){
        Y <- Xs[[i]]
        ans[,,csumNCols[i]+(1L:nCols[i]),] <- Y
    }
    ans
}

