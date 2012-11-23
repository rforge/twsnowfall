which.max.matrix <- function(
    ### row and column indices of entry with maximum value
    x   ##<< a matrix
){
    if( !is.matrix(x) ) stop("matrix argument required.")
    i0 <- which.max(x)-1
    col0 <- i0 %/% nrow(x)
    row0 <- i0-col0*nrow(x)    #i0 %% nrow(x)
    structure( c(row0+1, col0+1), names=dimnames(x) )
    ### numeric array (2): row and colum
}

which.min.matrix <- function(
    ### row and column indices of entry with minimum value
    x   ##<< a matrix
){
    if( !is.matrix(x) ) stop("matrix argument required.")
    i0 <- which.min(x)-1
    col0 <- i0 %/% nrow(x)
    row0 <- i0-col0*nrow(x)    #i0 %% nrow(x)
    structure( c(row0+1, col0+1), names=dimnames(x) )
    ### numeric array (2): row and column
}

