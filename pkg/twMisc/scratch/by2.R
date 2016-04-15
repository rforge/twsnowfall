by2 <- function(
    ### modification of by that returns a data frame
    data, 
    INDICES, 
    FUN, 
    ...
    )UseMethod("by2")

by2.data.frame <- function(
    ### modification of by that returns a data frame
    data, 
    INDICES, 
    FUN, 
    ...) 
{
    ##<<seealso \code{\link{tapply2}}
    if (!is.list(INDICES)) {
        IND <- vector("list", 1)
        IND[[1]] <- INDICES
        names(IND) <- deparse(substitute(INDICES))[1]
    }
    else IND <- INDICES
    FUNx <- function(x) FUN(data[x, ], ...)
    nd <- nrow(data)
    #ans <- eval(substitute(tapply(1:nd, IND, FUNx)), data)
    ans <- eval(substitute(tapply2(1:nd, IND, FUNx)), data)
    attr(ans, "call") <- match.call()
    #class(ans) <- "by"
    ans
}
#mtrace(by2.data.frame)

tapply2 <- function(
    ### modification of tapply that returns a data frame
    X, 
    INDEX, 
    FUN = NULL, 
    ...
){
    ##<<details
    ## assumes that FUN returns a list with each entry a single value
    ## dataframe consists of the indices and one column for each entry in the list returned by FUN
    FUN <- if (!is.null(FUN)) 
        match.fun(FUN)
    if (!is.list(INDEX)) 
        INDEX <- list(INDEX)
    nI <- length(INDEX)
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- as.integer(1)
    group <- rep.int(one, nx)
    ngroup <- one
    for (i in seq.int(INDEX)) {
        index <- as.factor(INDEX[[i]])
        if (length(index) != nx) 
            stop("arguments must have same length")
        namelist[[i]] <- levels(index)
        extent[i] <- nlevels(index)
        group <- group + ngroup * (as.integer(index) - one)
        ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN)) 
        return(group)
    ans <- lapply(split(X, group), FUN, ...)
    index <- as.numeric(names(ans))
    ansdf <- expand.grid( namelist )[index,]
    resnames <- names(ans[[1]])
    resname <- resnames[1]
    for( resname in resnames ){ ansdf[,resname] <- sapply( ans, function(x){ x[[ resname ]]})   }
    ansdf
}
#mtrace(tapply2)
