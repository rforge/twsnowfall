checkInterval <- function(
        ### checkTrue( all((current >= targetMin) && (current <= targetMax)) )
    current,
    targetMin=0.025,
    targetMax=0.975,
    ...     ##<< further arguments passed to checkTrue
){
    ##seealso<< \code{\link{twMisc}} \code{\link{checkMagnitude}}
    ##details<< 
    ## all parameters will be recycled int the comparison
    ## targetMin and targetMax default to two p of sided 95% confidence interval 
    checkTrue( all((current >= targetMin) && (current <= targetMax)), ... )
}


checkMagnitude <- function(
    ### checkTrue that current is within interval of +-magnitude around target
    target,     ##<< numeric vector
    current,    ##<< numeric vector (recyled) of the same length as target
    orderOfMagnitudes=1/2,  ##<< see details
    ...         ##<< further arguments passed to checkTrue
){
    ##seealso<< \code{\link{twMisc}} \code{\link{checkInterval}}
    ##details<< 
    ## in range \code{10^{ log10(current)+-orderOfMagnitudes } }
    .range <- 10^t( sapply(target, function(par){ log10(par)+orderOfMagnitudes*c(-1,+1) }))
    checkInterval(current,.range[,1],.range[,2],...)
    ### TRUE or error
}
#mtrace(checkMagnitude)
attr(checkMagnitude,"ex") <- function(){
    target=1e4
    checkMagnitude(1e4, 3e4)    #TRUE
    try(checkMagnitude(1e4, 6e4))   #error
    checkMagnitude(1e4, 5000)   #TRUE
    try(checkMagnitude(1e4, 2000))  #error
    
    checkMagnitude(1e-4, 3e-4)  #TRUE
    try(checkMagnitude(1e-4, 6e-4)) #error
    checkMagnitude(1e-4, 5e-5)  #TRUE
    try(checkMagnitude(1e-4, 2e-5)) #error
}

