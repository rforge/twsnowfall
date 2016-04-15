expect_isInInterval <- function(
        ### expect_true( all((current >= targetMin) && (current <= targetMax)) )
    current,
    targetMin=0.025,
    targetMax=0.975,
    ...     ##<< further arguments passed to expect_true
){
    ##seealso<< \code{\link{twMisc}} \code{\link{expect_isOfMagnitude}}
    ##details<< 
    ## all parameters will be recycled int the comparison
    ## targetMin and targetMax default to two p of sided 95% confidence interval
    if( !requireNamespace("testthat") ) stop("expect_isInInterval: library testthat must be loaded")
    testthat::expect_true( all((current >= targetMin) && (current <= targetMax)), ... )
}


expect_isOfMagnitude <- function(
    ### expect_true that current is within interval of +-magnitude around target
    target,     ##<< numeric vector
    current,    ##<< numeric vector (recyled) of the same length as target
    orderOfMagnitudes=1/2,  ##<< see details
    ...         ##<< further arguments passed to expect_true
){
    ##seealso<< \code{\link{twMisc}} \code{\link{expect_isInInterval}}
    ##details<< 
    ## in range \code{10^{ log10(current)+-orderOfMagnitudes } }
    .range <- 10^t( sapply(target, function(par){ log10(par)+orderOfMagnitudes*c(-1,+1) }))
    expect_isInInterval(current,.range[,1],.range[,2],...)
    ### TRUE or error
}
#mtrace(checkMagnitude)
attr(expect_isOfMagnitude,"ex") <- function(){
    tmp <- if( requireNamespace("testthat") ){
        target=1e4
        expect_isOfMagnitude(1e4, 3e4)    #TRUE
        #try(expect_isOfMagnitude(1e4, 6e4))   #error
        expect_isOfMagnitude(1e4, 5000)   #TRUE
        #try(expect_isOfMagnitude(1e4, 2000))  #error
        
        expect_isOfMagnitude(1e-4, 3e-4)  #TRUE
        #try(expect_isOfMagnitude(1e-4, 6e-4)) #error
        expect_isOfMagnitude(1e-4, 5e-5)  #TRUE
        #try(expect_isOfMagnitude(1e-4, 2e-5)) #error
    }
}

