
twIntegrateTS <- function( 
    ### Integrate dy between xl and xr.
    dy                      ##<< rate
    ,x                      ##<< times, rate dy refers to interval (x[i],x[i+1]), see \code{\link{twCalcObsScale}} to translate to this observation scale  
    ,xl=min(x,na.rm=TRUE)   ##<< left edge of integration
    ,xr=max(x,na.rm=TRUE)   ##<< right edge of integration
    ,na.rm=TRUE     ##<< can be set to FALSE for performance reasons
    ,doSort=TRUE    ##<< can be set to FALSE for performance reasons
){
    if( xl == xr ){
        r = 0
    }else{
        negbo = FALSE  
        if( xl > xr ){
            #we have a negtive integral
            negbo <- TRUE
            tmp <- xl; xl <- xr; xr <- tmp #swap boundaries
        }
        if( na.rm ){
            bo <- c(!is.na(dy[-length(dy)]),TRUE) & !is.na(x)   #last value of dy might be NA, there is no associated interval
            ty <- dy[bo]
            tx <- x[bo]
        }else{ ty <- dy; tx <- x    }
        if( length(ty) != length(tx) ) stop( gettextf("integrateTS: dy (l=%i) and x (l=%i) must have the same length after exlcuing NAs",length(ty),length(tx)))
        if( doSort ){
            ord <- order(tx)
            ty <- ty[ord]
            tx <- tx[ord]
        }
        if(xl < tx[1] )stop( gettext("integrateTS: left boundary of integral xl is less than x") )
        if(xr > tx[length(tx)] ) stop( gettext("integrateTS: right boundary of integral xr is greater than x[l-1]") )
        dtx <- diff(tx)
        # treat intevals that are entirely within integration
        il <- min( which(tx>=xl))
        ir <- max( which(tx<xr)) #strict less equal because for time xr dy=NA may be specified
        r <- sum( dy[il:ir] * dtx[il:ir] )
        # treat edges
        dxl <- tx[il] - xl
        if( dxl > 0 ) r = r + ty[il-1]*dxl 
        dxr <- tx[ir+1] - xr    #last interval exceed xr, integrted to much
        if(dxr >0) r = r - ty[ir]*dxr 
        if(negbo) r = -r
    }
    r
    ### Each dy_i is giving the slope for succeeding time interval x_i to x_i+1
    ### result vector is of length( sum(is.na(x)) )-1
}

twDiffrl <- function(
    ### Approximation of dx by (x_+1 - x_-1). 
    x
    ,na.rm=TRUE     ##<< can be set to FALSE for performance reasons
){
    tx <- if( na.rm ) x[ !is.na(x) ] else x;
    l <- length(tx)
    if( l < 2 ) 
        stop(gettextf("diffrl: argument x with  length (%i) smaller than two not meaningful", l ))
    r <- (tx[-(1:2)] - tx[-(l-(0:1))])/2
    rl <- (x[2] - x[1])
    rr <- (x[l] - x[l-1])
    c(rl, r, rr)
    ### (x_+1 - x_-1)
    ### with single sided intervals at the right and left side
}

twDiffTS <- function(
    ### Approximation of the derivative dy/dx 
    y,x             ## numeric vector of the same length of at least two components
    ,na.rm=TRUE     ##<< can be set to FALSE for performance reasons
    ,doSort=TRUE    ##<< can be set to FALSE for performance reasons
){
    if( na.rm ){
        ty <- y[ !is.na(y) ]
        tx <- x[ !is.na(x) ]
    }else{ ty <- y; tx <- x }
    l <- length(ty)
    if( l != length(tx) ) 
        stop(gettextf("diffTS: argument y has different length (%i) than argument x (%i)", l, length(time) ))
    if( l < 2 ) 
        stop(gettextf("diffTS: argument y with  length (%i) smaller than two not meaningful", l ))
    #order by increasing tx
    if( doSort ){
        ty <- ty[order(tx)]
        tx <- tx[order(tx)]
        #check for repeated x
        if( any(diff(tx)==0))
            stop(gettextf("diffTS: repeated value in argument x"))
    }
    #caluclation 
    r <- (y[-(1:2)] - y[-(l-(0:1))])/(x[-(1:2)] - x[-(l-(0:1))])
    #single differences at the edges
    rl <- (y[2] - y[1])/(x[2] - x[1])
    rr <- (y[l] - y[l-1])/(x[l] - x[l-1])
    c(rl, r, rr)
    ### numeric vector of length( x[!is.na(x)] ) 
    ### with two sided difference approximation (y_+1 - y_-1)/(x_+1 - x_-1)
    ### and single sided approximations at the edges
}


twCalcObsScale <- function(
    ### Calculate mean slopes, i.e. rates, between within time intervals.
    dx              ##<< numeric vector: slopes at time points times
    ,times1=NULL
    ,times2=NULL    ## << numeric vector: times for which slopes should be calculated
    ,na.rm=TRUE     ##<< can be set to FALSE for performance reasons
    ,doSort=TRUE    ##<< can be set to FALSE for performance reasons
){
    # calcObsScale
    ##details<< 
    ## Calculate the derivative for next intervals (mean of the two borders).
    ## At the end of times1 it corresponds to the slope at last  time point.
    
    if( na.rm ){
        bo <- if( is.numeric(times1) ) !is.na(dx) & !is.na(times1) else !is.na(dx)
        dx <- dx[bo]
        if( is.numeric(times2)) times2 <- times2[ !is.na(times2) ]
    }
    
    if( doSort ){
        if(0<length(times1)){
            ord <- order(times1)
            dx <- dx[ord]
            times1 <- times1[ord]
        }
        if( 0<length(times2)) times2 <- sort(times2)
    }
    
    # oneSided is the mean slope of two adjacent points
    oneSided <- c((dx[-length(dx)]+dx[-1])/2, dx[length(dx)])
    
    #make sure that range(times2) is within range(times1) before integration
    int <- NULL
    if( is.numeric(times1) && is.numeric(times2)){
        if( length(oneSided) != length(times1) ){
            traceback.curr()
            stop(paste("calcObsScale: not allowed: length(times1)=",length(times1),", oneSided=",paste(oneSided, collapse=","),sep=""))
        }
        int <- sapply( 2:length(times2), function(i){
                twIntegrateTS( oneSided, times1
                    , xl=times2[i-1]
                    , xr=times2[i]
                    , na.rm=FALSE #already removed NAs
                )
            })/diff(times2) #devide by time interval to obtain a rate
        # last point: rate over all times1 > times2
        t1 <- times1[length(times1)] 
        t2 <- times2[length(times2)]
        tmp.dt <- t1 - t2
        int <- c(int, if( tmp.dt > 0){
                    twIntegrateTS( oneSided, times1, t2, t1 )/tmp.dt
                }else NA)
    }
    list( oneSided=oneSided, int=int)
    ### list with components \describe{
    ### \item{oneSided}{slope within the interval (i,i+1)}
    ### \item{int}{same for intervals times2 (NULL if either times1 or times2 was NULL)}
    ### }
}

twSmoothSwitch <- function(
    ### Smooth increase from 0 to 1 around b.
    x       ##<< numeric vector: times to evaluate
    ,b=0    ##<< numeric scalar: time of switch
    ,smooth.range.width=0.1*b   ##<< width of smooth increase
){
    if( smooth.range.width==0 ) stop("smooth.range.width must be > 0.")
    tmp <- (x-b)/smooth.range.width*pi
    ifelse(tmp < pi/2, ifelse( tmp < -pi/2,-1,sin(tmp)) , 1 )/2+0.5
    ### 0 for x <= b - r/2 \cr
    ### sin(x-b)/r*pi for b-r/2 <= x <= b+r/2 \cr
    ### 1 for b - r/2 <= x \cr
}

twSaveZero <- function(
    ### Function going to zero for small x over several orders of magnitude
    x           ##<< numeric vector
    ,kM=1e-10   ##<< order of magnitude where decrease happens
){
    ##details<< 
    ## f(x)->1 for x >> kM; f(x)=0 for x << kM 
    ifelse( x<=0,0, exp(1-((x+kM)/x)) )
}


