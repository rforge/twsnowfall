# misc.R
# 
# miscelancelous functions and plotting style lists
#
# Author: twutz
###############################################################################

#plotting styles by different time series of experiments

seqRange <- function(
        ### Create a sequence based on a range c(min,max)
        range   ##<< the range for the sequence
        ,...    ##<< further arguments to seq, defaults to length.out=50
){
    if( 0==length(list(...)))
        seq( range[1],range[2], length.out=50 )
    else
        seq( range[1],range[2], ...)
}



capitalize <- function(
        ### "Mixed Case" Capitalizing
        x
### The string to capitalize
){
    ##<<details 
    ## toupper( every first letter of a word )
    ## useful for generating graph texts
    #lcase, ucase
    if( length(x) > 1)
        sapply(x, capitalize)
    else{
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
    }
}

#within function does not exist at the cluster, so specifiy it
if( !exists("within")){
    within <- function (data, expr, ...) UseMethod("within")
    within.list <- function (data, expr, ...){
        parent <- parent.frame()
        e <- evalq(environment(), data, parent)
        eval(substitute(expr), e)
        l <- as.list(e)
        l <- l[!sapply(l, is.null)]
        nD <- length(del <- setdiff(names(data), (nl <- names(l))))
        data[nl] <- l
        if (nD) 
            data[del] <- if (nD == 1) 
                        NULL
                    else vector("list", nD)
        data
    }
}

evalCommandArgs <- function(
        ### evaluate args passed to a batch script 
        args = commandArgs(TRUE)            ##<< the commands to be evaluated 
        ,envir = new.env(parent=baseenv())  ##<< the environment where args should be evaluated
){
    ##<<details R CMD BATCH --vanilla '--args i=1 n=8' testCommandArgs.R testCommandArgs.Rout
    for(i in seq( along.with=args)){
        eval( parse(text=args[[i]]), envir=envir)
    }
    ### The environment where the arguments have been evaluated (may use $ or as.list)
    envir
}
attr(evalCommandArgs,"ex") <- function(){
    system('Rscript -e "str(commandArgs(TRUE))" i=1 n=8')
    as.list(evalCommandArgs(args=c("i=1","n=8")))
    if( FALSE ){
        # do not run automatically, as it depends on twMisc installed
        system('Rscript -e "library(twMisc); str(as.list(evalCommandArgs())); ls()" i=1 n=8')
    }
}



# code that needs to be moved to other packages
# keep it temporarily, so that not need to build other packages


twWhichColsEqual <- function(
        ### compares each column of X to column of Z and returns indices of equal columns   
        X,  ##<< matrix
        Z=matrix(rep(z,ncol(X)),nrow=length(z)),    
        ### Matrix with same numer of rows. 
        z=NA
### alternatively, specify only one vector, which each row of X is compared against
){
    
    # column indices of all single components
    ##details<< 
    ## If both vectors contain NA's at the same position the vectors are regarded equal.
    ## This is different from which with ==, where any NA leeds to a FALSE 
    iComp <- which(X == Z | (is.na(X) & is.na(Z)), arr.ind=TRUE )[,2]
    # when all components in a row are the same, they will occure nrow(X) times in iComp
    nrX1 <- nrow(X)-1
    if( length(iComp) > nrX1){
        if( nrX1 != 0){
            # compare each index ii of iComp with index ii+nrX1
            # vectorized version by comparing vector with last and first part removed
            ii <- which( iComp[1:(length(iComp)-nrX1)] == iComp[-(1:nrX1)] )
            iComp[ii]
        }else iComp # if there is just one row iComp is already the index
    }else
        integer(0)
} 

whichColsEqualSumHeuristics <- function(
        ### compares each column of X to column of Z and returns indices of equal columns (determined by same sum column sum)    
        X,  ##<< matrix
        Z   ##<< Matrix with same numer of rows, or a vector representing a single column that is compared to each column of X 
){
    sumsX <- colSums(X, na.rm=TRUE)
    sumsZ <- if( is.array(Z) ) colSums(Z, na.rm=TRUE) else sum(Z, na.rm=TRUE)
    which( sumsX == sumsZ )
} 



twStripFileExt <- function(
        ### Remove the all the file extension, i.e. the last dot and suceeding characters.
        filenames
){
    ##seealso<< code{\link{fileExt}}, \link{twMisc}
    sub("[.][^.]*$", "", filenames, perl=TRUE)
}

fileExt <- function(
        ### Return the file extension
        filenames
){
    ifelse( regexpr("\\.",filenames) != -1,
            sub("(^.*[.])([^.]*)$", "\\2", filenames, perl=TRUE)
            ,"")
}
attr(fileExt,"ex") <- function(){
    ##seealso<< code{\link{fileExt}}, \link{twMisc}
    filenames=c("text.txt","dir/some.test.yml","a")
    fileExt(filenames)
}






twLastN1 <- function(
        ### last n components of vector x
        x       ##<< vector
        ,n=1    ##<< number of components from the end
){
    if( !is.finite(n) || (n<=0) ) return( x[FALSE] )
    if( n>=length(x)) return(x)
    x[ length(x)+1-(n:1) ]
}

twLastN21 <- function(
        ### last n rows of matrix x
        x       ##<< matrix
        ,n=1    ##<< number of components from the end
){
    if( !is.finite(n) || (n<=0) ) return( x[FALSE,,drop=FALSE] )
    if( n>=nrow(x)) return(x)
    x[ nrow(x)+1-(n:1), ]
}

twLastN22 <- function(
        ### last n columns of matrix x
        x       ##<< matrix
        ,n=1    ##<< number of components from the end
){
    if( !is.finite(n) || (n<=0) ) return( x[,FALSE,drop=FALSE] )
    if( n>=ncol(x)) return(x)
    x[ ,ncol(x)+1-(n:1) ]
}

twLogSumExp <- function(
        ### calculates the log(sum(exp(x))) in numerically safer way
        x   ##<< vector to be summed
        ,shiftUpperBound=FALSE ##<< use this if x has a clear upper bound
        ,shiftLowerBound=FALSE ##<< use this if x has a clear lower bound
){
    #sum(e^xi) = sum(e^(xi+a-a)) = sum(e^(xi-a) e^a) ) = e^a sum(e^(xi-a))
    #twUtestF(twLogSumExp)
    ##details<< 
    ## Before taking the exponent, all x are shifted towards zero.
    ## By default the median of x is subtracted. 
    ## If shiftUpperBound then max(x) is subtracted.
    ## This is useful if the distribution of x has a strong left tail but a defined upper bound.
    ## If shiftLowerBound then min(x) is subtracted.
    ## This is useful if the distribution of x has a strong right tail but a defined lower bound.
    x <- na.omit(x)
    xShift <- if( shiftUpperBound ) max(x) else     if( shiftLowerBound ) min(x) else median(x)
    xexp <- exp(x-xShift)
    xShift + log(sum(xexp))
}

twLogMeanExp <- function(
        ### calculates the log(mean(exp(x))) in numerically safer way
        x   ##<< vector for whose mean is to be caluclated
        ,...    ##<< futher arguments to twLogSumExp 
){
    #log(sum / n)
    twLogSumExp(x,...) - log(length(x))
}

twCloseDevs <- function(
        ### Closes all windows with a device number unless those specified with parameter omit.
        omit=c()    ### list of Devices not to close.
){
    for( dev in dev.list()[ !(dev.list() %in% omit)] ) 
        dev.off(dev)
}
cutQuantiles <- function (
        ### Cut a Numeric Variable into Intervals of about same number of observations.
        x           ##<< numeric vector to classify into intervals 
        , cuts      ##<< cut points 
        , m = 150   ##<< desired minimum number of observations in a group 
        , g         ##<< number of quantile groups 
        , levels.mean = FALSE   ##<< set to TRUE to make the new categorical vector have levels attribute that is the group means of x instead of interval endpoint labels 
        , digits    ##<< number of significant digits to use in constructing levels. Default is 3 (5 if levels.mean=TRUE) 
        , minmax = TRUE ##<< if cuts is specified but min(x)<min(cuts) or max(x)>max(cuts), augments cuts to include min and max x
        , oneval = TRUE ##<< if an interval contains only one unique value, the interval will be labeled with the formatted version of that value instead of the interval endpoints, unless oneval=FALSE
        , onlycuts = FALSE  ##<< set to TRUE to only return the vector of computed cuts. This consists of the interior values plus outer ranges. 
        , onlymeans = FALSE ##<< set to TRUE to only return the means of x within each group 
){
    ##details<< 
    ## copied from Hmisc:cutQuantiles to reduce package dependencies.
    
    ##seealso<< \code{\link{cut}},\code{\link{quantile}}
    ##seealso<< \code{\link{seqRange}}, \link{twMisc}
    
    method <- 1
    x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
    min.dif <- min(diff(x.unique))/2
    min.dif.factor <- 1
    if (missing(digits)) 
        digits <- if (levels.mean) 
                    5
                else 3
    oldopt <- options(digits = digits)
    on.exit(options(oldopt))
    xlab <- attr(x, "label")
    if (missing(cuts)) {
        nnm <- sum(!is.na(x))
        if (missing(g)) 
            g <- max(1, floor(nnm/m))
        if (g < 1) 
            stop("g must be >=1, m must be positive")
        options(digits = 15)
        n <- table(x)
        xx <- as.double(names(n))
        options(digits = digits)
        cum <- cumsum(n)
        m <- length(xx)
        y <- as.integer(ifelse(is.na(x), NA, 1))
        labs <- character(g)
        cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant", 
                rule = 2, f = 1)$y
        cuts[length(cuts)] <- max(xx)
        lower <- xx[1]
        upper <- 1e+45
        up <- low <- double(g)
        i <- 0
        for (j in 1:g) {
            cj <- if (method == 1 || j == 1) 
                        cuts[j]
                    else {
                        if (i == 0) 
                            stop("program logic error")
                        s <- if (is.na(lower)) 
                                    FALSE
                                else xx >= lower
                        cum.used <- if (all(s)) 
                                    0
                                else max(cum[!s])
                        if (j == m) 
                            max(xx)
                        else if (sum(s) < 2) 
                            max(xx)
                        else approx(cum[s] - cum.used, xx[s], xout = (nnm - 
                                                cum.used)/(g - j + 1), method = "constant", 
                                    rule = 2, f = 1)$y
                    }
            if (cj == upper) 
                next
            i <- i + 1
            upper <- cj
            y[x >= (lower - min.dif.factor * min.dif)] <- i
            low[i] <- lower
            lower <- if (j == g) 
                        upper
                    else min(xx[xx > upper])
            if (is.na(lower)) 
                lower <- upper
            up[i] <- lower
        }
        low <- low[1:i]
        up <- up[1:i]
        variation <- logical(i)
        for (ii in 1:i) {
            r <- range(x[y == ii], na.rm = TRUE)
            variation[ii] <- diff(r) > 0
        }
        if (onlycuts) 
            return(unique(c(low, max(xx))))
        if( onlymeans){
            return( as.vector(tapply(x, y, function(w) mean(w, na.rm = TRUE))) )
        }
        flow <- format(low)
        fup <- format(up)
        bb <- c(rep(")", i - 1), "]")
        labs <- ifelse(low == up | (oneval & !variation), flow, 
                paste("[", flow, ",", fup, bb, sep = ""))
        ss <- y == 0 & !is.na(y)
        if (any(ss)) 
            stop(paste("categorization error in cutQuantiles.  Values of x not appearing in any interval:\n", 
                            paste(format(x[ss], digits = 12), collapse = " "), 
                            "\nLower endpoints:", paste(format(low, digits = 12), 
                                    collapse = " "), "\nUpper endpoints:", paste(format(up, 
                                            digits = 12), collapse = " ")))
        y <- structure(y, class = "factor", levels = labs)
    }
    else {
        if (minmax) {
            r <- range(x, na.rm = TRUE)
            if (r[1] < cuts[1]) 
                cuts <- c(r[1], cuts)
            if (r[2] > max(cuts)) 
                cuts <- c(cuts, r[2])
        }
        l <- length(cuts)
        k2 <- cuts - min.dif
        k2[l] <- cuts[l]
        y <- cut(x, k2)
        if (!levels.mean) {
            brack <- rep(")", l - 1)
            brack[l - 1] <- "]"
            fmt <- format(cuts)
            labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l], 
                    brack, sep = "")
            if (oneval) {
                nu <- table(cut(x.unique, k2))
                if (length(nu) != length(levels(y))) 
                    stop("program logic error")
                levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)], 
                                fmt[l]), labs)
            }
            else levels(y) <- labs
        }
    }
    if (levels.mean) {
        means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
        levels(y) <- format(means)
    }
    attr(y, "class") <- "factor"
    if (length(xlab)) 
        attr(y, "label") <- xlab #label(y) <- xlab
    y
    ### a factor variable with levels of the form [a,b) or formatted means (character strings) unless onlycuts is TRUE in which case a numeric vector is returned
}
attr(cutQuantiles,"ex") <- function(){
    set.seed(1)
    x <- runif(1000, 0, 100)
    z <- cutQuantiles(x, c(10,20,30))
    table(z)
    table(cutQuantiles(x, g=10))      # quantile groups
    cutQuantiles(x, g=10, onlymeans=TRUE)     # get only the means of each group
    table(cutQuantiles(x, m=50))      # group x into intevals with at least 50 obs.
}

.inside <- function (x, interval){  x >= interval[1] & x <= interval[2] }

twRescale <- function (
        ### Rescale numeric vector to have specified minimum and maximum. 
        x               ##<< data to rescale
        ,to = c(0, 1)   ##<< range to scale to
        ,from =         ##<< range to scale from, defaults to range of data
                range(x[is.finite(x)], na.rm = TRUE)    
        ,clip = TRUE    ##<< should values be clipped to specified range?
){
    ##details<<
    ## adapted from package ggplot2 to avoid package redundancies
    
    ##author<< Hadley Wickham <h.wickham@gmail.com>, Thomas Wutzler
    
    ##details<< 
    ## If from[1] == from[2] then the mean of interval to is returned.
    if( length(from) != 2 || length(to) != 2)
        stop("twRescale: arguments to and from must be ranges.")
    if ( from[1] == from[2] ) 
        return( rep( mean(to), length(x) ) )
    if (is.factor(x)) {
        warning("twRescale: Categorical variable automatically converted to continuous", 
                call. = FALSE)
        x <- as.numeric(x)
    }
    scaled <- (x - from[1])/diff(from) * diff(to) + to[1]
    if (clip) {
        ifelse(!is.finite(scaled) | .inside(scaled,to), scaled, 
                NA)
    }
    else {
        scaled
    }
}


loadAssign <- function(
        ### Load a RData file and return the value of the first entry
        ... ##<< arguments to \code{\link{load}}
##seealso<< \code{\link{seqRange}}, \link{twMisc}
){
    ##details<<
    ## The load function is evaluated in a local environment.
    ## Then the value of the first entry of ls in that environment is returned.
    local({load(...); get(ls()[1])})
    ### Value of the first variable the loaded file
    
}
attr(loadAssign,"ex") <- function(){
    # save the filename character into a temporary file 
    fout <- fout2 <- file.path(tempdir(),"tmp.RData")
    save(fout,file=fout)
    fout <- "changed"
    (x <- loadAssign(file=fout2))   
    fout                # note that is has not been overwritten with load
}

reorderFactor <- function(
        ### reorder the factor levels in a factor
        x           ##<< factor to reorder
        , newLevels ##<< character vector: levels in new order
##seealso<< \code{\link{seqRange}}, \link{twMisc}
){
    levelMap <- match(levels(x), as.factor(newLevels))
    factor( newLevels[levelMap[x]], levels=newLevels)   
    ### factor with levels corresponding to newLevels
}
attr(reorderFactor,"ex") <- function(){
    x <- as.factor(sample(c("low","medium","high"),10,replace=TRUE) )
    x
    as.integer(x)
    y <- reorderFactor(x,c("low","medium","high"))
    y
    as.integer(y)
    all(x == y)
}



formatSig <- function(
        ### format real values to significant number of digits
        x, digits=3
##seealso<< \code{\link{seqRange}}, \link{twMisc}
){
    #l10x <- log10(x)
    ifelse( x < 10^(digits-1) #l10x < digits-1
            , formatC(x,digits=digits,format="fg",flag="#")
            , as.character( signif(x,digits) ) 
    )
    ### character vector with number rounded to significant digits and output including trailing zeros
}
attr(formatSig,"ex") <- function(){
    x <- c(0.0654,0.06,6,65,99.1,100,100.8,125,1024,2000)
    formatSig(x,3)
    formatSig(x,2)
}

"vectorElements<-" <- function(
        ### adds or replaces value in vector
        vec         ##<< a named vector
        ,value      ##<< a named vector of same mode as vec
){
    ##seealso<< \code{\link{seqRange}}, \link{twMisc}
    
    if( 0 == length(value) ) return( vec )
    if( is.null(vec) ) vec <- structure(vector(mode(value),0), names=character(0))
    if( !is.vector(vec) )
        stop("setVectorElements<-: called with argument vec being not a vector")
    vecNames <- names(vec)
    if( is.null(vecNames) )
        if( 0 == length(vec) )
            vecNames <- names(vec) <- character(0)
        else
            stop("setVectorElements<-: vec must be a named vector")
    newNames <- names(value)
    if( 0==length(newNames) || !all(nzchar(newNames)) )
        stop("setVectorElements<-: all components of newValues must have a name")
    for( newName in  newNames){
        if( newName %in% vecNames) 
            vec[newName] <- value[newName]
        else
            vec <- c(vec, value[newName])
    }
    vec
}
.tmp <- get("vectorElements<-")
attributes(.tmp )$ex <- function(){
    vec <- c(a=1)
    vectorElements(vec) <- c(a=2)
    vec
    vectorElements(vec) <- c(b=2)
    vec
    vec=numeric(0)  # special case: names are created
    vectorElements(vec) <- c(b=2)
    vec
    
    l1 <- list()
    vectorElements(l1$a) <- c(b=2)  # creating list entry a
    l1
}
#is.language(as.name("vectorElements<-"))





dfcol <- function(
        ### extract column from a data.frame and reassing names
        x           ##<< the dataFrame
        ,colName    ##<< the column name
##seealso<< \code{\link{seqRange}}, \link{twMisc}
){
    structure( x[,colName], names=rownames(x) )
    ### \code{structure( x[,colName], names=rownames(x) )}
}
attr(dfcol,"ex") <- function(){
    data <- data.frame( a=1:4, b=2*(1:4) )
    rownames(data) <- LETTERS[1:4]
    dfcol(data,"b")
}


catNamedVector <- function(
        ### print numeric vector in form name=value,name2=value2 
        x           ##<< numeric vector
        , digits=3  ##<< number of digits passed to \code{\link{signif}}
){
    paste(names(x),signif(x,digits=digits),sep="=",collapse=",")
}
attr(catNamedVector,"ex") <- function(){
    x <- c(a=5, b=pi)
    catNamedVector(x)
}

catHeadNamedVector <- function(
        ### print first numeric vector in form name=value,name2=value2 
        x
        , digits=3
        , nmax=4
){
    paste0("(",length(x),") ",catNamedVector(head(x,nmax)))
}
attr(catNamedVector,"ex") <- function(){
    x <- c(a=5, b=pi)
    catHeadNamedVector(rep(x,5))
}

asCharacterWithLeading <- function(
        ### convert integer to string with fixed width by leading zeros (or spaces)
        x                   ##<< integer vector: number to convert to string
        , template="00"	    ##<< scalar string of intended output length with specifying the leading characters.
){
    result <- as.character(x)
    zeros <- rep(template, length(x) )
    nZero <- nchar(template)-nchar(result)
    if( any(nZero < 0) ) stop("asCharacterWithLeading: input has too many digits. Maybe extend the template argument to more characters.")
    result[ nZero != 0] <- paste( substr(zeros, 1, nZero), result, sep="")[nZero != 0]
    result
}
attr(asCharacterWithLeading,"ex") <- function(){
    x <- c(1,11,111)
    asCharacterWithLeading(x,"000")
    try( asCharacterWithLeading(x) )
}


