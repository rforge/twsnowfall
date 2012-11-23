.setUp <- function(){
    .setUpDf <- within( list(),{
            x <- -1:-1e5
            fun <- function(i){x[round(i)]}
        })
    attach(.setUpDf)
}
.tearDown <- function () {
    detach()
}

test.ordinary <- function(){
    #mtrace(twBinOptimize.numeric)
    res <- twBinOptimize(x,-15,showiter=TRUE)
    checkEquals(15,res$where)
    
    res <- twBinOptimize(x,-14.8)
    checkEquals(15,res$where)
}

test.empty <- function(){
    res <- twBinOptimize(x[FALSE],-15)
    checkEquals(as.integer(NA),res$where)
}

test.ordinaryF <- function(){
    res<-NULL; res <- twBinOptimize(fun,-15,interval=c(1,length(x)),showiter=TRUE)
    checkEquals(15,res$where)
    
    res <- twBinOptimize(x,-14.8,interval=c(1,length(x)))
    checkEquals(15,res$where)
}

test.boundaryF <- function(){
    #mtrace(twBinOptimize.function)
    res <- twBinOptimize(fun,-15,interval=c(length(x),1))   # lower and upper are min and max
    checkEquals(15,res$where)
    
    res <- twBinOptimize(x,-14.8,upper=1,lower=length(x))
    checkEquals(as.integer(NA),res$where)
}