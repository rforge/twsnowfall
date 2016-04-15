test.abind3DAlong2 <- function(){
    X <- array(rep(1:4),dim=c(4,2,2), dimnames=list(parms=paste("a",1:4,sep="_"), cols=paste("colX",1:2,sep="_"),NULL))
    Y <- array(rep(10*(1:4)),dim=c(4,3,2))
    Xs <- list(X,Y,Y*10)
    ex <- abind::abind(Xs,along=2L)
    res <- abind3DAlong2(Xs)
    checkEquals(ex, res , check.attributes=FALSE)
    checkEquals(dimnames(X)[c(1,3)], dimnames(res)[c(1,3)] )
}

test.abind4DAlong3 <- function(){
    X <- array(rep(1:4),dim=c(4,2,2,2), dimnames=list(parms=paste("a",1:4,sep="_"), cols=paste("colX",1:2,sep="_"),NULL,NULL))
    Y <- array(rep(10*(1:4)),dim=c(4,2,3,2))
    Xs <- list(X,Y,Y*10)
    ex <- abind::abind(Xs,along=3L)
    res <- abind4DAlong3(Xs)
    checkEquals(dim(ex), dim(res))
    checkEquals(ex, res , check.attributes=FALSE)
    checkEquals(dimnames(X)[c(1,2,4)], dimnames(res)[c(1,2,4)] )
}
