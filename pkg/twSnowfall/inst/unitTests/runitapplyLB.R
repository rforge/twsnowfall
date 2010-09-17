.setUp <-function () {}

.tearDown <- function () {}


test.sfFArgsApplyLB <- function(){
	X <- matrix(0:9,nrow=5,ncol=2)
	dimnames(X) <- list( rows=paste("r",1:nrow(X),sep=""), cols=paste("c",1:ncol(X),sep="")  )
	Y <- X*10
	F_ARGS <- function(i){c(list(X[i,]),list(Y[i,]))}
	F_ARGS(1)
	F_APPLY <- paste
	str(resSeq <- sfFArgsApplyLB( nrow(X), F_ARGS, F_APPLY, sep="-", debugSequential=TRUE))
	str(resPar <- sfFArgsApplyLB( nrow(X), F_ARGS, F_APPLY, sep="-"))
	checkEquals( resSeq, resPar )
}

.testMargins <- function(FUN,...){
	X <- matrix(0:9,nrow=5,ncol=2)
	.testMarginsXDots(X,FUN,...)
	dimnames(X) <- list( rows=paste("r",1:nrow(X),sep=""), cols=paste("c",1:ncol(X),sep="")  )
	.testMarginsXDots(X,FUN,...)
}

.testMarginsX <- function(X,FUN){
	#for debuggin interactively
	.margin=1
	.exp <- apply( X, .margin, FUN)
	.resSeq <- sfApplyMatrixLB(X,.margin, FUN,debugSequential=TRUE)
	checkEquals(.exp,.resSeq)	
	.resPar <- (sfApplyMatrixLB(X,.margin, FUN))
	checkEquals(.exp,.resPar)
	
	.margin=2
	.exp <- apply( X, .margin, FUN)
	.resSeq <- sfApplyMatrixLB(X,.margin, FUN,debugSequential=TRUE)
	checkEquals(.exp,.resSeq)	
	.resPar <- (sfApplyMatrixLB(X,.margin, FUN))
	checkEquals(.exp,.resPar)
}

.testMarginsXDots <- function(X,FUN,...){
	.margin=1
	.exp <- apply( X, .margin, FUN, ...)
	.resSeq <- sfApplyMatrixLB(X,.margin, FUN,debugSequential=TRUE,...)
	checkEquals(.exp,.resSeq)	
	.resPar <- (sfApplyMatrixLB(X,.margin, FUN, ...))
	checkEquals(.exp,.resPar)

	.margin=2
	.exp <- apply( X, .margin, FUN, ...)
	.resSeq <- sfApplyMatrixLB(X,.margin, FUN,debugSequential=TRUE,...)
	checkEquals(.exp,.resSeq)	
	.resPar <- (sfApplyMatrixLB(X,.margin, FUN, ...))
	checkEquals(.exp,.resPar)
	.resPar
}


test.scalar <- function(){
	FUN=sum
	.testMargins(FUN)
}

test.namedVector <- function(){
	FUN <- function(x,...){ y=paste("v",x,...); names(y)<-paste("a",seq(along.with=x),sep=""); y}
	str(res <- .testMargins(FUN))
}

test.vectorAttributes <- function(){
	FUN <- function(x,...){ y=paste("v",x,...); attr(y,"someAttrib")<-paste("a",seq(along.with=x),sep=""); y}
	str(res <- .testMargins(FUN))
}


test.matrix <- function(){
	FUN <- function(x,...){ matrix(x,ncol=length(x),nrow=2)}
	#FUN(X[1,])
	str(res <- .testMargins(FUN))
	#mtrace(.testMarginsXDots)
	#mtrace(sfSimplifyLBResult)

	FUN <- function(x,...){ y=matrix(x,ncol=length(x),nrow=2); colnames(y)<-paste("a",seq(along.with=x),sep=""); y}
	#FUN(X[1,])
	str(res <- .testMargins(FUN))
	
}

test.list <- function(){
	FUN <- function(x,...){ list(a="a", b=2)}
	.testMargins(FUN)
}

test.data.frame <- function(){
	FUN <- function(x,...){ data.frame(a="a", b=rep(2,3))}
	.testMargins(FUN)
}

test.empty <- function(){
	FUN <- function(x,...){ character(0) }
	.testMargins(FUN)
}

test.NA <- function(){
	FUN <- function(x,...){ NA }
	.testMargins(FUN)
}

test.NULL <- function(){
	FUN <- function(x,...){ NULL }
	.testMargins(FUN)
}





depr.test.twSfApply2MatrixLB <- function(){
	X1 <- matrix(0:9,nrow=5,ncol=2)
	X2 <- X1*10
	.expl <-  lapply(1:nrow(X1), function(index){ paste(X1[index,],X2[index,],sep="_")})
	.exp <- sfSimplifyLBResult(.expl, dimnames(X1)[1])
	.resSeq <- twSfApply2MatrixLB(X1,X2,paste,sep="_",debugSequential=TRUE)
	checkEquals(.exp,.resSeq)	
	.resPar <- twSfApply2MatrixLB(X1,X2,paste,sep="_")
	checkEquals(.exp,.resPar)
	
	X2empty <- (X1*10)[,FALSE]
	.expemptyl <- lapply(1:nrow(X1), function(index){ paste(X1[index,],X2empty[index,],sep="_")})
	.expempty <- sfSimplifyLBResult(.expemptyl, dimnames(X1)[1])
	.resParempty <- twSfApply2MatrixLB(X1,X2empty,paste,sep="_")
	checkEquals(.expempty,.resParempty)

	X1empty <- (X1)[,FALSE]
	.expemptyl <- lapply(1:nrow(X1empty), function(index){ paste(X1empty[index,],X2empty[index,],sep="_")})
	.expempty <- sfSimplifyLBResult(.expemptyl, dimnames(X1)[1])		#nothing: character(0)
	.resParempty <- twSfApply2MatrixLB(X1empty,X2empty,paste,sep="_")
	checkEquals(.expempty,.resParempty)
}

test.sfFArgsApplyDep <- function(){
	X <- matrix(0:9,nrow=5,ncol=2)
	dimnames(X) <- list( rows=paste("r",1:nrow(X),sep=""), cols=paste("c",1:ncol(X),sep="")  )
	Y <- X*10
	F_ARGS <- function(i,prevRes){c(list(X[i,]),list(Y[i,]))}
	F_ARGS(1)
	F_APPLY <- paste
	.exp <- sfFArgsApplyLB( nrow(X), F_ARGS, F_APPLY, sep="-", debugSequential=TRUE)
	str(resSeq <- sfFArgsApplyDep( nrow(X), F_ARGS, F_APPLY, 1:3, sep="-", debugSequential=TRUE))
	checkEquals(.exp, resSeq )
	str(resPar <- sfFArgsApplyDep( nrow(X), F_ARGS, F_APPLY, 1:3, sep="-"))
	checkEquals( resSeq, resPar )
	
	#mtrace(twDynamicClusterApplyDep)
	#mtrace(sfFArgsApplyDep)
	.exp <- cumsum(0:4)
	F_APPLY <- function(x,z) x+z;
	Z<-X[,1]
	F_ARGS <- function(i,prevRes){list(x=prevRes,z=Z[i])}	
	.res0 <- 0
	str(resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0, debugSequential=TRUE))
	resSeqS <- sfSimplifyLBResult(resSeq)
	checkEquals( .exp, resSeqS )
	str(resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0 ))
	resSeqS <- sfSimplifyLBResult(resSeq)
	checkEquals( .exp, resSeqS )
	
	Z<-matrix(1,nrow=6,ncol=2)
	.exp<-t(array(rep(1:3,each=2),dim=dim(Z)))
	F_APPLY <- function(x,z) x+z;
	F_ARGS <- function(i,prevRes){list(x=prevRes,z=Z[i,])}	
	.res0 <- c(0,0)
	str(resSeq <- sfFArgsApplyDep( nrow(Z), F_ARGS, F_APPLY, .res0, debugSequential=TRUE))
	(resSeqS <- sfSimplifyLBResult(resSeq))
	checkEquals( .exp, resSeqS )
	str(resPar <- sfFArgsApplyDep( nrow(Z), F_ARGS, F_APPLY, .res0 ))
	resParS <- sfSimplifyLBResult(resSeq)
	checkEquals(.exp, resParS)
	
	sfInit(parallel=TRUE, cpus=2)	#less cpus than dependsStep
	.exp<-structure(rbind(X+1,X+2,X+3),dimnames=NULL)
	F_APPLY <- function(x,z) x+z;
	F_ARGS <- function(i,prevRes){list(x=prevRes,z=c(1,1))}	
	.res0 <- lapply(1:nrow(X),function(row){X[row,]})
	(resSeq <- sfFArgsApplyDep( nrow(.exp), F_ARGS, F_APPLY, .res0, debugSequential=TRUE))
	(resSeqS <- structure(t(sfSimplifyLBResult(resSeq)), dimnames=NULL))
	checkEquals( .exp, resSeqS )
	resPar <- sfFArgsApplyDep( nrow(.exp), F_ARGS, F_APPLY, .res0)
	(resParS <- structure(t(sfSimplifyLBResult(resSeq)), dimnames=NULL))
	checkEquals(.exp, resParS)
	
	# two dimensional case using as many cpus as rows in Z
	Z<-matrix(letters[1:12],nrow=3)
	F_APPLY <- function(x,z) paste(x,z,sep="");
	F_ARGS <- function(i,prevRes){list(x=prevRes,z=Z[i])}	
	.res0 <- rep("",nrow(Z))
	resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0,debugSequential=TRUE)
	res <- matrix(sfSimplifyLBResult(resSeq),nrow=nrow(Z))
	i <- 3
	.exp <- sapply( 1:ncol(Z), function(i){
		t(apply(Z[,1:i,drop=FALSE], 1, paste, collapse=""))
	})
	checkEquals(.exp, res)
	
	resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0,debugSequential=FALSE)
	res <- matrix(sfSimplifyLBResult(resSeq),nrow=nrow(Z))
	checkEquals(.exp, res)
}

