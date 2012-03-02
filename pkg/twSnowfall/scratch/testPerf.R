# observed on the cluster that one process is working at 100% always and the others below
# possible diagnostics: the distributing process is busy and can not distribute
# if true: keep one masternode free from assigning jobs
# which is the master node?
f1 <- function(n){	i=0;	while(i < n){	i=i+1	}  }
system.time( f1(2e6) )
sfExport("f1")

(Z<-matrix(letters[1:12],nrow=3))
F_APPLY <- function(x,z){f1(2e6); paste(x,z,sep="") };
F_ARGS <- function(i,prevRes){list(x=prevRes,z=Z[i])}	
.res0 <- rep("_",nrow(Z))	# dependStep will be length of .res0
resSeq <- sfFArgsApplyDep( length(Z), F_ARGS, F_APPLY, .res0)
(res <- matrix(sfSimplifyLBResult(resSeq),nrow=nrow(Z)))
