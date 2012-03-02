clusterApplyLB <- function(){
	argfun <- function(i) c(list(x[[i]]), list(...))
	dynamicClusterApply(cl, fun, length(x), argfun)
}
trace(clusterApplyLB,recover)	#untrace(clusterApplyLB)