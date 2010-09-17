test.one <- function(){
	x <- rnorm(100)
	r0 <- log(sum(exp(x)))
	#mtrace(twLogSumExp)
	res <- twLogSumExp(x)
	checkEqualsNumeric(r0,res)
	
	x2 <- -1000+x
	exp(x2)	#zero
	log(mean(exp(x2)))
	res <- twLogMeanExp(x2)
	hist(x2)
	abline(v=res,col="red")
	abline(v=mean(x2),col="blue")
	checkTrue( res < max(x2))
	checkTrue( res > mean(x2))
}

