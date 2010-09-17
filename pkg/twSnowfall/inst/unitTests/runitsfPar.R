.setUp <- function(){
	.setUpDf <- within( list(),{
			fList <- list(
				function(...){ "Hello world!" }	# put the dots so that other functoins can have arguments
				,function(a){ a }
			)
		})
	attach(.setUpDf)
}
.tearDown <- function () {
	#detach(.setUpDf)
	detach()
}


test.sfParInternal <- function(){
	#mtrace(sfParInternal)
	checkEquals("Hello world!", .sfParInternal( 1, fList, a="ah"))
	checkEquals("ah", .sfParInternal( 2, fList, a="ah"))
	checkException( .sfParInternal( 2, fList))	#error because argument is missing
	checkException( .sfParInternal( 2, fList, sfParArgsList="ah"))	#error message because is not a list
}

test.sfParSequential <- function(){
	.exp <- list("Hello world!","aha")
	checkEquals(.exp, sfPar( fList, sfParParallel=FALSE, a="aha" ))
	checkEquals(.exp, sfPar( fList, sfParParallel=FALSE, sfParArgsList=list(a="aha") ))	
	checkException( sfPar( fList, sfParParallel=FALSE, "aha" ))	#error
	checkException( sfPar( fList, sfParParallel=FALSE, sfParArgsList=list(a="aha"), a="ahb" ))	
	
	checkEquals(.exp,sfPar( fList, sfParArgsList=list(a="aha") ))	
	checkEquals(.exp,sfPar( fList, a="aha" ))
	
	aPre = "aha"
	checkEquals(.exp,sfPar( fList, a=as.name("aPre") ))	#should work, ... are evaluated before distribution
	if( sfParallel() ){
	  checkException(sfPar( fList, sfParArgsList=list(a=as.name("aPre")) ))	#should give an error because not yet exported
	  sfExport("aPre")
	  checkEquals(.exp, sfPar( fList, sfParArgsList=list(a=as.name("aPre")) ))	#should work now
 	 }
}
