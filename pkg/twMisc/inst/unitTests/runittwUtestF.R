t_est.setTestSubDir <- function() {
	unitTestSubDir = file.path("inst","unitTests")
	(res <- twUtestF("tw.fTestDummyFailures", package="twMisc",unitTestSubDir=unitTestSubDir))
	checkEquals( 2, nrow(res) )
	(resErr <- sapply(res[res$test=="test.errordemo",1:3],as.character))
	checkEquals(c(kind="error", test="test.errordemo", unit="tw.fTestDummyFailures"), resErr)
}

t_est.setTestSubDirSuite <- function() {
	#mtrace(twUtest)
	unitTestSubDir = file.path("inst","unitTests")
	(res <- twUtestF(package="twMisc",unitTestSubDir=unitTestSubDir))
	(resErr <- sapply(res[res$test=="test.errordemo",1:4],as.character))
	checkEquals(c(kind="error", test="test.errordemo", unit="tw.fTestDummyFailures",suite="twMisc"), resErr)
}



