#----------- Unit Tests
twUtestF <- function(
	### Running \code{\link{twUtest}} and reporting failed Tests as data.frame
	... ##<< arguments passed to twUtest
){
	##seealso<< 
	## \code{\link{twUtest}}
	## \code{\link{as.data.frame.RUnitTestData}}
	##seealso<< \link{twMisc}
	
	##details<< Enhanced support for checking \itemize{
	## \item{ Number inside Intervals: \code{\link{checkInterval}} }
	## \item{ Magnitudes of a number: \code{\link{checkMagnitude}} }
	##}
	
	testResult <- twUtest(...)
	print(testResult)
	df <- as.data.frame(testResult)
	subset(df,df$kind!="success")
	### data.frame as returned by \code{\link{as.data.frame.RUnitTestData}}
}
attr(twUtestF,"ex") <- function(){
	# demonstrate package development with testCases in subdir inst/unitTests
	packagePath <- system.file(package="twMisc")
	oldwd <- setwd(packagePath)	# dummy testCase with failures in inst/unitTests dir
	
	# running all testUnits
	(nonSucessDf <- twUtestF())
	names(nonSucessDf)
	subset( nonSucessDf,kind=="error" )
	
	# running a single testUnit, specified as string or by object name
	twUtestF("seqRange")
	twUtestF(seqRange)
	
	# running a specific testCase. Note that original output is not diverted
	twUtestF("tw.fTestDummyFailures", "test.failure")
	
	setwd(oldwd)
	
	#invoking a unit test from another package
	twUtestF("SetUp", package="RUnit")	# only works when package RUnit is installed already
}

#mtrace(twUtestF)
#mtrace(twUtest)


twUtest <- function (
	### Running a Testunit from a specified source of a package.
	unitname=NULL,
		### the name of the Testunit or object with the same name. Null for running all test
	testFunc = NULL,
		### Regular expression for matching test functions. Provide function name for executing a single test function (useful for debuggin).
	package = NULL, lib.loc = NULL,
		### see \code{\link{data}}
	verbose = getOption("verbose"), envir = .GlobalEnv,
		### see \code{\link{data}}
	unitTestSubDir="unitTests",
		### the directory in the package where the unit test resides
	#fileExtension=".R",
		### the default extension to of testCase Files	
	divertOutputFile=(if(is.null(testFunc)) tempfile() else NULL),
		### File where to divert the output to, NULL for screen output
		### defaults to tempfile if no test-Function is given 
		### defaults to NULL, i.e. screen output, if testFunction is specified
	unlinkOutputFile=TRUE,
		### if TRUE, output file is deleted after runTestSuite
	mtrace=FALSE,
		### if TRUE, testFunc will be mtraced (see package debug), not implemented yet
	...
		### other argmuents passed to \code{\link{runTestSuite}} or \code{\link{runTestFile}}
){
	##seealso<< 
	## \code{\link{twUtestF}}
	## \code{\link{as.data.frame.RUnitTestData}}
	
	#library(RUnit)
	##details<< 
	## if no test function is given, it uses the pattern "^test.+"
	divertOutputFile	#eval before overwriting testFunc
	#divertOutputFile=NULL	#in mtrace to see output
	if( is.null(testFunc))
		testFunc = "^test.+"
	##details<< if package is not specified, use inst subdir of current directory
	## if package is specified, search it's directory, maybe constrained to lib.loc 
	#pathsTest <- if( is.null(package))
	#	file.path(getwd(),"inst",unitTestSubDir)
	#else{
		paths <- c()
		paths <- c(.find.package(package, lib.loc, verbose = verbose),paths)
		if (is.null(lib.loc)) 
			paths <- c(.path.package(package, TRUE), paths)
		#prepend source
		if( !is.character(package) || !nzchar(package) )
			paths <- c(file.path(getwd(),"inst"), paths)
		else
			paths <- c(file.path(getwd(),package,"inst"), paths)
		paths <- unique(paths)
		pathsTest <- file.path(paths, unitTestSubDir)
	#}
	#filter where testsuite exists and is a directory
	pathsTest <- pathsTest[file.exists(pathsTest)]
	pathsTest <- pathsTest[file_test("-d", pathsTest)]
	if( !length(pathsTest)){ stop("twUtest: no package with testsuite directory found.")}
	if( !is.null(unitname) ){
		if( !is.character(unitname))
			unitname <- deparse(substitute(unitname))	#get the name of the object
		for( unitTestDir in pathsTest ){
			filename=file.path(unitTestDir,paste("runit",unitname,".R",sep=""))
			if( !file_test("-f",filename))
				filename=file.path(unitTestDir,paste("runit",unitname,".r",sep=""))
			## all package directories are searched for given testcase, the first one is executed
			if( file_test("-f",filename) ){  
				##details<< 
				## be default, if no test function is specified, all the verbous screen output is diverted to a temporary file, 
				## which is deleted after the run.
				## specify argument divertOutputFile=NULL to see the output. 
				## Or spcefiy a filename and unlinkOutputFile=FALSE to log the output 
				if( !is.null(divertOutputFile)){
					cat(paste("diverting output to",divertOutputFile,"\n"))
					oldOptions <- tmp <- getOption("RUnit"); tmp$outfile=divertOutputFile; options(RUnit=tmp)
				} 
				filenameP <- paste(substr(filename,1,nchar(filename)-2),".[Rr]",sep="") #pattern to include both R and r
				res <- runTestFile(filenameP,testFuncRegexp=testFunc,...)
				if( !is.null(divertOutputFile)){
					options(RUnit=oldOptions)
					if(unlinkOutputFile) unlink(divertOutputFile)
				}
				return(res)
			}
		}
		stop("twUtest: specified test case not found.")
	}else{
		#entire suite, take the first directory from available paths
		#testsuite.pkg <- defineTestSuite(name=(if(is.null(package)) paste(basename(getwd()),"src",sep="_") else package), dirs=pathsTest[1])
		testsuite.pkg <- defineTestSuite(name=(pathsTest[1]), dirs=pathsTest[1])
		if( !is.null(divertOutputFile)){
			cat(paste("diverting output to",divertOutputFile,"\n"))
			oldOptions <- tmp <- getOption("RUnit"); tmp$outfile=divertOutputFile; options(RUnit=tmp)
		} 
		res <- runTestSuite(testsuite.pkg,  ...)	#not accepted: testFuncRegexp=testFunc
		if( !is.null(divertOutputFile)){
			options(RUnit=oldOptions)
			if(unlinkOutputFile) unlink(divertOutputFile)
		}
		return(res)
	}
	### RUnitTestData, see \code{\link{runTestSuite}}
}
#mtrace(twUtest)

tw.fTestDummy <- function(
	### simple function used in examples of testcases
	a,b
){	a*b }

.tmp.f <- function(
	### interactive development code
){
	library(RUnit)
	#library(debug)
	#currentPackage="twMisc"
	#mtrace(twUtest)
	(res <- twUtest(tw.fTestDummy))
	(res <- twUtest(tw.fTestDummy,divertOutputFile=NULL))
	(res <- twUtest("tw.fTestDummyFailures",unitTestSubDir="inst/unitTests", divertOutputFile=NULL))
	printHTMLProtocol(testResult, fileName=paste("testResults.html",sep=""))
	(res <- twUtest("RUnit",package="RUnit"))
	(res <- twUtest("RUnit"))
	as.data.frame(res)
	printTextProtocol(res, showDetails =FALSE)
	
}

.tmp.f <- function(
### interactive development code
){
	# deprecated: used svUnit, now using RUnit
	# creating unit-Test Files for a specific function
	#library(svUnit)
	unitTestDir="twMisc/inst/unitTests"
	makeUnit(twUtest, dir=unitTestDir)
	
	#for testing twRunTest do it again but including a function
	#again with attaching a Test Function twRunTest
	test(twUtest) <- function(){checkTrue(FALSE,"<twRuntest failed.>")}
	clearLog(); (runTest(twUtest))
	makeUnit(twUtest, dir=unitTestDir)
	# make another unit
	tmp.fsub <- function(){}
	test(tmp.fsub) <- function(){checkTrue(FALSE,"<tmp.fsub failed.>")}
	clearLog(); (runTest(tmp.fsub))
	makeUnit(tmp.fsub, dir=unitTestDir)
	
	#running all TestCases
	(failedUnits <- twUtest("twMisc"))
	lapply(rownames(failedUnits),function(testunit){Log()[[testunit]]})
	
	#running a specified TestCase
	twUtest("twMisc",unitname="bla")
	twUtest("twMisc",unitname=twUtest)
}

#setMethodS3("as.data.frame", "RUnitTestData", function(
# genercis with a dot are not allowed

as.data.frame.RUnitTestData <- function(
		### extract information from RUnit Testresult return value
		x, row.names, optional,	...
){
	##seealso<< \code{\link{twUtestF}}
	testData <- x	#in order to have consistent interface
	if (!is(testData, "RUnitTestData")) {
		stop("Argument 'testData' must be of class 'RUnitTestData'.")
	}
	dfResList2 <- lapply(names(testData),function(suite){
		testDataSrc <- testData[[suite]]
		if( length(testDataSrc$sourceFileResults) == 0){
			return(data.frame(kind="",test="",unit="",suite="",msg="")[FALSE,])
		}
		f.int <- function(filename){
			testunit=gsub("^.*runit(.+)\\.[rR]$","\\1",filename)
			testsRes <- testDataSrc$sourceFileResults[[filename]]
			if( length(testsRes)==0 )
				df <- data.frame(kind="",test="",unit="",suite="",msg="")[FALSE,]
			else	
				df <- data.frame( msg=sapply(testsRes,function(x) ifelse(is.null(x$msg),"",x$msg)), kind=sapply(testsRes,function(x) x$kind ), test=I(names(testsRes)), unit=testunit	)
		}
		dfResList <- lapply(names(testDataSrc$sourceFileResults),f.int)
		dfRes1 <- do.call(rbind, dfResList)
		if( nrow(dfRes1)==0)
			dfRes2 <- cbind(dfRes1[,-1], suite=suite[FALSE], msg=dfRes1$msg)
		else
			dfRes2 <- cbind(dfRes1[,-1], suite=suite, msg=dfRes1$msg)
	})
	dfRes <- do.call(rbind, dfResList2)
	rownames(dfRes)<- NULL
	dfRes
	### dataframe with columns \describe{
	### \item{kind}{factor error failure success.}
	### \item{test}{name of the test function.}
	### \item{unit}{name of the testunit.}
	### \item{suite}{name of the suite.}
	### \item{msg}{message of the test.}}

	##examples<<
	## (testResult <- twUtest(package="twMisc"))
	## tmp <- as.data.frame(testResult); subset(tmp, kind=="success")
}
#mtrace(as.data.frame.RUnitTestData)

#tmp <- as.data.frame(testResult); subset(tmp, kind!="success")


.tmp.f <- function(
### interactive development code
){
	currentPackage="twMisc"
	# executing a single testcase, good for debuggin 
	mtrace(tw.fTestDummy)
	res <- twUtest(tw.fTestDummy,"test.tw.fTestDummy")
	res <- twUtest("tw.fTestDummyFailures",unitTestSubDir="inst/unitTests",divertOutputFile=NULL)
}


checkInterval <- function(
		### checkTrue( all((current >= targetMin) && (current <= targetMax)) )
	current,
	targetMin=0.025,
	targetMax=0.975,
	...
		### further arguments passed to checkTrue
){
	##seealso<< \code{\link{twUtestF}}
	##details<< 
	## all parameters will be recycled int the comparison
	## targetMin and targetMax default to two p of sided 95% confidence interval 
	checkTrue( all((current >= targetMin) && (current <= targetMax)) )
}


checkMagnitude <- function(
	### checkTrue that current is within interval of +-magnitude around target
	target,		##<< numeric vector
	current,	##<< numeric vector (recyled) of the same length as target
	orderOfMagnitudes=1/2,	##<< see details
	...			##<< further arguments passed to checkTrue
){
	##seealso<< \code{\link{twUtestF}}
	##details<< 
	## in range \code{10^{ log10(current)+-orderOfMagnitudes } }
	.range <- 10^t( sapply(target, function(par){ log10(par)+orderOfMagnitudes*c(-1,+1) }))
	checkInterval(current,.range[,1],.range[,2],...)
	### TRUE or error
}
#mtrace(checkMagnitude)
attr(checkMagnitude,"ex") <- function(){
	target=1e4
	checkMagnitude(1e4, 3e4)	#TRUE
	try(checkMagnitude(1e4, 6e4))	#error
	checkMagnitude(1e4, 5000)	#TRUE
	try(checkMagnitude(1e4, 2000))	#error
	
	checkMagnitude(1e-4, 3e-4)	#TRUE
	try(checkMagnitude(1e-4, 6e-4))	#error
	checkMagnitude(1e-4, 5e-5)	#TRUE
	try(checkMagnitude(1e-4, 2e-5))	#error
}

