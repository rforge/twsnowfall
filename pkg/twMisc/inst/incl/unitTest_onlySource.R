#----------- Unit Tests
twUtest <- function(
        ### Running a Testunit from a specified source of a package.
    unitname=NULL,
        ### the name of the Testunit or object with the same name. Null for running all test
    testFunc = NULL,
        ### Regular expression for matching test functions. Provide function name for executing a single test function (useful for debuggin).
    pkg=NULL, #eval(parse(text="currentPackage")),
        ### the name of the unitTest (or object with the same name)
    #unitTestDir=file.path(pkg,"inst","unitTests"),
    unitTestDir=file.path("inst","unitTests"),
        ### the directory where the unit test resides
    fileExtension=".R",
        ### the default extension to of testCase Files  
    divertOutputFile=(if(is.null(testFunc)) tempfile() else NULL),
        ### File where to divert the output to, NULL for screen output
    unlinkOutputFile=TRUE,
        ### if TRUE, output file is deleted after runTestSuite
    ...
        ### other argmuents passed to runTestSuite or runTestFile
){
    ##seealso<< 
    ## \code{\link{twUtestF}}
    ## \code{\link{as.data.frame.RUnitTestData}}
    
    #library(RUnit)
    ##details<< 
    ## if no test function is given, it uses the pattern "^test.+"
    divertOutputFile    #eval before overwriting testFunc
    if( is.null(testFunc))
        testFunc = "^test.+"
    if( !is.null(unitname) ){
        if( !is.character(unitname))
            unitname <- deparse(substitute(unitname))   #get the name of the object
        filename=paste(unitTestDir,"/runit",unitname,fileExtension,sep="")
        ##details<< 
        ## be default, if no test function is specified, all the verbous screen output is diverted to a temporary file, 
        ## which is deleted after the run.
        ## specify argument divertOutputFile=NULL to see the output. 
        ## Or spcefiy a filename and unlinkOutputFile=FALSE to log the output 
        if( !is.null(divertOutputFile)){
            cat(paste("diverting output to",divertOutputFile,"\n"))
            oldOptions <- tmp <- getOption("RUnit"); tmp$outfile=divertOutputFile; options(RUnit=tmp)
        } 
        res <- runTestFile(filename,testFuncRegexp=testFunc,...)
        if( !is.null(divertOutputFile)){
            options(RUnit=oldOptions)
            if(unlinkOutputFile) unlink(divertOutputFile)
        }
        res
    }else{
        #entire suite
        testsuite.pkg <- defineTestSuite(pkg, dirs=unitTestDir)
        if( !is.null(divertOutputFile)){
            cat(paste("diverting output to",divertOutputFile,"\n"))
            oldOptions <- tmp <- getOption("RUnit"); tmp$outfile=divertOutputFile; options(RUnit=tmp)
        } 
        res <- runTestSuite(testsuite.pkg,  ...)    #not accepted: testFuncRegexp=testFunc
        if( !is.null(divertOutputFile)){
            options(RUnit=oldOptions)
            if(unlinkOutputFile) unlink(divertOutputFile)
        }
        res
    }
    ### RUnitTestData, see \code{\link{runTestSuite}}
    
    ##examples<< 
    ## \dontrun{
    ## tw.fTestDummy    #show function
    ## currentPackage="twMisc"  #assume sources in this subdirectory 
    ## (res <- twUtest(tw.fTestDummy)
    ## 
    ## # same with printing output to screen
    ## (res <- twUtest(tw.fTestDummy,divertOutputFile=NULL)
    ## 
    ## # run a single testcase (usefule for debugging)
    ## twUtest(tw.fTestDummy,"test.tw.fTestDummy")
    ## }
}
#mtrace(twUtest)

tw.fTestDummy <- function(
    ### simple function used in examples of testcases
    a,b
){  a*b }

.tmp.f <- function(){
    library(RUnit)
    currentPackage="twMisc"
    (res <- twUtest(tw.fTestDummy))
    printTextProtocol(res, showDetails =FALSE)
    getErrors(res)
    summary(res)
    
    testResult <- twUtest()
    printHTMLProtocol(testResult, fileName=paste(currentPackage,"_testResults.html",sep=""))
    
}

.tmp.f <- function(){
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

setMethodS3("as.data.frame", "RUnitTestData", function(
        ### extract information from RUnit Testresult return value
        x, row.names, optional, ...
){
    testData <- x   #in order to have consistent interface
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
            df <- data.frame(   msg=sapply(testsRes,function(x) ifelse(is.null(x$msg),"",x$msg)), kind=sapply(testsRes,function(x) x$kind ), test=I(names(testsRes)), unit=testunit )
        }
        dfResList <- lapply(names(testDataSrc$sourceFileResults),f.int)
        dfRes1 <- do.call(rbind, dfResList)
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
    ## \dontrun{
    ## currentPackage="twMisc"  #assume sources in this subdirectory 
    ## testResult <- twUtest(tw.fTestDummy)
    ## tmp <- as.data.frame(testResult); subset(tmp, kind!="success")
    ## tmp
    ## }
})
#mtrace(as.data.frame.RUnitTestData)

#tmp <- as.data.frame(testResult); subset(tmp, kind!="success")

twUtestF <- function(
        ### Running \code{\link{twUtest}} and reporting failed Tests as data.frame
    ...
        ### arguments passed to twUtest
){
    testResult <- twUtest(...)
    print(testResult)
    df <- as.data.frame(testResult)
    subset(df,df$kind!="success")
    ### data.frame as returned by \code{\link{as.data.frame.RUnitTestData}}
}
#mtrace(twUtestF)
#mtrace(twUtest)

.tmp.f <- function(){
    #diverting output does not work, internally erorrs are sent to stdOuput
    #twUtestF(tw.fTestDummy, divertOutputFile=NULL)
    #twUtestF(tw.fTestDummy, divertOutputFile="tmp.txt", unlinkOutputFile=FALSE)

    # tests do reside in other directory because they demonstrate fails and would prevent check and installation
    currentPackage="twMisc"
    twUtestF("tw.fTestDummyFailures", unitTestDir=paste(currentPackage,"/inst/test_unitTests",sep=""))
    twUtestF("tw.fTestDummyFailure", unitTestDir=paste(currentPackage,"/inst/test_unitTests",sep=""))
    twUtestF(unitTestDir=paste(currentPackage,"/inst/test_unitTests",sep=""))
    
    #not in TestCase because will fail of changed directory structure (missing inst) after installation
}

.tmp.f <- function(){
    currentPackage="twMisc"
    # executing a single testcase, good for debuggin 
    mtrace(tw.fTestDummy)
    res <- twUtest(tw.fTestDummy,"test.tw.fTestDummy")
    res <- twUtest(tw.fTestDummy,"test.tw.fTestDummy",divertOutputFile=NULL)
    
    #oFile <- getOption("RUnit")$outfile    
    #tmp <- getOption("RUnit"); tmp$silent=FALSE; options(RUnit=tmp)
    #tmp <- getOption("RUnit"); tmp$silent=TRUE; options(RUnit=tmp)
    #oFile <- getOption("RUnit")$outfile
    #tmp <- getOption("RUnit"); tmp$outfile="Runit_out.txt"; options(RUnit=tmp)
    #tmp <- getOption("RUnit"); tmp$outfile=NULL; options(RUnit=tmp)
}

