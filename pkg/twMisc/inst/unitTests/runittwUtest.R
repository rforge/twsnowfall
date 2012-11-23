# RUnit logger gets confused when invoking runTestSuite within a runTestSuite
# so do not implement this code as tests

t_st.localExecution <- function() {
    packagePath <- .find.package("twMisc")
    oldwd <- setwd(packagePath)
    # now inst/unitTests is a subdirectory with file runittw.fTestDummyFailures.R
    (res <- twUtest("tw.fTestDummyFailures"))
    checkEquals( c(nTestFunc=3,nErr=1,nFail=1), unlist(res[[1]][c("nTestFunc","nErr","nFail")]))
    setwd(oldwd)
}

t_st.packageExecution <- function() {
    (res <- twUtest("tw.fTestDummy",package="twMisc"))
    checkEquals( c(nTestFunc=1,nErr=0,nFail=0), unlist(res[[1]][c("nTestFunc","nErr","nFail")]))
}

t_st.setTestSubDir <- function() {
    unitTestSubDir = file.path("inst","unitTests")
    (res <- twUtest("tw.fTestDummyFailures", package="twMisc",unitTestSubDir=unitTestSubDir))
    checkEquals( c(nTestFunc=3,nErr=1,nFail=1), unlist(res[[1]][c("nTestFunc","nErr","nFail")]))
}

t_st.setTestSubDirSuite <- function() {
    #mtrace(twUtest)
    unitTestSubDir = file.path("inst","unitTests")
    (res <- twUtest(package="twMisc",unitTestSubDir=unitTestSubDir))
    checkEquals( c(nTestFunc=3,nErr=1,nFail=1), unlist(res[[1]][c("nTestFunc","nErr","nFail")]))
}

t_est.otherPackage <- function() {
    #mtrace(twUtest)
    (res <- twUtest("RUnit",package="RUnit"))
    checkTrue( res[[1]]$nTestFunc > 0 )
}


