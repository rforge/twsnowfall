#install.packages("RUnit")
library(RUnit)

system.file("examples", package = "RUnit")

currentPackage="twMisc"
source(paste(currentPackage,"/R/unitTest.R",sep=""))


function (testData, fileName = "", separateFailureList = TRUE, 
		showDetails = TRUE, traceBackCutOff = 9) 
{
	if (!is(testData, "RUnitTestData")) {
		stop("Argument 'testData' must be of class 'RUnitTestData'.")
	}
	if (!is.character(fileName)) {
		stop("Argument 'fileName' has to be of type character.")
	}
	if (length(fileName) != 1) {
		stop("Argument 'fileName' must contain exactly one element.")
	}
	if (!is.logical(separateFailureList)) {
		stop("Argument 'separateFailureList' has to be of type logical.")
	}
	if (length(separateFailureList) != 1) {
		stop("Argument 'separateFailureList' must contain exactly one element.")
	}
	if (!is.logical(showDetails)) {
		stop("Argument 'showDetails' has to be of type logical.")
	}
	if (length(showDetails) != 1) {
		stop("Argument 'showDetails' must contain exactly one element.")
	}
	if (!is.numeric(traceBackCutOff)) {
		stop("Argument 'traceBackCutOff' has to be of type logical.")
	}
	if (length(traceBackCutOff) != 1) {
		stop("Argument 'traceBackCutOff' must contain exactly one element.")
	}
	if (traceBackCutOff < 0 || traceBackCutOff > 100) {
		stop("Argument 'traceBackCutOff' out of valid range [0, 100].")
	}
	pr <- function(..., sep = " ", nl = TRUE) {
		if (nl) {
			cat(..., "\n", file = fileName, append = TRUE, sep = sep)
		}
		else {
			cat(..., file = fileName, append = TRUE, sep = sep)
		}
	}
	sop <- function(number, word, plext = "s") {
		ifelse(number == 1, paste(number, word), paste(number, 
						paste(word, plext, sep = "")))
	}
	cat("RUNIT TEST PROTOCOL --", date(), "\n", file = fileName)
	pr("***********************************************")
	if (length(testData) == 0) {
		pr("no test cases :-(")
		return(invisible(TRUE))
	}
	errInfo <- getErrors(testData)
	pr("Number of test functions:", errInfo$nTestFunc)
	if (errInfo$nDeactivated > 0) {
		pr("Number of deactivated test functions:", errInfo$nDeactivated)
	}
	pr("Number of errors:", errInfo$nErr)
	pr("Number of failures:", errInfo$nFail, "\n\n")
	pr(sop(length(testData), "Test Suite"), ":")
	for (tsName in names(testData)) {
		pr(tsName, " - ", sop(testData[[tsName]]$nTestFunc, "test function"), 
				", ", sop(testData[[tsName]]$nErr, "error"), ", ", 
				sop(testData[[tsName]]$nFail, "failure"), sep = "")
		if (separateFailureList && (testData[[tsName]]$nErr + 
					testData[[tsName]]$nFail > 0)) {
			srcFileRes <- testData[[tsName]][["sourceFileResults"]]
			for (i in seq_along(srcFileRes)) {
				testFuncNames <- names(srcFileRes[[i]])
				for (j in seq_along(testFuncNames)) {
					funcList <- srcFileRes[[i]][[testFuncNames[j]]]
					if (funcList$kind == "error") {
						pr("ERROR in ", testFuncNames[j], ": ", funcList$msg, 
								nl = FALSE, sep = "")
					}
					else if (funcList$kind == "failure") {
						pr("FAILURE in ", testFuncNames[j], ": ", 
								funcList$msg, sep = "", nl = FALSE)
					}
					else if (funcList$kind == "deactivated") {
						pr("DEACTIVATED ", testFuncNames[j], ": ", 
								funcList$msg, sep = "", nl = FALSE)
					}
				}
			}
		}
	}
	if (!showDetails) {
		return(invisible(TRUE))
	}
	pr("\n\n\nDetails")
	for (tsName in names(testData)) {
		tsList <- testData[[tsName]]
		pr("***************************")
		pr("Test Suite:", tsName)
		pr("Test function regexp:", tsList$testFuncRegexp)
		pr("Test file regexp:", tsList$testFileRegexp)
		if (length(tsList$dirs) == 0) {
			pr("No directories !")
		}
		else {
			if (length(tsList$dirs) == 1) {
				pr("Involved directory:")
			}
			else {
				pr("Involved directories:")
			}
			for (dir in tsList$dirs) {
				pr(dir)
			}
			res <- tsList$sourceFileResults
			testFileNames <- names(res)
			if (length(res) == 0) {
				pr("no test files")
			}
			else {
				for (testFileName in testFileNames) {
					testFuncNames <- names(res[[testFileName]])
					if (length(testFuncNames) > 0) {
						pr("---------------------------")
						pr("Test file:", testFileName)
						for (testFuncName in testFuncNames) {
							testFuncInfo <- res[[testFileName]][[testFuncName]]
							if (testFuncInfo$kind == "success") {
								pr(testFuncName, ": (", testFuncInfo$checkNum, 
										" checks) ... OK (", testFuncInfo$time, 
										" seconds)", sep = "")
							}
							else {
								if (testFuncInfo$kind == "error") {
									pr(testFuncName, ": ERROR !! ", sep = "")
								}
								else if (testFuncInfo$kind == "failure") {
									pr(testFuncName, ": FAILURE !! (check number ", 
											testFuncInfo$checkNum, ")", sep = "")
								}
								else if (testFuncInfo$kind == "deactivated") {
									pr(testFuncName, ": DEACTIVATED, ", 
											nl = FALSE)
								}
								else {
									pr(testFuncName, ": unknown error kind", 
											sep = "")
								}
								pr(testFuncInfo$msg, nl = FALSE)
								if (length(testFuncInfo$traceBack) > 
										0) {
									pr("   Call Stack:")
									if (traceBackCutOff > length(testFuncInfo$traceBack)) {
										pr("   (traceBackCutOff argument larger than length of ", 
												"trace back: full trace back printed)")
										for (i in 1:length(testFuncInfo$traceBack)) {
											pr("   ", i, ": ", testFuncInfo$traceBack[i], 
													sep = "")
										}
									}
									else {
										for (i in traceBackCutOff:length(testFuncInfo$traceBack)) {
											pr("   ", 1 + i - traceBackCutOff, 
													": ", testFuncInfo$traceBack[i], 
													sep = "")
										}
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return(invisible(TRUE))
}















