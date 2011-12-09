\name{twUtest}
\alias{twUtest}
\title{twUtest}
\description{Running a Testunit from a specified source of a package.}
\usage{
twUtest(unitname = NULL,
testFunc = NULL,
package = NULL,
lib.loc = NULL,
verbose = getOption("verbose"),
envir = .GlobalEnv,
unitTestSubDir = "unitTests",
divertOutputFile = (if (is.null(testFunc)) tempfile() else NULL),
unlinkOutputFile = TRUE,
mtrace = FALSE,
...)
}
\arguments{
  \item{unitname}{the name of the Testunit or object with the same name. Null for running all test}
  \item{testFunc}{Regular expression for matching test functions. Provide function name for executing a single test function (useful for debuggin).}
  \item{package}{see \code{\link{data}}}
  \item{lib.loc}{
}
  \item{verbose}{see \code{\link{data}}}
  \item{envir}{
}
  \item{unitTestSubDir}{the directory in the package where the unit test resides}
  \item{divertOutputFile}{File where to divert the output to, NULL for screen output
defaults to tempfile if no test-Function is given 
defaults to NULL, i.e. screen output, if testFunction is specified}
  \item{unlinkOutputFile}{if TRUE, output file is deleted after runTestSuite}
  \item{mtrace}{if TRUE, testFunc will be mtraced (see package debug), not implemented yet}
  \item{\dots}{other argmuents passed to \code{\link{runTestSuite}} or \code{\link{runTestFile}}}
}
\details{if no test function is given, it uses the pattern "^test.+"

if package is not specified, use inst subdir of current directory
if package is specified, search it's directory, maybe constrained to lib.loc 

be default, if no test function is specified, all the verbous screen output is diverted to a temporary file, 
which is deleted after the run.
specify argument divertOutputFile=NULL to see the output. 
Or spcefiy a filename and unlinkOutputFile=FALSE to log the output }
\value{RUnitTestData, see \code{\link{runTestSuite}}}

\author{Thomas Wutzler}



\seealso{\code{\link{twUtestF}}
\code{\link{as.data.frame.RUnitTestData}}}

