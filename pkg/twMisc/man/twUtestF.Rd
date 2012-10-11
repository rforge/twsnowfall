\name{twUtestF}
\alias{twUtestF}
\title{twUtestF}
\description{Running \code{\link{twUtest}} and reporting failed Tests as data.frame}
\usage{
twUtestF(...)
}
\arguments{
  \item{\dots}{arguments passed to twUtest}
}
\details{Enhanced support for checking \itemize{
\item{ Number inside Intervals: \code{\link{checkInterval}} }
\item{ Magnitudes of a number: \code{\link{checkMagnitude}} }
}}
\value{data.frame as returned by \code{\link{as.data.frame.RUnitTestData}}}

\author{Thomas Wutzler}



\seealso{\code{\link{twUtest}}
\code{\link{as.data.frame.RUnitTestData}}

\link{twMisc}}
\examples{
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
