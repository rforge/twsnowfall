\name{updateVersionAndDate}
\alias{updateVersionAndDate}
\title{updateVersionAndDate}
\description{update the version and the date in the DESCRIPTION file }
\usage{
updateVersionAndDate(versionUpdate = c(svnHead = "svnHead",
svnCur = "svnCur",
inc = "inc"))
}
\arguments{
  \item{versionUpdate}{method of updating the version\describe{
\item{svnHead}{replace number after last dot by the subversin head revision +1}
\item{svnCur}{replace number after last dot by highest subversion revision number of the current workspace}
\item{inc}{increment the number after the last dot}
}}
}
\details{Replaces the part after last dot with top revision of current working copy}
\value{character vector: the version and the revisions }

\author{Thomas Wutzler}




\examples{
#mtrace(updateVersionAndDate)
updateVersionAndDate(versionUpdate="svnCur")
{tmp <- scan('DESCRIPTION', what = character(), sep = '\n'); tmp[ grep("Version:",tmp)]}
updateVersionAndDate()
{tmp <- scan('DESCRIPTION', what = character(), sep = '\n'); tmp[ grep("Version:",tmp)]}
updateVersionAndDate(versionUpdate="inc")
{tmp <- scan('DESCRIPTION', what = character(), sep = '\n'); tmp[ grep("Version:",tmp)]}
}
