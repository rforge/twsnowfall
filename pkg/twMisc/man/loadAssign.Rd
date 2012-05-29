\name{loadAssign}
\alias{loadAssign}
\title{loadAssign}
\description{Load a RData file and return the value of the first entry}
\usage{
loadAssign(...)
}
\arguments{
  \item{\dots}{arguments to \code{\link{load}}}
}
\details{The load function is evaluated in a local environment.
Then the value of the first entry of ls in that environment is returned.}


\author{Thomas Wutzler}



\seealso{\code{\link{copy2clip}}, \link{twMisc}}
\examples{
# save the filename character into a temporary file 
fout <- fout2 <- file.path(tempdir(),"tmp.RData")
save(fout,file=fout)
fout <- "changed"
(x <- loadAssign(file=fout2))	
fout				# note that is has not been overwritten with load
}
