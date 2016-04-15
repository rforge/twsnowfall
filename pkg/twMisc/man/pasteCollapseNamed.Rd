\name{pasteCollapseNamed}
\alias{pasteCollapseNamed}
\title{pasteCollapseNamed}
\description{paste arguments by name=value }
\usage{pasteCollapseNamed(x, collapse = ",", digits = 4L)}
\arguments{
  \item{x}{object to be collapsed into string}
  \item{collapse}{delimited to separate items in vector x}
  \item{digits}{number of digits for numeric values}
}
\details{If x is a list, then \code{\link{pasteHead}} is applied to each item. }
\value{a scalar string, surrounded by () if x is not a scalar.}

\author{Thomas Wutzler}



\seealso{\code{\link{pasteHead}}}
\examples{
pasteCollapseNamed( c(a=1) )
pasteCollapseNamed( c(a=pi,b=2) )
pasteCollapseNamed( c() )
pasteCollapseNamed( tmp <- list(aSub=list(c(a1=pi,a2=2,a3=3,a4=4)),b="b",c=c()) )
}
