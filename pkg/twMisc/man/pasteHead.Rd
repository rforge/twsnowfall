\name{pasteHead}
\alias{pasteHead}
\title{pasteHead}
\description{Collapse each argument to at most nHead elements and apply fPasteItem to each one }
\usage{pasteHead(..., nHead = 3L, fPasteItem = pasteCollapseNamed)}
\arguments{
  \item{\dots}{arguments to paste together}
  \item{nHead}{number of elements to include}
  \item{fPasteItem}{collapse function applied to each argument.
Defaults to \code{\link{pasteCollapseNamed}}.}
}



\author{Thomas Wutzler}




\examples{
pasteHead("short vector is: ",c(a=1,b=2)," by string.")
pasteHead("longer vector is: ",1:5," by string.")
}
