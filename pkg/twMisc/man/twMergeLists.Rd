\name{twMergeLists}
\alias{twMergeLists}
\title{twMergeLists}
\description{Recursively merge a list or vector to another list or vector}
\usage{twMergeLists(dest, source, level = 0)}
\arguments{
  \item{dest}{list or vector that values are merged to}
  \item{source}{list or vector that values are taken from}
  \item{level}{level of recursion, used internally}
}
\details{Values of source are appended to dest or overwrite values in dest.
If an item is a vector or a list itself, the item is merged recursively. }
\value{modified argument dest}

\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
dest0 <- list(a=1, b=list(b1=2, b2=4), c=2)
source0 <- list(b=list(b1=12))
(tmp <- twMergeLists(dest0,source0))
#
dest0 <- list(a=1, b=c(b1=2, b2=4), c=2)
source0 <- list(b=c(b1=12))
(tmp <- twMergeLists(dest0,source0))

(tmp <- twMergeLists( list(), source0) )

(tmp <- twMergeLists( dest0, NULL) )

}
