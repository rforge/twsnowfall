\name{matchClosest}
\alias{matchClosest}
\title{matchClosest}
\description{Find index in b that is closest to a.}
\usage{
matchClosest(a, b, maxAbsDiff = Inf, isSortedA = FALSE, isSortedB = FALSE, allFiniteB = FALSE, chunkSize = round(length(b)/length(a) * 1.6))
}
\arguments{
  \item{a}{numeric vector}
  \item{b}{numeric vector}
  \item{maxAbsDiff}{if maximum difference between best b and a[i] is larger than this, return NA for index i}
  \item{isSortedA}{if TRUE assures that A is sorted to speed up}
  \item{isSortedB}{if TRUE assures that B is sorted to speed up}
  \item{allFiniteB}{if TRUE assures that B contains only finite entries to speed up}
  \item{chunkSize}{length of the vector to calculate difference for each point in a}
}
\details{For each a_i the c_i=j will be selected so that b_j is closest to a_i among all b
If a_i is not finite, then c_i is NA}
\value{vector of length of a, with each element holding an index from b}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\link{twMisc}}
\examples{
a = seq(0,1,length.out=11)
b = sort(runif(length(a)))
#mtrace(matchClosest)
c = matchClosest(a,b,isSortedA=TRUE,isSortedB=TRUE)

plot(b~a)
points(b[c]~a, col="red", pch=2)
abline(0,1)
}
