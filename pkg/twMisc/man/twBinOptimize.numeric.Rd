\name{twBinOptimize.numeric}
\alias{twBinOptimize.numeric}
\title{twBinOptimize numeric}
\description{Finding the index of a sorted numeric vector whose element is closest to target. }
\usage{
twBinOptimize.numeric(x, target = 0, ..., interval = c(1, length(x)), lower = ceiling(min(interval)), upper = floor(max(interval)), maxiter = 100, showiter = FALSE)
}
\arguments{
  \item{x}{numeric vector to be optimized. The term abs(x[i])-target) is minimized.}
  \item{target}{Target value for \code{x}. Defaults to 0.}
  \item{\dots}{not used}
  \item{interval}{a vector containing the end-points of the interval to be searched for target.}
  \item{lower}{the lower end point of the interval to be searched.}
  \item{upper}{the upper end point of the interval to be searched.}
  \item{maxiter}{Maximum number of search iterations. Defaults to 100.}
  \item{showiter}{Boolean flag indicating whether the algorithm state should be printed at each iteration. Defaults to FALSE.}
}
\details{This function can be applied similar to \code{\link{optimize}} for cases, where the argument
to be optimized is an index instead of a contnuous variable. 
A binary search over the index is performed.
\cr The function \code{x} must be monotonic. If it is not strictly monotonic the returned
index (\code{$where}) can be any of the indices with equal values. 
\cr The code is based on binsearch of the gtools package. In difference to the original code, 
it returns always only one best estimate.}
\value{A list containing:
\item{call}{How the function was called.}
\item{numiter}{The number of iterations performed}
\item{flag }{One of the strings,  "Found", "Between Elements",
    "Maximum number of iterations reached", "Reached lower boundary", or
    "Reached upper boundary."}
\item{where}{One or two values indicating where the search terminated.}
\item{value}{Value of the \code{x} at the index of \code{where}.}
If vector is empty or upper boundary is lower than lower boundary, \code{where} and \code{value} are NA}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{twBinOptimize.function}}, \code{\link{twMisc}}}
\examples{
# linear search is faster up with vectors to about 1e4 entries 
x <- exp(seq(-10,100,length.out=1e4))
# with longer vectors, the binary search is superior
x <- exp(seq(-10,100,length.out=1e6))

# generate some sample indices that will be found by optimization
n <- 1e2
.where <- sample.int(length(x),n, replace=TRUE)
.val <- x[.where]*1.00001	# add some error, for exact matches use the faster function match
system.time( .where2 <- sapply(.val, function(vali){ which.min(abs(x-vali))  } ))	
system.time( .where3 <- sapply(.val, function(vali){ twBinOptimize(x,vali)$where} ))
#Rprof();  .numiter3 <- sapply(.val, function(vali){ twBinOptimize(x,vali)$numiter} ); Rprof(NULL);	summaryRprof()
}
