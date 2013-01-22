\name{twBinOptimize.function}
\alias{twBinOptimize.function}
\title{twBinOptimize function}
\description{Optimizing a function where first argument is an index. }
\usage{\method{twBinOptimize}{function}(x, target = 0, ..., interval, lower = ceiling(min(interval)), 
    upper = floor(max(interval)), maxiter = 100, showiter = FALSE)}
\arguments{
  \item{x}{the monotonic vectorized function to be optimized. The term abs(x(i,...)-target) is minimized.}
  \item{target}{Target value for \code{x}. Defaults to 0.}
  \item{\dots}{additional named or unnamed arguments to be passed to f.}
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
\item{value}{Value of the function \code{x} at the values of \code{where}.}}

\author{Thomas Wutzler}



\seealso{twMisc}
\examples{FALSE}
