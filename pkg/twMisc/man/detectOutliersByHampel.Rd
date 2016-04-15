\name{detectOutliersByHampel}
\alias{detectOutliersByHampel}
\title{detectOutliersByHampel}
\description{detect unusual local values, i.e. outliers, in univariate time series by The Hampel Filter  }
\usage{detectOutliersByHampel(x, k = 3, t0 = 3, minWidth = as.integer(1.5 * 
    k))}
\arguments{
  \item{x}{numeric vector representing a time series}
  \item{k}{window length 2*k+1 in indices, default to window length of 5}
  \item{t0}{threshold, default is 3 (Pearson's rule), see below.}
  \item{minWidth}{minimum number of records in window, to apply detection}
}
\details{Based on hampel function of the pracma package

The 'median absolute deviation' computation is done in the [-k...k] vicinity of each point 
at least k steps away from the end points of the interval. 
At the lower and upper end the time series values are preserved.
A high threshold makes the filter more forgiving, a low one will declare 
more points to be outliers. t0<-3 (the default) corresponds 
to Ron Pearson's 3 sigma edit rule, t0<-0 to John Tukey's median filter. 

In each window, NA-values are omitted. If the time resulting series 
is too short, no outlier detection for this index is performed.}
\value{A list with entires
\item{y}{the corrected time series}
\item{ind}{the indices of detected outliers
}}

\author{Thomas Wutzler}




\examples{
set.seed(0815)
x <- diffinv(rnorm(99)/5)	# autocorrelated time series
x[20:25] <- NA
x[10] <- x[10] + median(abs(x[7:13]-x[10]))*3
x[30] <- x[30] - median(abs(x[27:33]-x[30]))*3
(iOut <- (res <- detectOutliersByHampel(x, k=4))$ind)
(iOut <- (res <- detectOutliersBySuperSmoother(x))$ind)
plot( x )
points( x[iOut] ~ iOut, col="blue")
points( res$y[iOut] ~ iOut, col="red")


}
