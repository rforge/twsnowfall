\name{detectOutliersBySuperSmoother}
\alias{detectOutliersBySuperSmoother}
\title{detectOutliersBySuperSmoother}
\description{detect unusual local values, i.e. outliers, in univariate time series by The Hampel Filter  }
\usage{detectOutliersBySuperSmoother(x, maxPropResid = 2, 
    ...)}
\arguments{
  \item{x}{numeric vector representing a time series}
  \item{maxPropResid}{minimum factor of resid/(interquantile-range/2) to be classified as outlier (with quantiles=(0.1,0.9))}
  \item{\dots}{further arguments to \code{\link{supsmu}}, such as \code{bass}}
}
\details{NA values are first filled by linear interpolation before applying the smoother}
\value{A list with entires
\item{y}{the corrected time series (outliers replaced by smoothed value)}
\item{ind}{the indices of detected outliers}
\item{ySmooth}{the filled and smoothed time series}
\item{propResid}{resid/(interQuartileRange/2)
}}

\author{Thomas Wutzler}





