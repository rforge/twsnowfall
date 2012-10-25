\name{twCalcObsScale}
\alias{twCalcObsScale}
\title{twCalcObsScale}
\description{Calculate mean slopes, i.e. rates, between within time intervals.}
\usage{twCalcObsScale(dx, times1 = NULL, times2 = NULL, na.rm = TRUE, 
    doSort = TRUE)}
\arguments{
  \item{dx}{numeric vector: slopes at time points times}
  \item{times1}{
}
  \item{times2}{
}
  \item{na.rm}{can be set to FALSE for performance reasons}
  \item{doSort}{can be set to FALSE for performance reasons}
}
\details{Calculate the derivative for next intervals (mean of the two borders).
At the end of times1 it corresponds to the slope at last  time point.}
\value{list with components \describe{
\item{oneSided}{slope within the interval (i,i+1)}
\item{int}{same for intervals times2 (NULL if either times1 or times2 was NULL)}
}}

\author{Thomas Wutzler}





