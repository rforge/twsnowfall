\name{twLogSumExp}
\alias{twLogSumExp}
\title{twLogSumExp}
\description{calculates the log(sum(exp(x))) in numerically safer way}
\usage{twLogSumExp(x, shiftUpperBound = FALSE, shiftLowerBound = FALSE)}
\arguments{
  \item{x}{vector to be summed}
  \item{shiftUpperBound}{use this if x has a clear upper bound}
  \item{shiftLowerBound}{use this if x has a clear lower bound}
}
\details{Before taking the exponent, all x are shifted towards zero.
By default the median of x is subtracted. 
If shiftUpperBound then max(x) is subtracted.
This is useful if the distribution of x has a strong left tail but a defined upper bound.
If shiftLowerBound then min(x) is subtracted.
This is useful if the distribution of x has a strong right tail but a defined lower bound.}


\author{Thomas Wutzler}





