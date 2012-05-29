\name{twRescale}
\alias{twRescale}
\title{twRescale}
\description{Rescale numeric vector to have specified minimum and maximum. }
\usage{
twRescale(x,
to = c(0,
1),
from = range({
    x[!is.finite(x)] <- NA
    x
}, na.rm = TRUE), clip = TRUE)
}
\arguments{
  \item{x}{data to rescale}
  \item{to}{range to scale to}
  \item{from}{range to scale from, defaults to range of data}
  \item{clip}{should values be clipped to specified range?}
}
\details{adapted from package ggplot2 to avoid package redundancies

If from[1] == from[2] then the mean of interval to is returned.}


\author{Hadley Wickham <h.wickham@gmail.com>, Thomas Wutzler
Thomas Wutzler}





