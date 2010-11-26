\name{twRescale}
\alias{twRescale}
\title{twRescale}
\description{Rescale numeric vector to have specified minimum and maximum. }
\usage{
twRescale(x, to = c(0, 1), from = range(x, na.rm = TRUE), clip = TRUE)
}
\arguments{
  \item{x}{data to rescale}
  \item{to}{range to scale to}
  \item{from}{range to scale from, defaults to range of data}
  \item{clip}{should values be clipped to specified range?}
}
\details{copied from package ggplot2 to avoid package redundancies}


\author{Hadley Wickham <h.wickham@gmail.com>
Thomas Wutzler <twutz@bgc-jena.mpg.de>}





