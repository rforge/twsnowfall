\name{twSmoothSwitch}
\alias{twSmoothSwitch}
\title{twSmoothSwitch}
\description{Smooth increase from 0 to 1 around b.}
\usage{
twSmoothSwitch(x, b = 0, smooth.range.width = 0.1 * b)
}
\arguments{
  \item{x}{numeric vector: times to evaluate}
  \item{b}{numeric scalar: time of switch}
  \item{smooth.range.width}{width of smooth increase}
}

\value{0 for x <= b - r/2 \cr
sin(x-b)/r*pi for b-r/2 <= x <= b+r/2 \cr
1 for b - r/2 <= x \cr}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





