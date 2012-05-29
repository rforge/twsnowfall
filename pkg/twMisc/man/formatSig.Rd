\name{formatSig}
\alias{formatSig}
\title{formatSig}
\description{format real values to significant number of digits}
\usage{
formatSig(x,
digits = 3)
}
\arguments{
  \item{x}{
}
  \item{digits}{
}
}

\value{character vector with number rounded to significant digits and output including trailing zeros}

\author{Thomas Wutzler}



\seealso{\code{\link{copy2clip}}, \link{twMisc}}
\examples{
x <- c(0.0654,0.06,6,65,99.1,100,100.8,125,1024,2000)
formatSig(x,3)
formatSig(x,2)
}
