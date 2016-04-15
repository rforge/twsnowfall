\name{asCharacterWithLeading}
\alias{asCharacterWithLeading}
\title{asCharacterWithLeading}
\description{convert integer to string with fixed width by leading zeros (or spaces)}
\usage{asCharacterWithLeading(x, template = "00")}
\arguments{
  \item{x}{integer vector: number to convert to string}
  \item{template}{scalar string of intended output length with specifying the leading characters.}
}



\author{Thomas Wutzler}




\examples{
x <- c(1,11,111)
asCharacterWithLeading(x,"000")
try( asCharacterWithLeading(x) )
}
