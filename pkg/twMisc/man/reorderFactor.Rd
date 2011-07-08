\name{reorderFactor}
\alias{reorderFactor}
\title{reorderFactor}
\description{reorder the factor levels in a factor}
\usage{
reorderFactor(x, newLevels)
}
\arguments{
  \item{x}{factor to reorder}
  \item{newLevels}{character vector: levels in new order}
}

\value{factor with levels corresponding to newLevels}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{copy2clip}}, \link{twMisc}}
\examples{
x <- as.factor(sample(c("low","medium","high"),10,replace=TRUE) )
x
as.integer(x)
y <- reorderFactor(x,c("low","medium","high"))
y
as.integer(y)
all(x == y)
}
