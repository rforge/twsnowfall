\name{cutQuantiles}
\alias{cutQuantiles}
\title{cutQuantiles}
\description{Cut a Numeric Variable into Intervals of about same number of observations.}
\usage{
cutQuantiles(x, cuts, m = 150, g, levels.mean = FALSE, digits, minmax = TRUE, oneval = TRUE, onlycuts = FALSE, onlymeans = TRUE)
}
\arguments{
  \item{x}{numeric vector to classify into intervals}
  \item{cuts}{cut points}
  \item{m}{desired minimum number of observations in a group}
  \item{g}{number of quantile groups}
  \item{levels.mean}{set to TRUE to make the new categorical vector have levels attribute that is the group means of x instead of interval endpoint labels}
  \item{digits}{number of significant digits to use in constructing levels. Default is 3 (5 if levels.mean=TRUE)}
  \item{minmax}{if cuts is specified but min(x)<min(cuts) or max(x)>max(cuts), augments cuts to include min and max x}
  \item{oneval}{if an interval contains only one unique value, the interval will be labeled with the formatted version of that value instead of the interval endpoints, unless oneval=FALSE}
  \item{onlycuts}{set to TRUE to only return the vector of computed cuts. This consists of the interior values plus outer ranges.}
  \item{onlymeans}{set to TRUE to only return the means of x within each group}
}
\details{copied from Hmisc:cutQuantiles to reduce package dependencies.}
\value{a factor variable with levels of the form [a,b) or formatted means (character strings) unless onlycuts is TRUE in which case a numeric vector is returned}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}



\seealso{\code{\link{cut}},\code{\link{quantile}}

\code{\link{copy2clip}}, \link{twMisc}}
\examples{
set.seed(1)
x <- runif(1000, 0, 100)
z <- cutQuantiles(x, c(10,20,30))
table(z)
table(cutQuantiles(x, g=10))      # quantile groups
cutQuantiles(x, g=10, onlymeans=TRUE)	  # get only the means of each group
table(cutQuantiles(x, m=50))      # group x into intevals with at least 50 obs.
}
