\name{dfcol}
\alias{dfcol}
\title{dfcol}
\description{extract column from a data.frame and reassing names}
\usage{dfcol(x, colName)}
\arguments{
  \item{x}{the dataFrame}
  \item{colName}{the column name}
}

\value{\code{structure( x[,colName], names=rownames(x) )}}

\author{Thomas Wutzler}



\seealso{\code{\link{seqRange}}, \link{twMisc}}
\examples{
data <- data.frame( a=1:4, b=2*(1:4) )
rownames(data) <- LETTERS[1:4]
dfcol(data,"b")
}
