\name{locateArrowText}
\alias{locateArrowText}
\title{locateArrowText}
\description{generate Arrow and Label positions source code for \code{\link{plotArrowText}}}
\usage{locateArrowText(nLabel = 1, digits = 2, ...)}
\arguments{
  \item{nLabel}{number of labels to generate}
  \item{digits}{number of digits to round position to}
  \item{\dots}{further arguments to \code{\link{write.table}}}
}

\value{string source code to be pasted into \code{\link{plotArrowText}} command. }

\author{Thomas Wutzler}




\examples{
isInteractiveAllowed <- FALSE
if( isInteractiveAllowed ){
    plot(sin(1:10),type="l")
    tmp <- locateArrowText(2)  # paste the result into the following command (second line) 
    plotArrowText(c("Here is one label.","And another.")
        ,X=c(3.48, 7.9),Y=c(0.59, -0.45),x=c(2.53, 6.22),y=c(0.5, -0.06)    # this line is pasted from clipboard
        ,adj=c(1,0))
}
}
