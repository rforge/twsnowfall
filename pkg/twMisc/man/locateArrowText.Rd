\name{locateArrowText}
\alias{locateArrowText}
\title{locateArrowText}
\description{generate Arrow and Label positions source code for \code{\link{plotArrowText}}}
\usage{locateArrowText(nlab = 1, digits = 2)}
\arguments{
  \item{nlab}{
}
  \item{digits}{
}
}



\author{Thomas Wutzler}




\examples{
plot(sin(1:10),type="l")
#copy2clip(locateArrowText(2))  # paste the result into the following command (second line) 
plotArrowText(c("Here is one label.","And another.")
    ,X=c(3.48, 7.9),Y=c(0.59, -0.45),x=c(2.53, 6.22),y=c(0.5, -0.06)    # this line is pasted from clipboard
    ,adj=c(1,0))
}
