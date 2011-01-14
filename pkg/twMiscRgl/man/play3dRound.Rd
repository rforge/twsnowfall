\name{play3dRound}
\alias{play3dRound}
\title{play3dRound}
\description{Spin one round.}
\usage{
play3dRound(duration = Inf, ...)
}
\arguments{
  \item{duration}{duration of one round in seconds}
  \item{\dots}{arguments to \code{\link{view3dTiltSpin}}}
}



\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




\examples{
view3dTiltSpin()	#default is -20 degress spin
plot3d( cube3d(col="green") )
play3dRound(12)
}
