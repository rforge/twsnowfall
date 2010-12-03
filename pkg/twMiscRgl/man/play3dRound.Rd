\name{play3dRound}
\alias{play3dRound}
\title{play3dRound}
\description{Set Standard position and spin one round.}
\usage{
play3dRound(duration = Inf, ..., fov = 10, zoom = 0.8)
}
\arguments{
  \item{duration}{duration of one round in seconds}
  \item{\dots}{arguments to \code{\link{view3dTiltSpin}}}
  \item{fov}{see  \code{\link{view3d}}}
  \item{zoom}{
}
}



\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




\examples{
plot3d( cube3d(col="green") )
play3dRound(12)
}
