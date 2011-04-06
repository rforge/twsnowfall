\name{view3dTiltSpin}
\alias{view3dTiltSpin}
\title{view3dTiltSpin}
\description{Adjusting the rgl viewing position by first tilting to front and then spinning to right.}
\usage{
view3dTiltSpin(spin = -20, tilt = 20)
}
\arguments{
  \item{spin}{spinning angle in degree}
  \item{tilt}{tilting angle in degree}
}

\value{the tilted but not spinned userMatrix}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}




\examples{
.tmp.f <- function(){	# wrapped inside function, because it takes long 
	view3d(fov = 10, zoom = 0.8)	
	view3dTiltSpin()
	plot3d( cube3d(col="green") )
	dir <- movie3dRound("testCube")	
	#avi does not work dir <- movie3dRound("testCube",type="avi")	#doees not work	
	#dir <- movie3dRound("testCube",1/22,type="mpeg", duration=12)	
	copy2clip(dir)			# the directory where the movie was generated
}
.tmp.f.small <- function(){
	open3d(windowRect=c(0,0,200,200)+20)	# adjust window widht
	plot3d( cube3d(col="green"), axes=FALSE )
	view3dTiltSpin(spin=60)	# and adjust zoom
	copy2clip( movie3dRound("testCubeSmooth", 1/24, 6))		# 1/24 seems a real movement
}
}
