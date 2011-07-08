view3dTiltSpin <- function(
	### Adjusting the rgl viewing position by first tilting to front and then spinning to right.
	spin=-20	##<< spinning angle in degree
	,tilt=20	##<< tilting angle in degree
){
	M <- rotationMatrix((-90+tilt)*pi/180, 1,0,0)	# looking a bit from above
	par3d(userMatrix = rotate3d(M, spin*pi/180, 0, 0, 1) )	# spinning a bit to the left
	return(invisible(M))
	### the tilted but not spinned userMatrix
}
attr(view3dTiltSpin,"ex") <- function(){
	plot3d( cube3d(col="green") )
	view3dTiltSpin()	#default is -20 degress spin
	view3dTiltSpin(70)	#spinning70 degress
	
}

play3dRound <- function(
	### Spin one round.
	duration= Inf	##<< duration of one round in seconds
	,rpm=if( is.infinite(duration)) 60/secRound else 60/duration	##<< rotations per minute
	,...			##<< arguments to \code{\link{view3dTiltSpin}}
){
	play3d( spin3d(rpm=rpm), duration=duration )
}
attr(play3dRound,"ex") <- function(){
	view3dTiltSpin()	#default is -20 degress spin
	plot3d( cube3d(col="green") )
	play3dRound(12)
}


movie3dRound <- function(
	### Generating a movie of a full round
	movie="movie"
	,frameTime=1 #0.75	##<< number of seconds for displying a frame
	,duration=16		##<< number of seconds for the full rotation
	,dir = tempdir()		##<< A directory in which to create temporary files for each frame of the movie
	,convert="convert -delay 1x%f %s*.png %s.%s" ##see \code{\link{movie3d}}
	,top=TRUE		##<< bring rgl window to top before creating snapshots
	,...			##<< further arguments to \code{\link{movie3d}}
){
	##details<<
	## The default parameterization provides 16 views, in analogy of the clock
	
	##details<<
	## In contrast to play3dRound this methods does not set standard position. 
	## See examples how to do this.
	if( top ) rgl.bringtotop()
	movie3d(spin3d(rpm=60/duration), fps=1/frameTime, duration=duration, movie = movie, dir=dir, convert=convert, top=FALSE)	#full round in 16 seconds
	dir
	### The directory where the movie was generated 	
}
attr(view3dTiltSpin,"ex") <- function(){
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
