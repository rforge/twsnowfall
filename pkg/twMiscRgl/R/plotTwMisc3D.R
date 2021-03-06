twApply3DMesh <- function(
	### Applying FUN over cube-grid of x,y,z values
	x,y=NULL,z=NULL				##<< range of x,y and z ordinate see \code{\link{xyz.coords}}
	,FUN="+", argsFUN=list()	
	,dims=5						##<< integer vector of length 1 or 3: number of points
	,knotSpacing=c(				##<< method for calulating the knots 
		##describe<<
		quantile="quantile"			##<< \code{\link{cutQuantiles}} for breaks of intervals holding about equal number of points, includes edges (default) 
		,midquantile="midquantile"	##<< \code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points, by excluding the edges the sample is represented better  
		,all="all"					##<< take all the provided xyz coordinates (overwrites nKnots)
		,equidistant="equidistant")	##<< cover the range of dimension i by \code{nKnots} equidistant points
		##end<<
	,nSample=0			##<< number of points to sample from xyz in addition to grid.
	    ## Results will be provided in dataframe of four coloumns with list entry "sample".
	,label=deparse(substitute(FUN))	##<< label of the z-variable, stored in attribute label
	,...				##<< further arguments passed to FUN
){
	##seealso<< \code{\link{plot.twApply3DMesh}}
	##seealso<< \code{\link{twPairs}}, \link{twMisc}

	##details<< 
	## note that knotSpacing default is "quantile", which differs from the 2D version
	## the edges are not representative of the sample, but the grid spans the full space of the sample
	
	if( length(dims)<3 ) dims <- rep(dims[1],3)
	xyz <- xyz.coords(x,y,z)
	if( 0==length(xyz$xlab) ) xyz$xlab=deparse(substitute(x))
	if( 0==length(xyz$ylab) ) xyz$ylab=deparse(substitute(y))
	if( 0==length(xyz$zlab) ) xyz$zlab=deparse(substitute(z))
	dr <- lapply(xyz[1:3],range)
	#i<-1;mtrace(.calcKnots);.calcKnots(xyz[[i]], nKnots=dims[i], knotSpacing=knotSpacing)
	fCalcKnots <- if( exists(".calcKnots")) .calcKnots else twMisc:::.calcKnots
	grid <- lapply(1:3, function(i){ fCalcKnots(xyz[[i]], nKnots=dims[i], knotSpacing=knotSpacing) })
	names(grid) <- names(xyz)[1:3]
	mydf <- do.call( expand.grid, grid )
	h <- do.call(FUN, c(list(mydf$x,mydf$y,mydf$z),argsFUN,list(...)) )
	res <- array(h, dim=dims, dimnames=list(x=NULL,y=NULL,z=NULL))
	names(dimnames(res)) <- names(grid) <- c(xyz$xlab,xyz$ylab,xyz$zlab)
	# sample points
	if( 0 < nSample ){
		ds <- as.data.frame(xyz[1:3])
		dsSub <- if( nSample < nrow(ds)) ds[ sample.int(nrow(ds),nSample),] else ds
		dsSub$h <- do.call(FUN, c(list(dsSub$x,dsSub$y,dsSub$z),argsFUN,list(...)) )
		names(dsSub) <- c(names(dimnames(res)), label)
	}
	resList <- list( mesh=do.call(cbind, grid), fval=res, sample=dsSub, label=label, rangeOrig=lapply(xyz[1:3],range))
	class(resList) <- "twApply3DMesh"
	resList
	### list of class twApply2DMesh with itmes \itemize{
	### \item mesh:matrix with each row one coordinate and two columns corresponding to x and y 
	### \item fval: the two dimensional array of evaluated function values
	### \item sample: data frame with four columns corresponding to points additional to the grid
	### \item label: argument label describing fval
	### \item rangeOrig: list with items x, and y with the range of the original sample
	### }
}



R.methodsS3::setMethodS3("plot","twApply3DMesh", function( 
		### Creates an rgl scene with contours from calculated mesh.
		x							##<< results of \code{\link{twApply3DMesh}}
		, probs=NULL 				##<< numeric vector: levels will be calculated with quantiles of the results of the sample
		, levels=NULL				##<< numeric vector: values of FUN return values where contour surface should be drawn
		, xlab=NULL, ylab=NULL, zlab=NULL	##<< labels, default to variable names or column names in x
		, sample=x$sample			##<< dataframe of four columns: sample points to draw
		, nDrawPoints=if( 0<length(sample) ) nrow(sample) else 200
		### number of points in xyz to draw. Set to 0 if no points should be drawn
		, col= rev(heat.colors(100))	##<< colour of the points
		, alo = 0.1, ahi = 0.5		##<< minimum and maximum transparency for contour levels
		, cmap = heat.colors
		, box = TRUE, axes = TRUE	##<< whether to draw box and axes
		,xlim=NULL,ylim=NULL,zlim=NULL		
		, ...						##<< further arguments to \code{\link{plot3d}}
	){
		# plot.twApply3DMesh
		
		##alias<< twMiscRgl
		
		##details<< \describe{\item{Further plotting Functionality of package twMiscRgl}{
		## \itemize{
		## \item 3D scatter and contour plots: this method 
		## }
		##}}
		
		xyz <- xyz.coords(x$mesh)
		if( 0==length(xlab) ) xlab=xyz$xlab
		if( 0==length(ylab) ) ylab=xyz$ylab
		if( 0==length(zlab) ) zlab=xyz$zlab
		
		if( 0==length(xlim)) xlim=x$rangeOrig$x
		if( 0==length(ylim)) ylim=x$rangeOrig$y
		if( 0==length(zlim)) zlim=x$rangeOrig$z
		
		##details<<
		## If argument \code{level} is supplied, the argument \code{probs} is ignored. 
		## Together with a small \code{nDrawPoints} this saves calculation time.
		## If a sample was provided the quantiles of surface levels are calculated from
		## the sample, otherwise from the mesh x.
		boCalcLevelsFromQuantiles = (0<length(probs)) & (0==length(levels))
		if( boCalcLevelsFromQuantiles )
			if( 0 == length(sample) )
				levels <- quantile(as.vector(x),probs=probs)
			else
				levels <- quantile(as.vector(sample[,4]),probs=probs)
		
		ds <- expand.grid(xyz[1:3])
		ds$h <- as.vector(x$fval)
		
		# drawing points
		if( nDrawPoints > 0){
			##detaily<<
			## If \code{nDrawPoints > 0} and no sample was provided, 
			## then the points are sampled from mesh x.
			if( 0 == length(sample) ) sample <- ds
			drawSample <- if( nrow(sample) > nDrawPoints)
				sample[sample.int( n=nrow(ds), size=nDrawPoints ),] else sample
			plot3d(drawSample
				, col = col[ round(twRescale(drawSample[,4],c(1,length(col)))) ]
				, box = FALSE, axes = FALSE
				, xlim=xlim, ylim=ylim, zlim=zlim
				, ...
			)
		}# drawing points
		
		# drawing contour surfaces
		nc <- length(levels)
		if( 0 < nc){
			col <- rev(cmap(length(levels)))
			al <- seq(alo, ahi, len = length(levels))
			misc3d::contour3d(x$fval,levels,xyz$x,xyz$y,xyz$z,color=col,alpha=al, add=( nDrawPoints > 0 ), box = FALSE, axes = FALSE
				, ...
			)
		}
		decorate3d( xlim,ylim,zlim,xlab = xlab, ylab = ylab, zlab = zlab, box=box, axes=axes )
		### If contourLevles was given then the 3D array of function evaluations 
	})
attr(plot.twApply3DMesh,"ex") <- function(){
	#Example: Nested contours of mixture of three tri-variate normal densities
	nmix3 <- function(x, y, z, m, s) {
		0.4 * dnorm(x, m, s) * dnorm(y, m, s) * dnorm(z, m, s) +
			0.3 * dnorm(x, -m, s) * dnorm(y, -m, s) * dnorm(z, -m, s) +
			0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) * dnorm(z, m, s)
	}
	f <- function(x,y,z) nmix3(x,y,z,.5,.5)
	
	n <- 250
	x <- rnorm(n,.5,.5)
	y <- c(rnorm(n/2,.5,.5), rnorm(n/2,-.5,.5)) 
	zz <- rnorm(n,.5,.5)
	
	plot(tmp <- twApply3DMesh(x,y,zz,f, nSample=200, dims=10))	# just the points
	#mtrace(plot.twApply3DMesh)
	plot( tmp, col=rev(heat.colors(22))[-(1:5)]) # avoiding near white colors
	plot( tmp, levels=seq(0.05, 0.14, len=3) )	# specifying contour levels directly at function value scale 
	plot( tmp, probs=seq(0.5, 0.95, len=4), nDrawPoints=0)		# specifying quantiles of FUN results
}

.tmp.f <- function(){
	# generate a movie
	# make sure command convert from ImageMagick can be found
	open3d(windowRect=c(0,0,400,400)+20)	# adjust window widht
	plot3d( cube3d(col="green") )
	# remember that z axis goes into depth of the screen with rotationMatrix and to top with rotate3d
	#par3d(userMatrix = rotationMatrix(-90*pi/180, 1,0,0))	# rotate so that z points up
	view3d(fov = 10, zoom = 0.8)	
	M <- rotationMatrix(-70*pi/180, 1,0,0)	# looking a bit from above
	par3d(userMatrix = rotate3d(M, -20*pi/180, 0, 0, 1) )	# spinning a bit to the left
	#play3d(spin3d(rpm=5), duration=60/5)	#full round
	#movie3d(spin3d(rpm=5), duration=60/5, movie = "triVariateNormal")	#full round in 12 seconds
}




