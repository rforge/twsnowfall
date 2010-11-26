.panel.hist <- function (x, ...){
	usr <- par("usr")
	on.exit(par(usr))
	par(usr = c(usr[1:2], 0, 2))
	h <- hist(x, plot = FALSE)
	breaks <- h$breaks
	nB <- length(breaks)
	y <- h$counts
	y <- y/max(y)
	rect(breaks[-nB], 0, breaks[-1], y, col = "grey")
}

.panel.cor <- function (x, y, ...){
	cr <- cor(x,y)	
	text(x = mean(range(x)), y = mean(range(y)), labels = format(cr, digits = 2), cex=0.8+1.2*abs(cr) )
}


twPairs <- function (
	### Pairs-plot with scatterplots in upper part and histogram in diagonal
	x				##<< numeric matrix
	,nsample = 200	##<< maximum number of points in scatterplot, set to NULL to show all rows
	, ...			##<< further arguments to \code{\link{pairs}}
){
	##seealso<< \link{twMisc}
	
	##details<< \describe{\item{Further plotting Functionality of package twMisc}{
	## \itemize{
	## \item Pairs-plot with scatterplots in upper part and histogram in diagonal: this method 
	## \item 2D image and contour plots: \code{\link{plot.twApply2DMesh}}
	## \item 3D scatter and contour plots: \code{\link{plot.twApply3DMesh}} 
	## \item TODO: link functions: \code{\link{twDf2wikiTable}} 
	## }
	##}}
		
	##details<<
	## based on pairs.modCost from package FME
	panel.main <- function(x, y, ...) points(x[ii], y[ii], ...)
	if (is.vector(x)) 
		x <- as.matrix(x)
	if (is.null(nsample) || (nrow(x)<nsample) ) 
		ii <- 1:nrow(x)
	else ii <- sample((1:nrow(x)), nsample)
	labels <- colnames(x)
	dots <- list(...)
	dots$diag.panel <- if (is.null(dots$diag.panel)) 
			.panel.hist
		else dots$diag.panel
	dots$lower.panel <- if (is.null(dots$lower.panel)) 
			.panel.cor
		else dots$lower.panel
	dots$upper.panel <- if (is.null(dots$upper.panel)) 
			panel.main
		else dots$upper.panel
	dots$gap <- if (is.null(dots$gap)) 
			0
		else dots$gap
	dots$labels <- if (is.null(dots$labels)) 
			labels
		else dots$labels
	do.call("pairs", c(alist(x), dots))
}

.calcKnots <- function(
	### calculate the knot positions
	x					##<< numeric vector of samples
	,nKnots				##<< number of knots
	,knotSpacing=c(					
		### method for calulating the knots 
		##describe<<
		quantile="quantile"		##<< \code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points (default) 
		,all="all"				##<< take all the provided xyz coordinates (overwrites nKnots)
		,equidistant="equidistant"	##<< cover the range of dimension i by \code{nKnots} equidistant points
		##end<<
	)	
){
	r <- range(x)
	knotSpacing <- match.arg(knotSpacing)
	res <- switch(knotSpacing,
		all = x,
		equidistant = seq(r[1], r[2], length.out=nKnots),
		quantile = cutQuantiles(x,g=nKnots,levels.mean=TRUE,onlycuts=TRUE),
		error(".calcKnots: unknown method of spacing knots."))
	### numeric vector of positions across the range of x 
}

twApply2DMesh <- function(
	### Applying FUN over cube-grid of x,y,z values
	x,y=NULL					##<< range of x and y ordinate see \code{\link{xy.coords}}
	,FUN="+", argsFUN=list()	
	,dims=20					##<< integer vector of length 1 or 3: number of points
	,knotSpacing=c(				##<< method of calculating knot positions
		##describe<<
		quantile="quantile",	##<< \code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points (default) 
		all="all",				##<< take all the provided xyz coordinates (overwrites nKnots)
		equidistant="equidistant")	##<< cover the range of dimension i by \code{dims[i]} equidistant points
		##end<<
	,...				##<< further arguments passed to FUN
){
	##seealso<< \code{\link{plot.twApply2DMesh}}
	##seealso<< \code{\link{twPairs}}, \link{twMisc}
	if( length(dims)<2 ) dims <- rep(dims[1],2)
	xy <- xy.coords(x,y)
	if( 0==length(xy$xlab) ) xy$xlab=deparse(substitute(x))
	if( 0==length(xy$ylab) ) xy$ylab=deparse(substitute(y))
	dr <- lapply(xy[1:2],range)
	grid <- lapply(1:2, function(i){ .calcKnots(xy[[i]], nKnots=dims[i], knotSpacing=knotSpacing) })
	names(grid) <- names(xy)[1:2]
	mydf <- do.call( expand.grid, grid )
	h <- do.call(FUN, c(list(mydf$x,mydf$y),argsFUN,list(...)) )
	res <- array(h, dim=dims, dimnames=grid)
	names(dimnames(res)) <- c(xy$xlab,xy$ylab)
	class(res) <- "twApply2DMesh"
	res
	### two dimensional array of class twApply2DMesh with results of calling FUN. Attribute dimnames holds the arguments.
}

setMethodS3("plot","twApply2DMesh", function( 
	### Creating an image or contour plot of a three-dimensional array.
	x							##<< object of class twApply2DMesh, a result of \code{\link{twApply2DMesh}}
	, xlab=NULL, ylab=NULL, zlab=NULL	##<< labels, default to variable names or column names in x
	, key.title, key.axes, axes=TRUE, las = 1	##<< see \code{\link{filled.contour}}
	, contour=FALSE				##<< if TRUE then \code{\link{filled.contour}} is used for plotting. Otherwisee \code{\link{image}}
	, col= rev(heat.colors(20))	##<< colors for using image
	, color.palette=function(n){rev(heat.colors(n))} ##<< colors for using filled.contour
	, ...						##<< further arguments passed to \code{\link{filled.contour}} or \code{\link{image}} 
### such as \code{ key.title=title(sub="Log-Like-\nlihood\n"), color.palette=function(n){rev(heat.colors(n))} }
### or for image \code{col=rev(heat.colors(20))}
){
	# plot.twApply2DMesh
	##seealso<< \code{\link{twPairs}}, \link{twMisc}
	dn <- sapply( dimnames(x), as.numeric )
	xy <- xy.coords(dn)
	if( 0==length(xlab) ) xlab=xy$xlab
	if( 0==length(ylab) ) ylab=xy$ylab

	par.orig <- par(c("mar", "las", "mfrow"))
	on.exit(par(par.orig))
	
	if( contour ){
		if (missing(key.title)) 
			filled.contour( xy$x, xy$y, x, xlab=xlab, ylab=ylab, key.title=title(sub=zlab,line=1), color.palette=color.palette, ... )
		else
			filled.contour( xy$x, xy$y, x, xlab=xlab, ylab=ylab, color.palette=color.palette, ... )
	}else{
		mar.orig <- par.orig$mar
		zlim <- range(x, na.rm=TRUE)
		levels <- seq( zlim[1], zlim[2], length.out=length(col)+1 )
		
		w <- (3 + mar.orig[2L]) * par("csi") * 2.54
		layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
		par(las = las)
		mar <- mar.orig
		mar[4L] <- mar[2L]
		mar[2L] <- 1
		par(mar = mar)
		plot.new()
		plot.window(xlim = c(0, 1), ylim = zlim, xaxs = "i",	yaxs = "i")
		rect(0, levels[-length(levels)], 1, levels[-1L], col = col, border=NA) #"gray80"
		if (missing(key.axes)) {
			if (axes) axis(4)
		} else key.axes
		box()
		if (!missing(key.title)) key.title else
			if( 0<length(zlab) ) title(sub=zlab, line=1)
		mar <- mar.orig
		mar[4L] <- 1
		par(mar = mar)
		
		#image(xs,ys,res, xlab=xlab, ylab=ylab, breaks=levels, col=col )
		image(xy$x, xy$y,x, xlab=xlab, ylab=ylab, breaks=levels, col=col, ... )
		box()
	}
})
attr(plot.twApply2DMesh,"ex") <- function(){
	#Example: Nested contours of mixture of three tri-variate normal densities
	nmix3 <- function(x, y, m, s) {
		0.4 * dnorm(x, m, s) * dnorm(y, m, s)  +
			0.3 * dnorm(x, -m, s) * dnorm(y, -m, s)  +
			0.3 * dnorm(x, m, s) * dnorm(y, -1.5 * m, s) 
	}
	f <- function(x,y) nmix3(x,y,.5,.5)
	
	n <- 50
	x <- rnorm(n,.5,.7)
	y <- rnorm(n,.5,.8)
	#mtrace(twApply2DMesh)
	#mtrace(twPlot2DFun)
	plot( tmp <- twApply2DMesh(x,y,f,dims=30))
	plot( tmp, contour=TRUE, zlab="density")
}

twApply3DMesh <- function(
	### Applying FUN over cube-grid of x,y,z values
	x,y=NULL,z=NULL				##<< range of x,y and z ordinate see \code{\link{xyz.coords}}
	,FUN="+", argsFUN=list()	
	,dims=5						##<< integer vector of length 1 or 3: number of points
	,knotSpacing=c(				##<< method for calulating the knots. 
		##describe<<
		quantile="quantile"		##<< \code{\link{cutQuantiles}} for midpoints of intervals holding about equal number of points (default) 
		,all="all"				##<< take all the provided xyz coordinates (overwrites nKnots)
		,equidistant="equidistant")	##<< cover the range of dimension i by \code{dims[i]} equidistant points
		##end<<
	,nSample=0			##<< number of points to sample from xyz in addition to grid
		## Results will be provided in dataframe of four coloumns with attribute "sample".
	,...				##<< further arguments passed to FUN
){
	##seealso<< \code{\link{plot.twApply3DMesh}}
	##seealso<< \code{\link{twPairs}}, \link{twMisc}
	if( length(dims)<3 ) dims <- rep(dims[1],3)
	xyz <- xyz.coords(x,y,z)
	if( 0==length(xyz$xlab) ) xyz$xlab=deparse(substitute(x))
	if( 0==length(xyz$ylab) ) xyz$ylab=deparse(substitute(y))
	if( 0==length(xyz$zlab) ) xyz$zlab=deparse(substitute(z))
	dr <- lapply(xyz[1:3],range)
	grid <- lapply(1:3, function(i){ .calcKnots(xyz[[i]], nKnots=dims[i], knotSpacing=knotSpacing) })
	names(grid) <- names(xyz)[1:3]
	mydf <- do.call( expand.grid, grid )
	h <- do.call(FUN, c(list(mydf$x,mydf$y,mydf$z),argsFUN,list(...)) )
	res <- array(h, dim=dims, dimnames=grid)
	names(dimnames(res)) <- c(xyz$xlab,xyz$ylab,xyz$zlab)
	class(res) <- "twApply3DMesh"
	# sample points
	if( 0 < nSample ){
		ds <- as.data.frame(xyz[1:3])
		dsSub <- if( nSample < nrow(ds)) ds[ sample.int(nrow(ds),nSample),] else ds
		dsSub$h <- do.call(FUN, c(list(dsSub$x,dsSub$y,dsSub$z),argsFUN,list(...)) )
		attr(res,"sample") <- dsSub
	}
	res
	### three dimensional array of class twApply3DMesh with results of calling FUN. Attribute dimnames holds the mesh of respective arguments.
}

setMethodS3("plot","twApply3DMesh", function( 
	### Creates an rgl scene with contours from calculated mesh.
	x							##<< results of \code{\link{twApply3DMesh}}
	, probs=NULL 				##<< numeric vector: levels will be calculated with quantiles of the results of the sample
	, levels=NULL				##<< numeric vector: values of FUN return values where contour surface should be drawn
	, xlab=NULL, ylab=NULL, zlab=NULL	##<< labels, default to variable names or column names in x
	, sample=attr(x,"sample")	##<< dataframe of four columns: sample points to draw
	, nDrawPoints=if( 0<length(sample) ) nrow(sample) else 200
		### number of points in xyz to draw. Set to 0 if no points should be drawn
	, col= rev(heat.colors(100))	##<< colour of the points
	, alo = 0.1, ahi = 0.5		##<< minimum and maximum transparency for contour levels
	, cmap = heat.colors
	, box = TRUE, axes = TRUE	##<< whether to draw box and axes
	, ...						##<< further arguments to \code{\link{plot3d}}
){
	# plot.twApply3DMesh
	##seealso<< \code{\link{twPairs}}, \link{twMisc}
	require(rgl)
	dn <- sapply( dimnames(x), as.numeric )
	xyz <- xyz.coords(dn)
	if( 0==length(xlab) ) xlab=xyz$xlab
	if( 0==length(ylab) ) ylab=xyz$ylab
	if( 0==length(zlab) ) zlab=xyz$zlab
	
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
	ds$h <- as.vector(x)
	
	# drawing points
	if( nDrawPoints > 0){
		##detaily<<
		## If \code{nDrawPoints > 0} and no sample was provided, 
		## then the points are sampled from mesh x.
		if( 0 == length(sample) ){
			sample <- if( (nDrawPoints < length(x))  ) 
				ds[sample.int( n=nrow(ds), size=nDrawPoints ),] else ds
		}
		plot3d(sample
			, col = col[ round(twRescale(sample[,4],c(1,length(col)))) ]
			, box = FALSE, axes = FALSE
			#, ...
		)
	}# drawing points
		
	# drawing contour surfaces
	nc <- length(levels)
	if( 0 < nc){
		require(misc3d)
		col <- rev(cmap(length(levels)))
		al <- seq(alo, ahi, len = length(levels))
		contour3d(x,levels,xyz$x,xyz$y,xyz$z,color=col,alpha=al, add=( nDrawPoints > 0 ), box = FALSE, axes = FALSE
			, ...
		)
	}
	title3d( xlab = xlab, ylab = ylab, zlab = zlab )
	if (box) 
		box3d()
	if (axes) 
		axes3d()	
	### If contourLevles was given then the 3D array of function evaluations 
})
attr(twApply3DMesh,"ex") <- function(){
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
	
	#mtrace(twApply3DMesh)
	par3d(r3dDefaults[-match(c("bg","material"),names(r3dDefaults))])
	
	plot(tmp <- twApply3DMesh(x,y,zz,f, nSample=200, dims=10))	# just the points
	#mtrace(plot.twApply3DMesh)
	plot( tmp, col=rev(heat.colors(22))[-(1:5)]) # avoiding near white colors
	plot( tmp, levels=seq(0.05, 0.14, len=3) )	# specifying contour levels directly at function value scale 
	plot( tmp, probs=seq(0.5, 0.95, len=4), nDrawPoints=0)		# specifying quantiles of FUN results
}
.tmp.f <- function(){
	# generate a movie
	# make sure command convert from ImageMagick can be found
	open3d(windowRect=c(0,0,200,200)+20)	# make a bigger window
	plot3d( cube3d(col="green") )
	# remember that z axis goes into depth of the screen with rotationMatrix and to top with rotate3d
	#par3d(userMatrix = rotationMatrix(-90*pi/180, 1,0,0))	# rotate so that z points up
	view3d(fov = 10, zoom = 0.8)	
	M <- rotationMatrix(-70*pi/180, 1,0,0)	# looking a bit from above
	par3d(userMatrix = rotate3d(M, -20*pi/180, 0, 0, 1) )	# spinning a bit to the left
	#play3d(spin3d(rpm=5), duration=60/5)	#full round
	#movie3d(spin3d(rpm=5), duration=60/5, movie = "triVariateNormal")	#full round in 12 seconds
}


