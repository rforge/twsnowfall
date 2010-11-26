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
	## \item 3D scatter and contour plots: \code{plot.twApply3DMesh} has been moved to package twMiscRgl because of package dependencies. 
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
		stop(".calcKnots: unknown method of spacing knots."))
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

