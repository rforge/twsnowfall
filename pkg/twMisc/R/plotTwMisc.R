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
	## \item{ Pairs-plot with scatterplots in upper part and histogram in diagonal: this method }
	## \item{ TODO: link functions: \code{\link{twDf2wikiTable}} }
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

twPlot2DFun.contour <- function(
	### Applying FUN over grid of x and y values plot as an image.
	 ...						##<< further arguments passed to \code{\link{filled.contour}}
		### such as \code{ key.title=title(sub="Log-Like-\nlihood\n"), color.palette=function(n){rev(heat.colors(n))} }
){
	twPlot2DFun(..., contour=TRUE )
}

twPlot2DFun <- function(
	### Applying FUN over grid of x and y values plot as an image.
	x,y=NULL					##<< range of x and y ordinate see \code{\link{xy.coords}}
	,FUN="+", argsFUN=list()	##<< function and further arguments to be applied, first two arguemnts must be numeric vectors
	, xdiv=20, ydiv=xdiv		##<< scalars: number of points, set to NULL to use original x and y instead over a grid across the range
	, xlab=NULL, ylab=NULL		##<< labels, default to variable names or column names in x
	, col= heat.colors(20)
	, key.title, key.axes, axes=TRUE, las = 1
	, contour=FALSE
	, ...						##<< further arguments passed to \code{\link{filled.contour}}
### such as \code{ key.title=title(sub="Log-Like-\nlihood\n"), color.palette=function(n){rev(heat.colors(n))} }
### or for image \code{col=rev(heat.colors(20))}
){
	xy <- xy.coords(x,y)
	xr <- range(xy$x)
	yr <- range(xy$y)
	if( 0==length(xlab) ) xlab=xy$xlab
	if( 0==length(ylab) ) ylab=xy$ylab
	if( 0==length(xlab) ) xlab=deparse(substitute(x))
	if( 0==length(ylab) ) ylab=deparse(substitute(y))
	xs <- if( 0==length(xdiv)) xy$x else seq(xr[1], xr[2], length.out=xdiv)
	ys <- if( 0==length(ydiv)) xy$y else seq(yr[1], yr[2], length.out=ydiv)
	mydf <- expand.grid( xx=xs, yy=ys )
	mydf$zz <- if( is.numeric(FUN) ) as.vector(FUN) else do.call(FUN, c(list(mydf$xx,mydf$yy),argsFUN) )
	res <- matrix(mydf$zz,nrow=length(xs), dimnames={tmp<-list(xs=xs,ys=ys); names(tmp)<-c(xy$xlab,xy$ylab);tmp})
	if( contour ){
		filled.contour( xs, ys, res, xlab=xlab, ylab=ylab, ... )
	}else{
		zlim <- range(res, na.rm=TRUE)
		levels <- seq( zlim[1], zlim[2], length.out=length(col)+1 )
		
		mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
		on.exit(par(par.orig))
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
		if (!missing(key.title)) 		key.title
		mar <- mar.orig
		mar[4L] <- 1
		par(mar = mar)
		
		#image(xs,ys,res, xlab=xlab, ylab=ylab, breaks=levels, col=col )
		image(xs,ys,res, xlab=xlab, ylab=ylab, breaks=levels, col=col, ... )
		box()
	}
	res
}


