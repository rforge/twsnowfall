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
