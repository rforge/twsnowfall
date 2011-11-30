# misc.R
# 
# miscelancelous functions and plotting style lists
#
# Author: twutz
###############################################################################

#plotting styles by different time series of experiments

seqRange <- function(
	### Create a sequence based on a range c(min,max)
	range	##<< the range for the sequence
	,...	##<< further arguments to seq, defaults to length.out=50
){
	##alias<< twMisc
	
	##details<< \describe{\item{Functionality of package twMisc}{
	## \itemize{
	## \item enhanced support for Unit-Tests: \code{\link{twUtestF}} 
	## \item debugging: \code{\link{traceback.curr}} 
	## \item working with constants: \code{\link{twEnumNames}} 
	## \item support for working with ODEs: \code{\link{twIntegrateTS}} 
	## \item Optimizing a function where first argument is an index.: \code{\link{twBinOptimize.numeric}} 
	## \item plotting routines: \code{\link{twPairs}}
	## \item matching by closest value: \code{\link{matchClosest}}
	## \item dealing with summer/winter time: \code{\link{strptimeNoSummer}}
	## \item collection of misc: \code{\link{copy2clip}}
	## \item extracting from arrays: \code{\link{twExtractDim}}
	## \item updating the version and the date of the description file: \code{\link{updateVersionAndDate}}
	## }
	##}}
	if( 0==length(list(...)))
		seq( range[1],range[2], length.out=50 )
	else
		seq( range[1],range[2], ...)
}



copy2clip <- function(
	### copies argument to the clipboard
	x, col.names=NA, row.names=FALSE, quote=FALSE, sep="\t", ... 
){	
	##seealso<< \link{twMisc}

	##details<< \describe{\item{Further misc functionality of package twMisc}{
	## \itemize{
	## \item{ easy copying to clipboard: this method }
	## \item{ Mediawiki-Code for table of given data.frame: \code{\link{twDf2wikiTable}} }
	## \item{ assign first variable of RData-file into a variable: \code{\link{loadAssign}} }
	## \item{ reorder factor levels: \code{\link{reorderFactor}} }
	## \item{ format to significant number of digits including trailing zeros: \code{\link{formatSig}} }
	## \item{ TODO: link functions: \code{\link{twDf2wikiTable}} }
	## }
	##}}
	
	##<<seealso \code{\link{write.table}}
	write.table(x,"clipboard",sep=sep, row.names=row.names, col.names=row.names, quote=quote, ...)
}  



library(grDevices)
fColConv <- function(
		### color encoding to avoid symbol font, which gives problems in pdf-Output   
	cols
		###  list of colors
){
	###details<< applying fColConv causes problems in emf output
	if( is.list(cols))
		lapply( cols, function(cols){ rgb(t(col2rgb((cols)))/255, alpha=0.99)} )
	else
		rgb(t(col2rgb((cols)))/255, alpha=0.99)
}

### A list of colours corresponding to Excel, that are well distinguishable
twXlscol <- fColConv( list( blue="#000080", red="#800000", green="#008000", violet="#660066", brown="#666600", turquise="#006666", blueviolet="#330099", redviolet="#990033", greenbrown="#339900", redbrown="#993300", blueturquise="#003399", greenturquise="#009933" ))

### 10 Colours from Excel2007 are quite colour-blind safe
twXlscol07 <- list( blue="#4572A7", red="#AA4643", green="#89A54E", violet="#71588F"
	, seablue="#4198AF", orange="#DB843D", lightblue="#93A9CF", pink="#D19392"
	, lightgreen="#B9CD96", lightviolet="#A99BBD" )
#barplot(structure(rep(1,10),names=names(twXlscol07)),col=unlist(twXlscol07), horiz=TRUE, las=1 )
#twBlindCol <- list(blue="#0072B2",orange="#D55E00",yellow="#F0E442",lightblue="#56B4E9",lightorange="#E69F00"
	#,pink="#CC79A7",green="#2B9F78",black="#000000")
#barplot(structure(rep(1,8),names=names(twBlindCol)),col=unlist(twBlindCol), horiz=TRUE, las=1 )

### A list of line types, that are well distinguishable.
twLtys <- c("22", "44", "13", "1343", "73", "2262", "12223242", "F282", "F4448444", "224282F2", "F1")
	
.tmp.f <- function(
	### dummy, holds code that is not to be executed
){
	install.packages("R.utils") #GString
	install.packages("R.oo")
	install.packages("debug")
	install.packages("deSolve")
	install.packages("nlme")
	install.packages("snowfall") #parallel
	install.packages("sensitivity")
	install.packages("mnormt")
	install.packages("mvtnorm")
	install.packages("abind")
	install.packages("coda")
	install.packages("abind")
	install.packages("inlinedocs",repos="http://r-forge.r-project.org")
	install.packages("RUnit")
	install.packages("ggplot2")
	library(inlinedocs)			#package.skeleton.dx
	library(debug)
}

.tmp.f <- function(
	### interactive development code
){
	sfInit( parallel=TRUE, cpus=4)
	sfStop()
}



capitalize <- function(
		### "Mixed Case" Capitalizing
	x
		### The string to capitalize
){
	##<<details 
	## toupper( every first letter of a word )
	## useful for generating graph texts
	#lcase, ucase
	if( length(x) > 1)
		sapply(x, capitalize)
	else{
		s <- strsplit(x, " ")[[1]]
		paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
	}
}

#within function does not exist at the cluster, so specifiy it
if( !exists("within")){
	within <- function (data, expr, ...) UseMethod("within")
	within.list <- function (data, expr, ...){
		parent <- parent.frame()
		e <- evalq(environment(), data, parent)
		eval(substitute(expr), e)
		l <- as.list(e)
		l <- l[!sapply(l, is.null)]
		nD <- length(del <- setdiff(names(data), (nl <- names(l))))
		data[nl] <- l
		if (nD) 
			data[del] <- if (nD == 1) 
						NULL
					else vector("list", nD)
		data
	}
}

evalCommandArgs <- function(
	### evaluate args passed to a batch script
){
	##<<details R CMD BATCH --vanilla '--args i=1 n=8' testCommandArgs.R testCommandArgs.Rout
	args=(commandArgs(TRUE))
	for(i in seq( along.with=args)){
		eval.parent(parse(text=args[[i]]))
	}
}



# code that needs to be moved to other packages
# keep it temporarily, so that not need to build other packages


twWhichColsEqual <- function(
	### compares each column of X to column of Z and returns indices of equal columns   
	X,	##<< matrix
	Z=matrix(rep(z,ncol(X)),nrow=length(z)),	
	### Matrix with same numer of rows. 
	z=NA
### alternatively, specify only one vector, which each row of X is compared against
){
	
	# column indices of all single components
	##details<< 
	## If both vectors contain NA's at the same position the vectors are regarded equal.
	## This is different from which with ==, where any NA leeds to a FALSE 
	iComp <- which(X == Z | (is.na(X) & is.na(Z)), arr.ind=TRUE )[,2]
	# when all components in a row are the same, they will occure nrow(X) times in iComp
	nrX1 <- nrow(X)-1
	if( length(iComp) > nrX1){
		if( nrX1 != 0){
			# compare each index ii of iComp with index ii+nrX1
			# vectorized version by comparing vector with last and first part removed
			ii <- which( iComp[1:(length(iComp)-nrX1)] == iComp[-(1:nrX1)] )
			iComp[ii]
		}else iComp	# if there is just one row iComp is already the index
	}else
		integer(0)
} 

twStripFileExt <- function(
	### Remove the all the file extension, i.e. the last dot and suceeding characters.
	filenames
){
	sub("[.][^.]*$", "", filenames, perl=TRUE)
}





twLastN1 <- function(
	### last n components of vector x
	x		##<< vector
	,n=1	##<< number of components from the end
){
	if( !is.finite(n) || (n<=0) ) return( x[FALSE] )
	if( n>=length(x)) return(x)
	x[ length(x)+1-(n:1) ]
}

twLastN21 <- function(
	### last n rows of matrix x
	x		##<< matrix
	,n=1	##<< number of components from the end
){
	if( !is.finite(n) || (n<=0) ) return( x[FALSE,,drop=FALSE] )
	if( n>=nrow(x)) return(x)
	x[ nrow(x)+1-(n:1), ]
}

twLastN22 <- function(
	### last n columns of matrix x
	x		##<< matrix
	,n=1	##<< number of components from the end
){
	if( !is.finite(n) || (n<=0) ) return( x[,FALSE,drop=FALSE] )
	if( n>=ncol(x)) return(x)
	x[ ,ncol(x)+1-(n:1) ]
}

twLogSumExp <- function(
	### calculates the log(sum(exp(x))) in numerically safer way
	x	##<< vector to be summed
	,shiftUpperBound=FALSE ##<< use this if x has a clear upper bound
	,shiftLowerBound=FALSE ##<< use this if x has a clear lower bound
){
	#sum(e^xi) = sum(e^(xi+a-a)) = sum(e^(xi-a) e^a) ) = e^a sum(e^(xi-a))
	#twUtestF(twLogSumExp)
	##details<< 
	## Before taking the exponent, all x are shifted towards zero.
	## By default the median of x is subtracted. 
	## If shiftUpperBound then max(x) is subtracted.
	## This is useful if the distribution of x has a strong left tail but a defined upper bound.
	## If shiftLowerBound then min(x) is subtracted.
	## This is useful if the distribution of x has a strong right tail but a defined lower bound.
	x <- na.omit(x)
	xShift <- if( shiftUpperBound ) max(x) else 	if( shiftLowerBound ) min(x) else median(x)
	xexp <- exp(x-xShift)
	xShift + log(sum(xexp))
}

twLogMeanExp <- function(
	### calculates the log(mean(exp(x))) in numerically safer way
	x	##<< vector for whose mean is to be caluclated
){
	#log(sum / n)
	twLogSumExp(x) - log(length(x))
}

twCloseDevs <- function(
	### Closes all windows with a device number unless those specified with parameter omit.
	omit=c()	### list of Devices not to close.
){
	for( dev in dev.list()[ !(dev.list() %in% omit)] ) 
		dev.off(dev)
}

twDf2wikiTable <- function(
	### Mediawiki-Code for table of given data.frame. 
	ds		##<< data.frame
	,tableProps='style="float: right;"'	##<< additional text inlcuded in the header
){
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
	
	rows <- paste(by(ds, 1:nrow(ds), function(row){ paste("|-\n|",paste(row[1,],collapse=" || ")) }),collapse="\n")
	heading <-  paste("!",paste(colnames(ds),collapse=" !! "))
	tableProps <- 'style="float: right;"'
	#library(R.utils)
	#tmp <- as.character(GString('{| class="wikitable" frame="hsides" ${tableProps} \n${heading}\n${rows}\n|}'))
	tmp <- paste('{| class="wikitable" frame="hsides" ',tableProps,' \n',heading,'\n',rows,'\n|}',sep="")
	copy2clip(tmp)
	tmp
	### String, Side-effect: copied to clipboard
}

cutQuantiles <- function (
	### Cut a Numeric Variable into Intervals of about same number of observations.
	x			##<< numeric vector to classify into intervals 
	, cuts		##<< cut points 
	, m = 150	##<< desired minimum number of observations in a group 
	, g			##<< number of quantile groups 
	, levels.mean = FALSE	##<< set to TRUE to make the new categorical vector have levels attribute that is the group means of x instead of interval endpoint labels 
	, digits	##<< number of significant digits to use in constructing levels. Default is 3 (5 if levels.mean=TRUE) 
	, minmax = TRUE	##<< if cuts is specified but min(x)<min(cuts) or max(x)>max(cuts), augments cuts to include min and max x
	, oneval = TRUE	##<< if an interval contains only one unique value, the interval will be labeled with the formatted version of that value instead of the interval endpoints, unless oneval=FALSE
	, onlycuts = FALSE	##<< set to TRUE to only return the vector of computed cuts. This consists of the interior values plus outer ranges. 
	, onlymeans = FALSE	##<< set to TRUE to only return the means of x within each group 
){
	##details<< 
	## copied from Hmisc:cutQuantiles to reduce package dependencies.
	
	##seealso<< \code{\link{cut}},\code{\link{quantile}}
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
	
	method <- 1
	x.unique <- sort(unique(c(x[!is.na(x)], if (!missing(cuts)) cuts)))
	min.dif <- min(diff(x.unique))/2
	min.dif.factor <- 1
	if (missing(digits)) 
		digits <- if (levels.mean) 
				5
			else 3
	oldopt <- options(digits = digits)
	on.exit(options(oldopt))
	xlab <- attr(x, "label")
	if (missing(cuts)) {
		nnm <- sum(!is.na(x))
		if (missing(g)) 
			g <- max(1, floor(nnm/m))
		if (g < 1) 
			stop("g must be >=1, m must be positive")
		options(digits = 15)
		n <- table(x)
		xx <- as.double(names(n))
		options(digits = digits)
		cum <- cumsum(n)
		m <- length(xx)
		y <- as.integer(ifelse(is.na(x), NA, 1))
		labs <- character(g)
		cuts <- approx(cum, xx, xout = (1:g) * nnm/g, method = "constant", 
			rule = 2, f = 1)$y
		cuts[length(cuts)] <- max(xx)
		lower <- xx[1]
		upper <- 1e+45
		up <- low <- double(g)
		i <- 0
		for (j in 1:g) {
			cj <- if (method == 1 || j == 1) 
					cuts[j]
				else {
					if (i == 0) 
						stop("program logic error")
					s <- if (is.na(lower)) 
							FALSE
						else xx >= lower
					cum.used <- if (all(s)) 
							0
						else max(cum[!s])
					if (j == m) 
						max(xx)
					else if (sum(s) < 2) 
						max(xx)
					else approx(cum[s] - cum.used, xx[s], xout = (nnm - 
									cum.used)/(g - j + 1), method = "constant", 
							rule = 2, f = 1)$y
				}
			if (cj == upper) 
				next
			i <- i + 1
			upper <- cj
			y[x >= (lower - min.dif.factor * min.dif)] <- i
			low[i] <- lower
			lower <- if (j == g) 
					upper
				else min(xx[xx > upper])
			if (is.na(lower)) 
				lower <- upper
			up[i] <- lower
		}
		low <- low[1:i]
		up <- up[1:i]
		variation <- logical(i)
		for (ii in 1:i) {
			r <- range(x[y == ii], na.rm = TRUE)
			variation[ii] <- diff(r) > 0
		}
		if (onlycuts) 
			return(unique(c(low, max(xx))))
		if( onlymeans){
			return( as.vector(tapply(x, y, function(w) mean(w, na.rm = TRUE))) )
		}
		flow <- format(low)
		fup <- format(up)
		bb <- c(rep(")", i - 1), "]")
		labs <- ifelse(low == up | (oneval & !variation), flow, 
			paste("[", flow, ",", fup, bb, sep = ""))
		ss <- y == 0 & !is.na(y)
		if (any(ss)) 
			stop(paste("categorization error in cutQuantiles.  Values of x not appearing in any interval:\n", 
					paste(format(x[ss], digits = 12), collapse = " "), 
					"\nLower endpoints:", paste(format(low, digits = 12), 
						collapse = " "), "\nUpper endpoints:", paste(format(up, 
							digits = 12), collapse = " ")))
		y <- structure(y, class = "factor", levels = labs)
	}
	else {
		if (minmax) {
			r <- range(x, na.rm = TRUE)
			if (r[1] < cuts[1]) 
				cuts <- c(r[1], cuts)
			if (r[2] > max(cuts)) 
				cuts <- c(cuts, r[2])
		}
		l <- length(cuts)
		k2 <- cuts - min.dif
		k2[l] <- cuts[l]
		y <- cut(x, k2)
		if (!levels.mean) {
			brack <- rep(")", l - 1)
			brack[l - 1] <- "]"
			fmt <- format(cuts)
			labs <- paste("[", fmt[1:(l - 1)], ",", fmt[2:l], 
				brack, sep = "")
			if (oneval) {
				nu <- table(cut(x.unique, k2))
				if (length(nu) != length(levels(y))) 
					stop("program logic error")
				levels(y) <- ifelse(nu == 1, c(fmt[1:(l - 2)], 
						fmt[l]), labs)
			}
			else levels(y) <- labs
		}
	}
	if (levels.mean) {
		means <- tapply(x, y, function(w) mean(w, na.rm = TRUE))
		levels(y) <- format(means)
	}
	attr(y, "class") <- "factor"
	if (length(xlab)) 
		attr(y, "label") <- xlab #label(y) <- xlab
	y
	### a factor variable with levels of the form [a,b) or formatted means (character strings) unless onlycuts is TRUE in which case a numeric vector is returned
}
attr(cutQuantiles,"ex") <- function(){
	set.seed(1)
	x <- runif(1000, 0, 100)
	z <- cutQuantiles(x, c(10,20,30))
	table(z)
	table(cutQuantiles(x, g=10))      # quantile groups
	cutQuantiles(x, g=10, onlymeans=TRUE)	  # get only the means of each group
	table(cutQuantiles(x, m=50))      # group x into intevals with at least 50 obs.
}

.inside <- function (x, interval){	x >= interval[1] & x <= interval[2] }

twRescale <- function (
	### Rescale numeric vector to have specified minimum and maximum. 
	x,								##<< data to rescale
	to = c(0, 1),					##<< range to scale to
	from = range(x, na.rm = TRUE),	##<< range to scale from, defaults to range of data
	clip = TRUE						##<< should values be clipped to specified range?
){
	##details<<
	## copied from package ggplot2 to avoid package redundancies
	##author<< Hadley Wickham <h.wickham@gmail.com>
	if (length(from) == 1 || length(to) == 1 || from[1] == from[2] || 
		to[1] == to[2]) 
		return(x)
	if (is.factor(x)) {
		warning("Categorical variable automatically converted to continuous", 
			call. = FALSE)
		x <- as.numeric(x)
	}
	scaled <- (x - from[1])/diff(from) * diff(to) + to[1]
	if (clip) {
		ifelse(!is.finite(scaled) | .inside(scaled,to), scaled, 
			NA)
	}
	else {
		scaled
	}
}

loadAssign <- function(
	### Load a RData file and return the value of the first entry
	...	##<< arguments to \code{\link{load}}
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	##details<<
	## The load function is evaluated in a local environment.
	## Then the value of the first entry of ls in that environment is returned.
	local({load(...); get(ls()[1])})
	### Value of the first variable the loaded file
	
}
attr(loadAssign,"ex") <- function(){
	# save the filename character into a temporary file 
	fout <- fout2 <- file.path(tempdir(),"tmp.RData")
	save(fout,file=fout)
	fout <- "changed"
	(x <- loadAssign(file=fout2))	
	fout				# note that is has not been overwritten with load
}

reorderFactor <- function(
	### reorder the factor levels in a factor
	x			##<< factor to reorder
	, newLevels	##<< character vector: levels in new order
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	levelMap <- match(levels(x), as.factor(newLevels))
	factor( newLevels[levelMap[x]], level=newLevels)	
	### factor with levels corresponding to newLevels
}
attr(reorderFactor,"ex") <- function(){
	x <- as.factor(sample(c("low","medium","high"),10,replace=TRUE) )
	x
	as.integer(x)
	y <- reorderFactor(x,c("low","medium","high"))
	y
	as.integer(y)
	all(x == y)
}

cooksDistance.loess <- function(
	### calculate cooksDistance for loess models
	formula		##<< parameters for \code{\link{loess}}
	,data
	,...
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	mf <- model.frame(formula,data, na.action=NULL)
	i <- which( !apply(mf, 1, function(row) any(is.na(row)) ))
	mfComplete <- na.omit(mf) 
	x <- loess(formula, mfComplete,...)
	nP <- x$enp
	mse <- x$s
	denom <- nP*mse
	#iOut <- 1
	y <- fitted(x)
	cdist <- rep( NA, nrow(data) )
	cdist[i] <- sapply( 1:nrow(mfComplete), function(iOut){
			yi <- fitted(loess( formula, mfComplete[-iOut,], ...))
			sum( (y[-iOut] - yi)^2 ) / denom
		})
	##value<< list with elements
	list( mod=x			##<< the fitted loess model
		, cdist=cdist)		##<< numeric vector of cooks distance
	##end<<
}
attr(cooksDistance.loess,"ex") <- function(){
	tmp <- seq(1,350,length.out=9)
	nPar <-  
	data <- data.frame( 
		time = tmp
		,fluo = log(tmp) + rnorm(length(tmp), sd=max(log(tmp)/5))
	)
	plot(fluo ~ time, data)
	res <- cooksDistance.loess( fluo ~ time, data, span=0.9)
	plot( res ~ time, dss)
}


formatSig <- function(
	### format real values to significant number of digits
	x, digits=3
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	#l10x <- log10(x)
	ifelse( x < 10^(digits-1) #l10x < digits-1
		, formatC(x,digits=digits,format="fg",flag="#")
		, as.character( signif(x,digits) ) 
	)
	### character vector with number rounded to significant digits and output including trailing zeros
}
attr(formatSig,"ex") <- function(){
	x <- c(0.0654,0.06,6,65,99.1,100,100.8,125,1024,2000)
	formatSig(x,3)
	formatSig(x,2)
}

