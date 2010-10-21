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
	## \item{ enhanced support for Unit-Tests: \code{\link{twUtestF}} }
	## \item{ debugging: \code{\link{traceback.curr}} }
	## \item{ working with constants: \code{\link{twEnumNames}} }
	## \item{ support for working with ODEs: \code{\link{twIntegrateTS}} }
	## \item{ Optimizing a function where first argument is an index.: \code{\link{twBinOptimize.numeric}} }
	## \item{ collection of misc : \code{\link{copy2clip}}}
	## }
	##}}
	if( 0==length(list(...)))
		seq( range[1],range[2], length.out=50 )
	else
		seq( range[1],range[2], ...)
}

copy2clip <- function(
	### copies argument to the clipboard
	x, col.names=NA, row.names=FALSE, quote=FALSE, ... 
){	
	##seealso<< \link{twMisc}

	##details<< \describe{\item{Further mics Functionality of package twMisc}{
	## \itemize{
	## \item{ easy copying to clipboard: this method }
	## \item{ Mediawiki-Code for table of given data.frame: \code{\link{twDf2wikiTable}} }
	## \item{ TODO: link functions: \code{\link{twDf2wikiTable}} }
	## }
	##}}
	
	##<<seealso \code{\link{write.table}}
	write.table(x,"clipboard",sep="\t", row.names=row.names, col.names=row.names, quote=quote, ...)
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
	### compares each column of X to column of Z and returns indices of columns   
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
		# compare each index ii of iComp with index ii+nrX1
		# vectorized version by comparing vector with last and first part removed
		ii <- which( iComp[1:(length(iComp)-nrX1)] == iComp[-(1:nrX1)] )
		iComp[ii]
	}else
		integer(0)
} 

twStripFileExt <- function(
	### Remove the all the file extension, i.e. the last dot and suceeding characters.
	filenames
){
	sub("[.][^.]*$", "", filenames, perl=TRUE)
}

twExtractFromLastDims <- function(
	### Extract slices i from array Aext keeping the all first dimensions.
	Aext		##<< array to extract from
	,i			##<< indices in matrix of last dimensions
	,dPrev=c(1)	##<< dimensions in front to keep, defaults to rows
){
	nl=prod(dim(Aext)[dPrev])
	res <- Aext[ as.numeric(t(outer((i-1)*nl,(1:nl),"+"))) ] 
	dim(res)=c(dim(Aext)[dPrev],length(i)) 
	dimnames(res) = c(dimnames(Aext)[dPrev], list(i=NULL) )
	res
}

twExtractDim <- function(
	### Extract A[...,i,...] from the iDim dimension
	A			##<< the array to extract values from
	,i=1		##<< the index to extract
	,iDim=length(dim(A))	##<< the dimension to extract from 
){
	# see http://tolstoy.newcastle.edu.au/R/help/01c/2197.html
	if( length(i) != 1 ) stop("i must be and integer of length 1")
	if( length(iDim) != 1 ) stop("iDim must be and integer of length 1")
	dims<-dim(A)
	if( iDim<0 | iDim>length(dims) ) stop("wrong dimension iDim")
	if( is.null(dims))
		return( A[i] )
	D<-length(dims)
	skipBefore <- if(iDim>1) prod(dims[1:(iDim-1)]) else 1
	#skipAfter <- if(iDim<D) prod(dims[(iDim+1):D]) else 1
	#skip<-prod(dims[seq(length=D-1)])
	slice0<- sliceJ <-(i-1)*skipBefore+(1:skipBefore)
	if( iDim<D)
		for( jDim in (iDim+1):D ){ 
			#jDim <- iDim+1
			pDim <- prod(dims[1:(jDim-1)])
			sliceJ <- as.vector( outer(sliceJ, (0:(dims[jDim]-1))*(pDim), "+" ) )
		}
	slice <- A[sliceJ]
	dim(slice)<-dims[-iDim]
	return(slice)
	### vector representing the 
} 

twListArrDim <- function(
	### Splits a dimension of an array to a list. (useful for do.call and apply)
	x						##<< the array to split
	,iDim=length(dim(x))	##<< the dimension to split along, defautls to last dimension
){
	lapply(1:(dim(x)[iDim]),function(i){ twExtractDim(x,i,iDim) })
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
){
	#sum(e^xi) = sum(e^(xi+a-a)) = sum(e^(xi-a) e^a) ) = e^a sum(e^(xi-a))
	#twUtestF(twLogSumExp)
	x <- na.omit(x)
	xmin <- min(x)
	xexp <- exp(x-xmin)
	xmin+log(sum(xexp))
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

