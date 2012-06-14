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




twWin <- function(
	### creates a new window with default size and par
	width=4.6,height=3.2,pointsize=10, record=TRUE
	,xpos=40,ypos=-80
	,...
){
	cdev = dev.cur()
	windows(width=width,height=height,pointsize=pointsize, record=record, xpos=xpos, ypos=ypos, ...)	# nice to get 4 plot in powerpoint screen
	par( las=1 )					#also y axis labels horizontal
	par(mar=c(2.0,3.3,0,0)+0.3 )  #margins
	par(tck=0.02 )				#axe-tick length inside plots             
	par(mgp=c(1.1,0.2,0) )  #positioning of axis title, axis labels, axis
	#par( mfrow=c(1,2) )	#tow plots in window
	### the device before opening the new window
	cdev
}

str3 <- function(
	### Compactly Display the Structure with 3 nesting levels and without attributes
	object	##<< the object to display
	,...	##<< further arguments to str		
	##seealso<< \code{\link{copy2clip}}, \link{twMisc}
){
	str( object, max.level=3,  give.attr=FALSE, ...)
	### result of \code{\link{str}( object, max.level=3,  give.attr=FALSE, ...)}
}

copy2clip <- function(
	### copies argument to the clipboard
	x, col.names=NA, row.names=FALSE, quote=FALSE, sep="\t", ... 
){	
	##seealso<< \link{twMisc}
	
	##details<< \describe{\item{Further misc functionality of package twMisc}{
	## \itemize{
	## \item{ easy copying to clipboard: this method }
	## \item{ create a sequence based on a range c(min,max): \code{\link{seqRange}} }
	## \item{ merge several sequences to a single sequence: \code{\link{twMergeSequences}} }
	## \item{ recursively merge a named list or vector to another: \code{\link{twMergeLists}} }
	## \item{ Mediawiki-Code for table of given data.frame: \code{\link{twDf2wikiTable}} }
	## \item{ assign first variable of RData-file into a variable: \code{\link{loadAssign}} }
	## \item{ reorder factor levels: \code{\link{reorderFactor}} }
	## \item{ format to significant number of digits including trailing zeros: \code{\link{formatSig}} }
	## \item{ adds or replaces value in a vector: \code{\link{vectorElements<-}} }
	## \item{ extracting data frame collumn while keepign rownames: \code{\link{dfcol}} }
	## \item{ Compactly Display the Structure (\code{str}) with 3 levels without attributes: \code{\link{str3}} }
	## \item{ Retrieving and stripping filename extension: : \code{\link{fileExt}},\code{\link{twStripFileExt}} }
	## \item{ TODO: link functions: \code{\link{twDf2wikiTable}} }
	## }
	##}}
	
	##<<seealso \code{\link{write.table}}
	write.table(x,"clipboard",sep=sep, row.names=row.names, col.names=row.names, quote=quote, ...)
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





