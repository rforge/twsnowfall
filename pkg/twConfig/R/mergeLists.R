.mergeLists <- function(
	### Recursively merge a list or vector to another list or vector
	dest		##<< list or vector that values are merged to
	, source	##<< list or vector that values are taken from
	, level=0	##<< level of recursion, used internally
){
	##seealso<< \link{twMisc}
	#
	##details<< 
	## Values of source are appended to dest or overwrite values in dest.
	## If an item is a vector or a list itself, the item is merged recursively. 
	#
	#recover()
	#if( level > 0 ) recover()
	if( 0==length(source) ) source=list()
	if( 0==length(dest) ) 
		return(source)
	if( 0==length(names(source)) && 0 != length(source) )
		if( 0==length(names(dest)) && length(dest)==length(source) ){
			return(source)
		}else{
			stop("mergeLists: need to provide names for the source list")
		}
	#key = names(source)[1]
	for( key in names(source)){
		destItem <- dest[[key]]
		sourceItem <- source[[key]]
		dest[[key]] <- if( length(destItem)==0 || !(is.vector(sourceItem) || is.list(sourceItem)) ){ 
				sourceItem
			}else{
				tmp <- .mergeLists(destItem, sourceItem, level+1)
				#if( is.list(tmp) ) list(tmp) else if( is.vector(tmp) )  else tmp
			}
	}
	##value<< modified argument dest
	dest
}
attr(.mergeLists,"ex") <- function(){
	dest0 <- list(a=1, b=list(b1=2, b2=4), c=2)
	source0 <- list(b=list(b1=12))
	(tmp <- .mergeLists(dest0,source0))
	#
	dest0 <- list(a=1, b=c(b1=2, b2=4), c=2)
	source0 <- list(b=c(b1=12))
	(tmp <- .mergeLists(dest0,source0))
	
	(tmp <- .mergeLists( list(), source0) )
	
	(tmp <- .mergeLists( dest0, NULL) )
	
}

