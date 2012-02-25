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
	## If source holds unnamed components, they replace unnamed components of dest
	#
	#recover()
	#if( level > 0 ) recover()
	if( 0==length(source) ) source=list()
	if( 0==length(dest) ) 
		return(source)
	if( 0==length(names(source)) && 0 != length(source) )
		if( 0==length(names(dest)) ){	# if dest has no names, replace dest
			return(source)
		}else{
			stop("mergeLists: source has no names but dest has.")
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


.mergeItem <- function(
	### Recursively merge an possibly self-documenting item
	dest		##<< list or vector that values are merged to
	, source	##<< list or vector that values are taken from
	, propNames=		##<< names of the attributes to extract
		c("cid","desc")
	, level=0	##<< level of recursion, used internally
){
	##seealso<< \link{twMisc}
	#
	##details<< 
	## Values of source are appended to dest or overwrite values in dest.
	## If an item is a vector or a list itself, the item is merged recursively.
	## If source holds unnamed components, they replace unnamed components of dest
	#
	#recover()
	#if( level > 0 ) recover()
	if( 0==length(source) ) source=list()
	if( 0==length(dest) ) 
		return(source)
	stripSource <- .stripConfigPropsItem( source, propNames=propNames)
	sourceItem <- stripSource$item
	stripDest <- .stripConfigPropsItem( dest, propNames=propNames)
	destItem <- stripDest$item
	mergedProps <- .mergeLists( stripDest$props, stripSource$props )
	mergedItem <- if( 0==length(names(sourceItem)) && 0 != length(sourceItem) ){
		if( 0==length(names(destItem)) ){	# if dest has no names, replace dest
			sourceItem
		}else{
			stop("mergeLists: source has no names but dest has.")
		}
	}else{
		#key = names(source)[1]
		for( key in names(sourceItem)){
			destItemEntry <- destItem[[key]]
			sourceItemEntry <- sourceItem[[key]]
			destItem[[key]] <- if( 
					length(destItemEntry)==0 || 									# empty dest
					!(is.vector(sourceItemEntry) || is.list(sourceItemEntry))  	# atomic dest
				){ 
					sourceItemEntry
				}else{
					tmp <- .mergeItem(destItemEntry, sourceItemEntry, propNames=propNames, level=level+1)
					#if( is.list(tmp) ) list(tmp) else if( is.vector(tmp) )  else tmp
				}
		}
		destItem
	}
	if( length(mergedProps) ){
		list( mergedProps, mergedItem )
	}else
		mergedItem
}	
.tmp.f <- function(){
	require(RUnit)
	dest0 <- list()
	dest1 <- list( list(desc='top list item', cid="cid1")
		,list(
			vectorItem1 = 1:3
			,vectorItem2 = list( list(desc='subItem with cid', cid='subItem1')
				,1:3)
			,subList = list( list(desc='subList with cid', cid='subList1') 
				,list( 
					vectorItem = 1:5
					,subSubItem = "desc of subSubItem"
				))
		))
	source0 <- list()
	res <- .mergeItem( dest0, source0 )
	checkEquals( list(), res)
	str(res <- .mergeItem( dest1, source0 ))
	checkEquals( dest1, res)
	#
	source1 <- list(vectorItem1=1)	# element of the item
	res <- .mergeItem( dest0, source1 )
	checkEquals( source1, res)
	str(res <- .mergeItem( dest1, source1 ))
	.exp <- dest1
	.exp[[2]]$vectorItem1 <- 1
	checkEquals( .exp, res)
	#
	source2 <- list( list(desc="bla") )	# element of the props
	res <- .mergeItem( dest0, source2 )
	checkEquals( source2, res)
	str(res <- .mergeItem( dest1, source2 ))
	.exp <- dest1
	.exp[[1]]$desc <- "bla"
	checkEquals( .exp, res)
	#
	source3 <- list(vectorItem2=list(list(cid='cidChanged'),1), subList=list(vectorItem=1))	# element of the item
	#source3 <- list(vectorItem2=list(list(cid='cidChanged'),1))	# element of the item
	res <- .mergeItem( dest0, source3 )
	checkEquals( source3, res)
	str(res <- .mergeItem( dest1, source3 ))
	.exp <- dest1
	.exp[[2]]$vectorItem2[[1]]$cid <- "cidChanged"
	.exp[[2]]$vectorItem2[[2]] <- 1
	.exp[[2]]$subList[[2]]$vectorItem <- 1
	checkEquals( .exp, res)
}
