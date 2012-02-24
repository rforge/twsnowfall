.extractProps <- function(
	### Extracting ids and desc attributes of the items and subItmes in the environment
	env				##<< the environment, list or named vector to extract from				
	, propNames		##<< names of the attributes to extract
		=c("cid","desc")
	, maxLevel=20	##<< maximum level or recursion
	, maxNEntry=256	##<< maximum nuber of entries in a vector, above which elements are not inspected. 
		##<< This prevents expeding each element of a huge vector entry.
	, prefix=""		##<< prefix, used during recursive calls
	, level=0 		##<< level of calling, used during recursive calls
){
	props <- list()	
	envl <- if( is.environment(env) ) as.list(env) else env
	if( !(is.list(envl) || is.vector(envl)) ) 
		stop("extractProps: env must be an environment, list, or named vector")
	#key <- names(envl)[1]
	#key <- "f1"
	#key <- "testList"
	#i <- 1
	namesl <- names(envl)
	hasNames <- length(namesl) != 0
	for( i in seq_along(envl) ){
		key <- if(hasNames) paste( (if(nzchar(prefix))"$"else""), namesl[i], sep="") else paste("[[",i,"]]",sep="")
		pkey <- paste(prefix,key,sep="")
		entry <- envl[[i]]
		a <- attributes(entry)
		#pName <- 'desc'
		for( pName in propNames)
			if( length(val <- a[[pName]]) ) props[[pName]][pkey] <- a[[pName]]
		if( (is.list(entry) || is.vector(entry))
			#&& !(length(envl)==1 && identical(entry,envl)) # if entry==envl do not reprocess it 
			&& !identical(entry,envl) 			# if entry==envl do not reprocess it 
			&& length(entry) <= maxNEntry		# do not parse big vectors 
			&& level < maxLevel					# avoid too deep recursion
		){	
			propsL <- .extractProps( env=entry, propNames=propNames, maxLevel=maxLevel, maxNEntry=maxNEntry
				,prefix=pkey, level=level+1 )
			for( pName in names(propsL) )
				props[[pName]] <- c(props[[pName]], propsL[[pName]] )
		}
	}
	props
	### list with those enties from propNames
	### ,each a character vector of listExpression to  
}
.tmp.f <- function(){
	(tmp <- .extractProps(env))
}


.testEnv <- function(
	### testing the principle of working with environments
){
	fileName = "config.R"
	env <- new.env(parent=baseenv())
	env$fileName <- fileName
	evalq( source(fileName, local=TRUE), env )
	#envl <- if( is.environment(env) ) as.list(env) else env
	#cids <- list( subList1=parse(text='testList$subList'))
	props <- .extractProps(env)
	# reverse key -> value and convert key string to an expression
	cids <- structure( sapply(names(props$cid),function(key){ parse(text=key)}), names=as.vector(props$cid) )
	.tmp.f <- function(){
		fcid <- function(id){
			eval( cids[[id]], envir=env) 
		}
		fcid('subList1')
	}
	env$cid <- function(id){ eval(cids[[id]], envir=env ) }
	env$cid('subList1')
	str3( as.list(env) )
	try(with( env, f1() ))	# loca not yet defined
	env$loca="loca3"
	#with( env, f1() )		# now it should work
	with( env, f1(loca="loca3") )		# now it should work
	with( env, attr(f1,"desc") )
	with( env, f2() )		# note how function cid is used
	f2 <- with(env, f2)
	#
	envl <- as.list(env)
	with(envl, f2)()
	evalq( rm(list=ls()), env)
	with(envl, f2)()
	for( key in names(envl) )
		env[[key]] <- envl[[key]]
	env$cid <- function(id){ eval(cids[[id]], envir=envl ) }
	with( envl, f1)(loca="loca3")		# now it should work
	}

#----------------- twConfig S4 class -------------------
twConfig <- setClass("twConfig"
	,representation(env="environment",props="list")
)
setMethod(initialize, "twConfig", function(.Object, ...) {
		callNextMethod(.Object, ..., env=new.env(), props=list() )
	}) 

.copyAndEmptyEnv <- function(
	### Save the current binding of an environment to a list and then clear all binding from the environment
	env		##<< the environment to process
){
	prevEnv <- as.list(env)		# save the current state of the environment, closures still point to env
	evalq(rm(list=ls()),env)	# clear all binding from the environment
	prevEnv
	### The list holding all bindings previously in env
}

.mergeEnvToList <- function(
	### recursively merge an environment to a list
	prevEnv	## the list which entries should be recursive updated or appended
	,env	## the environment to update
){
	mergedEnv <- .mergeLists( prevEnv, as.list(env) )
	evalq(rm(list=ls()),env)			# clear it before reassigning
	for( key in names(mergedEnv) )		# reassign the merged env
		env[[key]] <- mergedEnv[[key]]
	invisible(mergedEnv)
}

.updatePropsAndEnv <- function(
	### Extract the properties 
	object		##<< the twConfig object to update
	,...		##<< further arguments passed to .extractProps
){
	props <- .extractProps(object@env, ...)
	# reverse key -> value and convert key string to an expression
	cids <- structure( sapply(names(props$cid),function(key){ parse(text=key)}), names=as.vector(props$cid) )
	#env$cid <- function(id){ eval(cids[[id]], envir=as.list(env) ) }
	object@env$cid <- function(id){ eval(cids[[id]], envir=object@env ) }
	props
	### the updated props that need to be assigned to the object
}


setGeneric("loadR",	function(
	### parsing and R file into a configuration
	object					##<< the class object	
	,fileName="config.R"	##<< the file to parse
){ standardGeneric("loadR") })
setMethod("loadR","twConfig",function(
	### parsing and R file into a configuration
	object					##<< the class object	
	,fileName="config.R"	##<< the file to parse
){
	##details<< 
	## The R file is source into a local environment.
	# source must be done in the same environment as the current environment
	# so that all closures point to the same frame
	# Hence we copy all entries, source into an emptied env, merge, and retransfer to the emptied env
	prevEnv <- .copyAndEmptyEnv( object@env )	
	object@env$fileName <- fileName
	evalq( source(fileName, local=TRUE), object@env )
	.mergeEnvToList( prevEnv, object@env)
	object@props <- .updatePropsAndEnv(object)
	invisible(object)
})

setGeneric("loadYaml",	function(
		### parsing and Yaml file into a configuration
		object					##<< the class object	
		,fileName="config.yml"	##<< the file to parse
	){ standardGeneric("loadYaml") })
setMethod("loadYaml","twConfig",function(
		### parsing and Yaml file into a configuration
		object					##<< the class object	
		,fileName="config.yml"	##<< the file to parse
	){
		if( !require(yaml)) stop("loadYaml: package yaml needs to be installed to use this functionality.")
		yml <- yaml.load_file( fileName )
		prevEnv <- .copyAndEmptyEnv( object@env )
		merged <- .mergeLists(prevEnv, yml)
		.mergeEnvToList( merged, object@env)
		object@props <- .updatePropsAndEnv(object)
		invisible(object)
	})


setMethod("show","twConfig",function(object){
	cat("twConfig object of class",	classLabel(class(object)), "\n")
	desc <- object@props$desc
	envl <- as.list(object@env)
	for( key in head(names(desc),20) ){
		cat("-- ",key,":", desc[[key]] ,"\n")
		if( is.finite(match("$",key)) ) recover()
		expr <- parse(text=key)
		tmp <- eval( expr, envir=envl )
		attributes(tmp)[c("desc","cid")] <- NULL
		if( is.function(tmp)) str(args(tmp)) else 
		if( is.list(tmp) ) str(tmp, max.level=3, give.attr = FALSE) else
		print(tmp)
	}
	if( length(desc) > 20)
		cat("out of ",length(desc),"described items.\n")
	cat("cid:",paste(object@props$cid,collapse=","),"\n")
})

#setGeneric("getEnv",	function(object ){standardGeneric("getEnv")})
#setMethod("getEnv","twConfig",function(object){
#		recover()
#		object@env
#	})

setMethod("get","twConfig",function(x){
		x@env
	})

setGeneric("getCid",	function(
	### obtaining configuration for config id
	object,... ){standardGeneric("getCid")})

setMethod("getCid","twConfig",function(object,...){
		object@env$cid(...)
	})

setGeneric("getDesc",	function(object,... ){standardGeneric("getDesc")})
setMethod("getDesc","twConfig",function(object,...){
		object@props$desc
	})



.tmp.f <- function(){
	cfg0 <- new("twConfig")
	names(as.list(get(cfg0)))
	(cfg1 <- loadR(new("twConfig")))
	names(as.list(get(cfg1)))
	#loadR(cfg1)
	getCid(cfg1, cfg1@props$cid[[1]])
	getDesc(cfg1)
	(tmp <- get(cfg1)$testList$scalarItem)
	rm(loca)
	try(get(cfg1)$f1())	# should throw an error because of missing loca
	loca="locaCallFrame"
	get(cfg1)$f1()		# should use now defined loca 
	get(cfg1)$f1(loca="locaDots") 	# should use arguments by ...
	get(cfg1)$f2()		# internal resolve by function cid
	#
	(cfg2 <- loadYaml(cfg1))
	names(as.list(get(cfg2)))
	get(cfg2)$yamlItem1
	get(cfg2)$msg  # now updated
	(tmp <- get(cfg2)$ev1)  
	(tmp2 <- substr(tmp,2,nchar(tmp)-1))
	eval( parse(text=tmp2), env=get(cfg2) )  
	get(cfg2)$f1()		# should use now loca defined in Yaml file 
	get(cfg2)$f1(loca="locaDots") 	# should use arguments by ...
}

.tmp.f <- function(){
	# S4
	require(lme4)
	?"mer-class"
	#
	showMethods("show") ## show all methods for show
	?show ## shows the generic documentation of show
	method?show("mer") ## method?generic("signature 1", "signature 2", ...) -- get help for the generic function for a particular signature list, usually a single class
	getMethod("show", signature="mer") ## function definition
	lme4:::printMer ## printMer is what the show method for mer calls	
}





