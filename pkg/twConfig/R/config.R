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
			propsL <- extractProps( env=entry, propNames=propNames, maxLevel=maxLevel, maxNEntry=maxNEntry
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
	(tmp <- extractProps(env))
}


.testEnv <- function(
	### testing the principle of working with environments
){
	fileName <- "config.R"
	env <- local({
		source(fileName, local=TRUE)
		environment()
	})
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
}

#----------------- twConfig S4 class -------------------
twConfig <- setClass("twConfig"
	,representation(env="environment",props="list")
)

setGeneric("mergeList",	function(
		### updateing the current configuration
		object			##<< the class object	
		,from			##<< some object that can be coerced to list holding new configuration
	){ standardGeneric("mergeList") })
setMethod("mergeList","twConfig",	function(
		### updateing the current configuration
		object			##<< the class object	
		,from			##<< some object that can be coerced to list holding new configuration
	){
		object@env <- env <- as.environment(.mergeLists(as.list(object@env), as.list(from)))
		object@props <- props <- .extractProps(object@env)
		# reverse key -> value and convert key string to an expression
		cids <- structure( sapply(names(props$cid),function(key){ parse(text=key)}), names=as.vector(props$cid) )
		#object@env$cid <- function(id){ eval(cids[[id]], envir=as.list(object@env) ) }
		object@env$cid <- function(id){ eval(cids[[id]], envir=object@env ) }
		object
	})

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
	env <- local({
			source(fileName, local=TRUE)
			environment()
		})
	mergeList(object,env)
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
		mergeList(object,yml)
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
	(cfg1 <- loadR(new("twConfig")))
	getCid(cfg1, cfg1@props$cid[[1]])
	getDesc(cfg1)
	(tmp <- get(cfg1)$testList$scalarItem)
	try(get(cfg1)$f1())
	f1 <- get(cfg1)$f1 
	f1(loca="loca3")
	get(cfg1)$f2
	get(cfg1)$f2()
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





