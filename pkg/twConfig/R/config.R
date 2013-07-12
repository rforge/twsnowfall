.stripConfigPropsItem <- function(
	item				##<< the item to extract
	, propNames=		##<< names of the attributes to extract
		c("cid","desc")
){
	##details<< 
	## If item is a sequence, i.e. an unnamed list,
	## and first entry are the cProps, i.e list with its first component in propNames
	## remove the cProps and if the sequence is of length one,
	cProps=list()
	if( length(item) &&
		is.list(item) && 0==length(names(item)) &&
		is.list(i1 <- item[[1]]) && 0!=length(names(i1)) && 
		names(i1)[1] %in% propNames
	){
		cProps <- i1
		item[1] <- NULL
		if( length(item) == 1) item <- item[[1]]
	}
	##value<< a List with components
	list(
		item=item		##<< item, with cProps removed and if remaining lenght=1, only the first element of the sequence 
		,props=cProps	##<< list: the config properties
		##end<< 
		)
}
.stripConfigProps <- function(
	### Extracting ids and desc attributes of the items and subItmes in the environment
	env				##<< the environment, thats bindings are changed
	, propNames		##<< names of the attributes to extract
		=c("cid","desc")
	, maxLevel=20	##<< maximum level or recursion
	, maxNEntry=256	##<< maximum nuber of entries in a vector, above which elements are not inspected.
	, isUpdateEnv=TRUE	##<< if TRUE, then env is updated so that all props are stripped
	##<< This prevents expeding each element of a huge vector entry.
	, prefix=""		##<< prefix, used during recursive calls
	, items=list()	##<< sublist corresponding to prefix			
	, level=0 		##<< level of calling, used during recursive calls
){
	if( 0==length(items) ) items <- 
		if( !nzchar(prefix) ){
			if( !is.environment(env)) stop(".stripConfigProps: env must be an environment.")
			items <- as.list(env)
		}else{
			expr <- parse(text=prefix)
			items <- eval( expr, envir=env )
		}
	props <- list()	
	#key <- names(envl)[1]
	#key <- "f1"
	#key <- "testList"
	namesl <- names(items)
	hasNames <- length(namesl) != 0
	#i <- 1	
	for( i in seq_along(items) ){
		key <- if(hasNames) paste( (if(nzchar(prefix))"$"else""), namesl[i], sep="") else paste("[[",i,"]]",sep="")
		pkey <- paste(prefix,key,sep="")
		resStrip <- .stripConfigPropsItem(items[[i]],propNames=propNames) 
		item <- resStrip$item
		propsItem <- resStrip$props
		#pName <- 'desc'
		if( length(propsItem)){
			if( !hasNames) warning(paste("twConfig: unnamed sequence containing described items'",pkey,"' will be replaced instead of merged during loading cascading configuration.",sep=""))
			for( pName in names(propsItem) )
				props[[pName]][pkey] <- propsItem[[pName]]
			#only need to replace entire list at the end
			#expr <- parse(text=paste("env$",pkey," <- item",sep=""))
			#eval(expr)
		}
		if( (is.list(item)  || (is.vector(item) && length(item) > 1)) &&
			#&& !(length(envl)==1 && identical(entry,envl)) # if entry==envl do not reprocess it 
			!identical(item,items) &&			# if entry==envl do not reprocess it 
			length(item) <= maxNEntry &&		# do not parse big vectors 
			level < maxLevel					# avoid too deep recursion
		){	
			resRec <- .stripConfigProps( env=env, propNames=propNames, maxLevel=maxLevel, maxNEntry=maxNEntry
				,prefix=pkey, items=item, level+1 )
			if( length(resRec$props) ){
				for( pName in names(resRec$props) )
					props[[pName]] <- c(props[[pName]], resRec$props[[pName]] )
				item <- resRec$items
			}
		} # end recursion
		items[[i]] <- item
	} # end item
	if( !nzchar(prefix) && isUpdateEnv ){
		.replaceAllBindings(env, items)
	}
	list(
		items = items
		,props = props
	)
	### list with those enties from propNames
	### ,each a character vector of listExpression to  
}


.tmp.f <- function(){
    cfText1 <- list(
		msg1 = list( list(cid="cid1", desc="message 1"), "Hello" )
		,vector = 1:3
		,subList = list( 
			subVector=list( list(cid="subVector", desc="subVector"), "msg1","msg2" )
			,subVector2=list( list(cid="subVector2", desc="subVector2"), c("msg1","msg2") )
			, subMsg="subMessage")
		,f1=function(){ TRUE }
	)
	yamlText1 <- as.yaml(cfText1) 
	cat( yamlText1 )
	tmp <- yaml.load( yamlText1 )
	item <- cfText1$msg1
	str(tmp2 <-.stripConfigPropsItem(item)) 
	
	envText1 <- list2env(cfText1)
	str(tmp2 <- .stripConfigProps( envText1 ))
	str(as.list(envText1))
	eval( parse(text=names(tmp2$props$cid)[1]), env=envText1)
}

.tmp.f <- function( # testEnv
	### testing the principle of working with environments
){
	fileName = "config.R"
	env <- new.env(parent=baseenv())
	env$fileName <- fileName
	evalq( source(fileName, local=TRUE), env )
	#envl <- if( is.environment(env) ) as.list(env) else env
	#cids <- list( subList1=parse(text='testList$subList'))
	props <- .extractPropsAttr(env)
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
	,representation(env="environment",props="list",unstripped="list"
		,cidLabel="character", cidFunctionName="character", descLabel="character"
		)
	##details<< 
	## Be warned that this class breaks the copy semantics. 
	## env returns an environment. All changes that you do to this environment 
	## will be visible in all other copies of the class.
	## Several functions, such as loadR, set, modify thise environment.
)
setMethod(initialize, "twConfig", function(.Object
		, ...
		, env=new.env()
		, cidLabel="cid"
		, cidFunctionName=cidLabel
		, descLabel="desc"
	) {
		callNextMethod(.Object, ..., env=env, props=list(), unstripped=list()
			,cidLabel=cidLabel, cidFunctionName=cidFunctionName, descLabel=descLabel
		)
	}) 

#.copyAndEmptyEnv <- function(
#	### Save the current binding of an environment to a list and then clear all binding from the environment
#	env		##<< the environment to process
#){
#	prevEnv <- as.list(env)		# save the current state of the environment, closures still point to env
#	evalq(rm(list=ls()),env)	# clear all binding from the environment
#	prevEnv
#	### The list holding all bindings previously in env
#}

.clearAllBindings <- function(
	### clear all the bindings in the environment 
	env		##<< the environment to process
){
	prevEnv <- as.list(env)		# save the current state of the environment, closures still point to env
	evalq(rm(list=ls()),env)	# clear all binding from the environment
	invisible(prevEnv)
	### The list holding all bindings previously in env
}


.replaceAllBindings <- function(
	### Clear all bindings from the environment and transfer all entries from biven list  
	env		## the environment to update
	,from	## the list which entries should be copied to the environment
){
	evalq(rm(list=ls()),env)		# clear it before reassigning
	for( key in names(from) )		# reassign the merged env
		env[[key]] <- from[[key]]
	invisible(from)
}

.updatePropsAndEnv <- function(
	### Extract the properties and add function cid to the environment
	object		##<< the twConfig object to update
	, propNames		##<< names of the attributes to extract
		=c( object@cidLabel, object@descLabel)
	,...		##<< further arguments passed to .extractProps
){
	#props <- .extractPropsAttr(object@env, ...)
	props <- .stripConfigProps(object@env, propNames=propNames,...)$props
	#as.list(object@env)
	# reverse key -> value and convert key string to an expression
	cids <- structure( sapply(names(props[[object@cidLabel]]),function(key){ parse(text=key)}), names=as.vector(props[[object@cidLabel]]) )
	#env$cid <- function(id){ eval(cids[[id]], envir=as.list(env) ) }
	object@env[[object@cidFunctionName]] <- function(id){
		expr <- cids[[id]]
		if( is.null(expr) ) warning(paste("twConfig: no cid found for key",id))
		eval(expr, envir=object@env ) }
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
	# Assume that unstripped holds all the bindings		
	.clearAllBindings( object@env )	
	object@env$fileName <- fileName
	evalq( source(fileName, local=TRUE), object@env )
	object@unstripped <- .mergeItem( object@unstripped, as.list(object@env) )
	.replaceAllBindings(object@env, object@unstripped) 
	object@props <- .updatePropsAndEnv(object)
	invisible(object)
})

setGeneric("loadYaml",	function(
		### parsing and Yaml file into a configuration
		object					##<< the class object
		,fileName="config.yml"	##<< the file to parse
		,...
	){ standardGeneric("loadYaml") })
setMethod("loadYaml","twConfig",function(
		### parsing and Yaml file into a configuration
		object					##<< the class object
		,fileName="config.yml"	##<< the file to parse
		,...
		,isSubstBacktick=TRUE	##<< if set to FALSE the backTick substitution is not done
	){
		if( !require(yaml)) stop("loadYaml: package yaml needs to be installed to use this functionality.")
		yml <- yaml.load_file( fileName )
		object@unstripped <- .mergeItem( object@unstripped, yml )
		.replaceAllBindings(object@env, object@unstripped) 
		object@props <- .updatePropsAndEnv(object)
		if( isSubstBacktick )
			substBacktick(object)
		invisible(object)
	})


setMethod("show","twConfig",function(object){
	cat("twConfig object of class",	classLabel(class(object)), "\n")
	desc <- object@props[[object@descLabel]]
	envl <- as.list(object@env)
	#key <- names(desc)[1]
	for( key in head(names(desc),6) ){
		cat("-- ",key,":", desc[[key]] ,"\n")
		if( is.finite(match("$",key)) ) recover()
		expr <- parse(text=key)
		tmp <- eval( expr, envir=envl )
		#attributes(tmp)[c("desc","cid")] <- NULL
		if( is.function(tmp)) str(args(tmp)) else 
		if( is.list(tmp) || is.vector(tmp)) str(tmp, max.level=3, give.attr = FALSE) else
		print(tmp)
	}
	if( length(desc) > 6)
		cat("out of ",length(desc),"described items. str(getv(<config>)).\n")
	if( length(object@props[[object@cidLabel]]) )
		cat(object@cidLabel,":",paste(object@props[[object@cidLabel]],collapse=","),"\n")
})

setGeneric("getv",	function(x,...){standardGeneric("getv")})
setMethod("getv","twConfig",function(x,path="",...){
		if( length(grep("<-",path)) )
			stop("twConfig.getv: <- not allowed in path expression.")
		if( 0==length(path) || !nzchar(path) )	as.list(x@env) else{
			expr <- parse(text=path)
			eval( expr, envir=x@env)
		}
	})

setGeneric("setv",	function(x,...){standardGeneric("setv")})
setMethod("setv","twConfig",function(x,path="",value="TRUE",...){
		if( 0!=length(path) && nzchar(path) ){
			expr <- parse(text=paste(path," <- '",value,"'",sep=""))
			eval( expr, envir=x@env)
		}
	})


#setGeneric("env",	function(
#		### obtaining the environment holding the configuration
#		object,... ){standardGeneric("env")})
#setMethod("env","twConfig",function(object){
#		object@env
#	})

setGeneric("getCid",	function(
	### obtaining configuration for config id
	object,... ){standardGeneric("getCid")})

setMethod("getCid","twConfig",function(object,...){
		object@env[[object@cidFunctionName]](...)
	})

setGeneric("getDesc",	function(object,... ){standardGeneric("getDesc")})
setMethod("getDesc","twConfig",function(object,path="",...,pattern=paste("^",gsub("\\$","\\\\$",path),sep="")){
		desc <- object@props[[object@descLabel]]
		if( 0==length(pattern) || !nzchar(pattern) ){
			desc
		}else{
			i <- grep( pattern, names(desc))
			desc[i]
		}
	})

.substBacktick <- function(
	### do a backtick substituion
	s0				##<< scalar string that need to be substituted
	,envir=parent.frame()	##<< the Environment where sustitutions should take place
	,maxSubst=25	##<< maximum number of substituions
){
	##details<< 
	## A backtick substitution could create more backticks, resulting
	## in an endless loop of subsitions. Hence limit the number of substitutions.
	pattern="`([^`]*)`"
	s1 <- s0
	m <- regexpr(pattern,s1)
	l <- attr(m,"match.length")
	if( l == nchar(s1) ){
		# when replacing the entire string just return the evaluated R object
		sm0 <- substr(s1,2,m+l-2)
		ret <- try({
				expr <- parse(text=sm0)
				eval(expr, envir=envir)	
			},silent=TRUE)
		if( inherits(ret,"try-error")){
			warning("error in twConfig: parsing text '",sm0,"': ",ret)
			ret <- "error"
		}
		return(ret)
	}
	iSubst <- 0
	while( m != -1 && iSubst < maxSubst ){
		iSubst <- iSubst + 1
		l <- attr(m,"match.length") 
		sm1 <- if( l == 2) "" else {
				sm0 <- substr(s1,m+1, m+l-2)
				sm1 <- try({ 
					expr <- parse(text=sm0)
					eval(expr, envir=envir) 
				},silent=TRUE)
				if( inherits(sm1,"try-error")){
					warning("error in twConfig: parsing text '",sm0,"': ",sm1)
					sm1 <- "error"
				}
				sm1
			} 
		s1 <- paste(substr(s1,1,m-1),sm1,substr(s1,m+l,nchar(s1)),sep="")
		m <- regexpr(pattern,s1)
	}
	s1
}
attr(.substBacktick, "ex") <- function(){
	b="chars of b"
	s0 <- "2+3=`2+3`, b=`b`, empty<``>"
	#gsub( "`[^`]*`","X",s0)
	(s1 <- .substBacktick(s0))
	.substBacktick("`function(a){TRUE}`")
}

setGeneric("substBacktick",	function(object,... ){standardGeneric("substBacktick")})
setMethod("substBacktick","twConfig",function(object,...){
		#fTmp <- function(entry){"XX"}
		#res <- rapply( as.list(object@env), fTmp, classes="character", how="replace" )
		#.substBacktick('item=`cid("yamlItem1")`',envir=object@env)
		res <- rapply( as.list(object@env), .substBacktick, classes="character", how="replace", envir=object@env )
		.replaceAllBindings(object@env,res)
		invisible(res)
	})

setGeneric("copySubConfig",	function(object,... ){standardGeneric("copySubConfig")})
setMethod("copySubConfig","twConfig",function(
		### copy a subTree of the configuration 
		object,path="",...,isSubstBacktick=TRUE
	){
		 #recover()
		 copy <- object
		 copy@env <- new.env()
		 tmp <- object@unstripped
		 iSub <- grep(paste("^",gsub("\\$","\\\\$",path),sep=""), names(tmp) )
		 if( path=="" || length(iSub) > 1 ){
			 copy@unstripped <- tmp[ iSub ]
		 } else if( 1==length(iSub) ){
			 copy@unstripped <- tmp[[ iSub ]]
			 # possibly remove first describing entry
			 if( any( c(copy@cidLabel,copy@descLabel) %in% names(copy@unstripped[[1]]) ))
				 copy@unstripped <- copy@unstripped[[2]]
		 } else if( 0==length(iSub) ){
			 copy@unstripped <- list()
		 }
		 .replaceAllBindings(copy@env, copy@unstripped )
		 copy@props <- .updatePropsAndEnv(copy)
		 if( isSubstBacktick )
			 substBacktick(copy)
		 invisible(copy)
#			 #pName <- names(copy@props)[1]
#			 for( pName in names(copy@props) ){
#				 prop <- copy@props[[pName]] 
#				 iSubProps <- grep(paste("^",gsub("\\$","\\\\$",path),"\\$",sep=""), names(prop) )
#				 prop <- prop[iSubProps]
#				 #name <- names(prop)[1]			 
#				 names(prop) <- 
#					 sapply(names(prop),function(name){ substr(name, start=nchar(path)+2, stop=nchar(name))})
#				 copy@props[[pName]] <- prop
#			 }
	})








.tmp.f <- function(){
	cfg0 <- new("twConfig")
	names(as.list(cfg0@env))
	(cfg1 <- loadR(new("twConfig")))
	names(as.list(cfg1@env))
	#loadR(cfg1)
	getCid(cfg1, cfg1@props$cid[[1]])
	getDesc(cfg1)
	(tmp <- getv(cfg1,"testList$vectorItem2"))
	try(getv(cfg1,"f1") <- "foo") # must throw an error, not allowed to change values
	rm(loca)
	try(getv(cfg1,"f1")()) # should throw an error because of missing loca
	loca="locaCallFrame"
	getv(cfg1,"f1")()		# should use now defined loca 
	getv(cfg1,"f1")(loca="locaDots") 	# should use arguments by ...
	getv(cfg1,"f2")()		# internal resolve by function cid
	#
	#(cfg2 <- loadYaml(cfg1)); getv(cfg2,"f3")() 
	(cfg2 <- loadYaml(cfg1, isSubstBacktick=FALSE))
	names(as.list(cfg2@env))
	getv(cfg2,"yamlItem1")
	getv(cfg2,"msg")  # now updated
	getCid(cfg2,"subItem1")
	getCid(cfg2,"yamlItem1")
	getv(cfg2,"f1")()		# should use now loca defined in Yaml file 
	getv(cfg2,"f1")(loca="locaDots") 	# should use arguments by ...
	getv(cfg2,"f3")
	substBacktick(cfg2)
	getv(cfg2,"ev2")
	getv(cfg2,"f3")()
	}

loadConfig <- function(
	### Loading configuration from file
	fileNames = c("config.yml")	##<< character vector: fileNames, order matters: duplicate entries are overwritten
	,...	##<< further arguments passed to new("twConfig")
){
	cfg <- new("twConfig",...)
	for( fName in fileNames ){
		ext <- .fileExt(fName)
		cfg <- switch(ext
			,R =  loadR(cfg,fName)
			,yml = loadYaml(cfg,fName, isSubstBacktick = FALSE)
			,stop(paste("loadConfig: unknown file format",ext))
		)
	}
	substBacktick(cfg)
	cfg
}
.tmp.f <- function(){
	(cfg <- loadConfig())
	(cfg <- loadConfig(c("config.yml","config.R")))
	getv(cfg,"msg")	# "Hello world" from config.R, overwriting msg in yml
	getv(cfg,"ev2")	# check that backtick substitution works
}








