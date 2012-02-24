extractProps <- function(env, prefix="", propNames=c("cid","desc") ){
	props <- list()
	envl <- as.list(env)
	#key <- names(envl)[1]
	#key <- "f1"
	for( key in names(envl) ){
		a <- attributes(envl[[key]])
		props[paste(prefix) a$bla
	}
}

.testSource <- function(){
	fileName <- "config.R"
	env <- local({
		source(fileName, local=TRUE)
		environment()
	})
	envl <- as.list(env)
	cids <- list( subList1=parse(text='testList$subList'))
	.tmp.f <- function(){
		fcid <- function(id){
			eval( cids[[id]], envir=env) 
		}
		fcid('subList1')
	}
	env$cid <- function(id){ eval(cids[[id]], envir=env ) }
	env$cid('subList1')
	str3( as.list(env) )
	try(with( env, f1() ))
	env$loca="loca3"
	with( env, f1() )
	with( env, attr(f1,"desc") )
	with( env, f2() )
}

