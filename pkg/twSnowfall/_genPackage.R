#------------ generating package tw.DEMC
# inlcuding generation of Rd files from inline-docs
# install.packages("inlinedocs")

.tmp.f <- function(){
	library(twMisc)
	library(snowfall)
	sfInit(parallel=TRUE,cpus=2)
	#library(inlinedocs) #with twMisc
	
	tmp <- sapply(Sys.glob(file.path("R","*.R")), source)
	
	twUtestF()
	sfInit(parallel=FALSE)
	twUtestF()
	
	twUtestF("sfPar",test="sfParSequential")
	twUtestF("applyLB")
}

.tmp.f <- function(){
	
}


.tmp.f <- function(){
	pkg<-"twSnowfall"
	library(inlinedocs)
	unlink( file.path("man","*.Rd") )	
	package.skeleton.dx(".")
	try(file.copy( Sys.glob(file.path("inst","genData","*.Rd")), "man" ), silent=TRUE)	# copy descriptions of data
	
	# generate the HTML  files
	prevWd <- setwd("..")
	system(	paste("R CMD INSTALL --html ",pkg, sep="") )
	setwd(prevWd)
	
	# show in Browser
	htmlRoot <- file.path( system.file(package = pkg), "html" )
	html_viewer <- function(path) {
		browser <- getOption("browser")
		if(is.null(browser) && .Platform$OS.type == "windows")
			shell.exec(chartr("/", "\\", path))
		else browseURL(paste("file://", URLencode(path), sep=""))
	}
	html_viewer(file.path(htmlRoot,"00Index.html"))
	
}


