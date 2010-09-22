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
	
	twUtestF("applyLB",test="test.sfFArgsApplyLB")
	twUtestF("applyLB")
}

.tmp.f <- function(){
	
}

.tmp.f <- function(){
	# generate documentation	
	library(inlinedocs)
	unlink( file.path("man","*.Rd") )	
	package.skeleton.dx(".")
	#file.copy( Sys.glob(file.path("inst","genData","*.Rd")), "man" )	# copy descriptions of data
	#unlink(file.path("man","twDEMC.Rd"))   # else overwrites alias twDEMC to twDEMCInt 
}


