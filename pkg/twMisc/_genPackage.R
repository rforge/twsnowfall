#------------ generating package tw.DEMC
# inlcuding generation of Rd files from inline-docs
# install.packages("inlinedocs")

library(RUnit)
library(abind)
library(R.methodsS3)

tmp <- sapply(Sys.glob(file.path("R","*.R")), source)

#mtrace(twUtest)
#(res <- twUtestF(tw.fTestDummy,divertOutputFile=NULL))
#(res <- twUtestF(twUtest,divertOutputFile=NULL))
#(res <- twUtestF(twUtestF,divertOutputFile=NULL))
#(res <- twUtest(divertOutputFile=NULL))
#(res <- twUtestF(divertOutputFile=NULL))
(res <- twUtestF())


.tmp.f <- function(){
	# generate documentation	
	library(inlinedocs)
	unlink( "man", recursive=TRUE)	# take care, entire man directory deleted
	package.skeleton.dx(".")
	#file.copy( Sys.glob(file.path("inst","genData","*.Rd")), "man" )	# copy descriptions of data
}


tmp.f <- function(){
	mtrace(package.skeleton.dx)
	mtrace(extract.docs.chunk)
	mtrace(modify.Rd.file)
	mtrace(modify.Rd.file,F)
}
#R CMD check --no-vignettes --no-latex --no-install twMisc
#R CMD check --no-vignettes --no-latex --no-codoc twMisc
#R CMD INSTALL --html twMisc
