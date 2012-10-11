# tries to sources the R files from twMisc without reinstalling the package
# need to define twMiscHome="Z:/twutz/projects/R/twMisc" before, e.g. in .Rprofile in home directory

if(!exists("twMiscHome")){
	warning("twMisc/inst/incl/sourceFiles: twMiscHome not defined")
	twMiscHome="Z:/twutz/projects/Rdev/twMisc"
}

twSourceMiscFiles <- function(){
	try(source(paste(twMiscHome,"/R/unitTest.R",sep="")))
	try(source(paste(twMiscHome,"/R/debug.R",sep="")))
	try(source(paste(twMiscHome,"/R/misc.R",sep="")))
	try(source(paste(twMiscHome,"/R/parallel.R",sep="")))
	try(source(paste(twMiscHome,"/R/extract_doc_chunk.R",sep="")))
}

twSourceMiscFiles()


