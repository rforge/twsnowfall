devMode <- TRUE		#rm(devMode)
pkg <- "twConfig"

.tmp.f <- function(){
	library(twMisc)	#
	tmp <- sapply(Sys.glob(file.path("R","*.R")), source)
	
	
	twUtestF("Config", divertOutputFile=NULL )
	
}

