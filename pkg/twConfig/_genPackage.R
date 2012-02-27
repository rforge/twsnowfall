
.tmp.f <- function(){
	devMode <- TRUE		#rm(devMode)
	pkg <- "twConfig"
	library(twMisc)	#
	library(RUnit)
	tmp <- sapply(Sys.glob(file.path("R","*.R")), source)
	
	
	twUtestF("Config", divertOutputFile=NULL )
	
}

