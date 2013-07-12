twResultFilename <- function(
	### Construct a modified filename from paramFilename.
	paramFilename	##<< filename to modify
	,iproc=NULL	##<< a number to append to basename before .RData (if NULL nothing is appended)
	,prefix="res_"	##<< prefix to prepend to basename
	,ext=".RData"	##<< file extension
){
	if( 0 < length(iproc))	
		file.path(dirname(paramFilename),paste(prefix,twMisc::twStripFileExt(basename(paramFilename)),"_",iproc,ext,sep=""))
	else
		file.path(dirname(paramFilename),paste(prefix,twMisc::twStripFileExt(basename(paramFilename)),ext,sep=""))
}
