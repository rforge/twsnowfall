# TODO: Add comment
# 
# Author: twutz
###############################################################################


buildGTemplate <- function(fileNameIn, fileNameOut = sub(pattern, replacement, fileName), replacement, pattern="template"){
    # buildGTemplate
    #
    # reads fileNameIn and processes each line through GString, substituting patterns
    # fileNameOut: name of the file to be created
    #   defaults to fileNameIn with text given in argument pattern replaced by text given in replacement
    # pattern: text in fileNameIn to be replaced to form fileNameOut
    # replacement: replacement of pattern in fileNameIn to form fileNameOut
    # requries GString from R.utils
    lines <- readLines(fileNameIn)
    linesSubst <- unlist(lapply( lines, function(line){as.character(GString(line))}))
    writeLines(linesSubst, fileNameOut)
}
#mtrace(buildGTemplate)

tmp.f <- function(){
    #example to invoke buildGTemplate
    library(R.utils) #GString
    library(debug)
    
    fileName <- "tw.DESS/incl_cluster/demc_template_test.R"
    
    model<-"FS"
    ofSourceFile<-"tw.DESS/R/of_Hamer_Bayes3.R"
    
    buildGTemplate( fileName, replacement=model )
    buildGTemplate( fileName, "tw.DESS/incl_cluster/demc_FS_test.R" )
} 

