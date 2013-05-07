pngBitmap <- function(
    ### savely opening a png Device
    fileName    ##<< string scalar: the output file name
    ,...        ##<< further arguments to png or bitmap
){
    ##details<<
    ## On unix systems without X11 \code{\link{png}} using a graphics window fails, 
    ## while on Windows systems \code{\link{bitmap}} using possibly missing ghostscript fails.
    ##
    ## \code{pngBitmap} tries both approaches.
    ##
    res <- suppressWarnings(try( png(fileName,...), silent=TRUE ))
    if( inherits(res, "try-error") ){
        bitmap(fileName,...)
    }
}
attr( pngBitmap, "ex" ) <- function(){
    if( FALSE ){    # do not execute on INSTALL
        pngBitmap("test1.png")
        plot(1:5,5:1)
        dev.off()
    }
}
