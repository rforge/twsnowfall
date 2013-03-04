pngBitmap <- function(
    ### opening a png Device, either by \code{\link{png}} using a graphics windows or \code{\link{bitmap}} using ghostscript
    fileName    ##<< string scalar: the output file name
    ,...        ##<< further arguments to png or bitmap
){
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