twEnumNames <- function(
    ### Creates integer variables with positions from colnames or names of x.
    x   
    ,inherits=TRUE  ##<< if FALSE checks only for existence in calling frame. Use with care!
    ,prefix="i_"    ##<< string scalar: prefix for the variabl names
){
    ##details<< Use with care: new variables are defined in 
    if( is.matrix(x) ) nms = colnames(x)
    else nms <- names(x)
    nms <- paste(prefix,nms,sep="")
    nms <- make.names(nms)
    pf <- parent.frame()    #evaluate outside loop
    for( i in seq(along.with=x) ){
        if( exists(nms[i], where=pf, inherits=inherits) )
            stop(paste("tried to override existing variable",nms[i]))
        assign( nms[i], i, pos=pf )
    }
    invisible(nms)
    ### Names of the variables assigned in parent frame.
}
#see .example.twEnumNames

