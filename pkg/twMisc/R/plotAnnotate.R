plotArrowText <- function(
        ### Add arrows and text to a plot 
        labels
        , X=NULL, Y=NULL     ##<< X,Y = position of the label and the tail of the arrow
        , x=NULL, y=NULL##<< position of the arrowhead; if NULL, then no arrows are drawn
        , adj=c(0,0)    ##<< which point to use in positioning the label, defaults to left/bottom
        , arrow.pos=1   ##<< the side from which the arrow tail emmanates, defaults to bottom (1), numbers go around clockwise: 1-4
        , col="black", cex=1,
                border=TRUE, border.col="grey", border.lwd=1, fill="white",
                offset=c(0,0),  arrow.lwd=1,
                arrow.angle=30, arrow.length=0.1, arrow.col="black"
){
##details<<     
## Annotate plots easily with a horizontal
## bit of text (a phrase) with optional filled rectangular box, and an
## optional arrow to a real data point (or area) on the plot, extending
## from the center of the text phrase.
##
## Either of the X/x can contain a list with x/X y/Y components, in which
## case the y/Y arguments are ignored.
##
## If X is not given, then the user has to locate points on the plot, arrow and text in turn
##
## Code was adopted from package heR.Misc.R
    arrow.pos <- rep(arrow.pos, length=length(labels))
    col <- rep(col, length=length(labels))
    cex <- rep(cex, length=length(labels))
    border <- rep(border, length=length(labels))
    border.col <- rep(border.col, length=length(labels))
    border.lwd <- rep(border.lwd, length=length(labels))
    fill <- rep(fill, length=length(labels))
    arrow.lwd <- rep(arrow.lwd, length=length(labels))
    arrow.angle <- rep(arrow.angle, length=length(labels))
    arrow.length <- rep(arrow.length, length=length(labels))
    arrow.col <- rep(arrow.col, length=length(labels))

    if( is.null(X) ){
        nlab <- length(labels)
        cat("\nClick on the plot twice locate arrowheads and text positions. Repeat this",nlab,"times\n")
        pos <- locator(nlab*2)
        evens <- (1:nlab)*2
        odds <- evens-1
        x <- pos$x[odds]
        y <- pos$y[odds]
        X <- pos$x[evens]
        Y <- pos$y[evens]
    } 
    
    if (is.list(X)) {
        Y <- X$y
        X <- X$x
    }  
    
    if (!is.null(x) & is.list(x)) {
        y <- x$y
        x <- x$x
    }
    
    if (is.null(Y)) 
        stop("`Y' should contain the label and arrow y coordinates, respectively, unless they are given as part of `x' or `X' lists.")
    
    heights <- c()
    widths <- c()
    for (i in 1:length(labels)) {
        heights[i] <- strheight(labels[i], cex=cex[i])
        widths[i] <- strwidth(labels[i], cex=cex[i])
    }   
    char.width <- widths/nchar(labels)
    char.height <- heights
    X <- rep(X, length=length(labels))
    Y <- rep(Y, length=length(labels))
    
    Xf <- X - adj[1]*widths
    Yf <- Y - adj[2]*heights
    
    xleft <- Xf - char.width - offset[1]
    xright <- Xf + widths + char.width + offset[1]
    ytop <- Yf + 3*char.height/2 + offset[2]
    ybottom <- Yf - char.height/2 - offset[2]
    
    width <- xright - xleft
    height <- ytop - ybottom
    
    if (!is.null(x) & !is.null(y)) {
        x <- rep(x, length=length(labels))
        y <- rep(y, length=length(labels))
        xtail <- c()
        ytail <- c()
        for (i in 1:length(labels)) {
            if (arrow.pos[i] == 1) {
                xtail[i] <- xleft[i] + width[i]/2
                ytail[i] <- ybottom[i]
            }
            if (arrow.pos[i] == 2) {
                xtail[i] <- xleft[i]
                ytail[i] <- ybottom[i] + height[i]/2
            }
            if (arrow.pos[i] == 3) {
                xtail[i] <- xleft[i] + width[i]/2
                ytail[i] <- ytop[i]
            }
            if (arrow.pos[i] == 4) {
                xtail[i] <- xright[i]
                ytail[i] <- ybottom[i] + height[i]/2
            }
        }
        arrows(xtail, ytail, x, y, angle=arrow.angle,
                lwd=arrow.lwd, col=arrow.col, length=arrow.length) 
    }    
    
    for (i in 1:length(labels))
        if (border[i]) 
            rect(xleft=xleft, ybottom=ybottom,
                    xright=xright, ytop=ytop,
                    col=fill, border=border.col, lwd=border.lwd)
    
    text(x=Xf, y=Yf, labels=labels, cex=cex, col=col, adj=c(0,0))
}
attr(plotArrowText,"ex") <- function(){
    plot(sin(1:10),type="l")
    cat("\nClick on the plot four times to place labels and arrowheads.\n")
    plotArrowText(c("Here is one label.","And another."),X=locator(2),
            x=locator(2),cex=c(1.4,3),arrow.lwd=c(2,4),arrow.length=c(0.1,0.25),
            border.lwd=c(1,3), fill=c("pink","white"), col=c("blue","green"),
            adj=c(0,1),border.col=c("orange","purple"))    
}

locateArrowText <- function(
    ### generate Arrow and Label positions source code for \code{\link{plotArrowText}}
    nlab=1  
    ,digits=2
){
    cat("\nClick on the plot twice locate arrowheads and text positions. Repeat this",nlab,"times\n")
    pos <- locator(nlab*2)
    evens <- (1:nlab)*2
    odds <- evens-1
    x <- pos$x[odds]
    y <- pos$y[odds]
    X <- pos$x[evens]
    Y <- pos$y[evens]
    ### string source code to be pasted into plotArrowText command 
    copy2clip(paste("X=",deparse(round(X,digits),width=500),",Y=",deparse(round(Y,digits),width=500),",x=",deparse(round(x,digits),width=500),",y=",deparse(round(y,digits),width=500),sep=""))
}
attr(locateArrowText,"ex") <- function(){
    if( FALSE ){    # not to be executed during package test
        plot(sin(1:10),type="l")
        copy2clip(locateArrowText(2))  # paste the result into the following command (second line) 
        plotArrowText(c("Here is one label.","And another.")
            ,X=c(6.45, 5.27),Y=c(0.81, -0.35),x=c(6.94, 5.73),y=c(0.61, -0.49)
            ,adj=c(1,0))
    }
}


