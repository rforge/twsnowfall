\name{plotArrowText}
\alias{plotArrowText}
\title{plotArrowText}
\description{Add arrows and text to a plot }
\usage{plotArrowText(labels, X = NULL, Y = NULL, x = NULL, y = NULL, 
    adj = c(0, 0), arrow.pos = 1, col = "black", cex = 1, border = TRUE, 
    border.col = "grey", border.lwd = 1, fill = "white", offset = c(0, 
        0), arrow.lwd = 1, arrow.angle = 30, arrow.length = 0.1, 
    arrow.col = "black")}
\arguments{
  \item{labels}{
}
  \item{X}{X,Y = position of the label and the tail of the arrow}
  \item{Y}{
}
  \item{x}{position of the arrowhead; if NULL, then no arrows are drawn}
  \item{y}{
}
  \item{adj}{which point to use in positioning the label, defaults to left/bottom}
  \item{arrow.pos}{the side from which the arrow tail emmanates, defaults to bottom (1), numbers go around clockwise: 1-4}
  \item{col}{
}
  \item{cex}{
}
  \item{border}{
}
  \item{border.col}{
}
  \item{border.lwd}{
}
  \item{fill}{
}
  \item{offset}{
}
  \item{arrow.lwd}{
}
  \item{arrow.angle}{
}
  \item{arrow.length}{
}
  \item{arrow.col}{
}
}
\details{Annotate plots easily with a horizontal
bit of text (a phrase) with optional filled rectangular box, and an
optional arrow to a real data point (or area) on the plot, extending
from the center of the text phrase.

Either of the X/x can contain a list with x/X y/Y components, in which
case the y/Y arguments are ignored.

If X is not given, then the user has to locate points on the plot, arrow and text in turn

Code was adopted from package heR.Misc.R}


\author{Thomas Wutzler}




\examples{
if( FALSE ){
    plot(sin(1:10),type="l")
    cat("\nClick on the plot four times to place labels and arrowheads (2xtext and 2xanchor). \n")
    tmp <- plotArrowText(c("Here is one label.","And another."),X=locator(2),
            x=locator(2),cex=c(1.4,3),arrow.lwd=c(2,4),arrow.length=c(0.1,0.25),
            border.lwd=c(1,3), fill=c("pink","white"), col=c("blue","green"),
            adj=c(0,1),border.col=c("orange","purple"))
}
}
