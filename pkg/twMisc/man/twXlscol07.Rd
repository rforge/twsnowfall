\name{twXlscol07}
\alias{twXlscol07}
\title{twXlscol07}
\description{Returns 10 Colours from Excel2007 are quite colour-blind safe}
\usage{twXlscol07()}
\details{\describe{\item{Providing standard colors functionality}{ \itemize{
\item \code{twXlscol07} (this function): Returns 10 Colours from Excel2007 are quite colour-blind safe
\item \code{\link{twXlscol}}: Returns a list of colours corresponding to Excel, that are well distinguishable 
\item \code{\link{twContCols16}}: Return a character vector of green to magenta colorblind safe continuos scale 
\item \code{\link{twLtys}}: Returns a character vector line types, that are well distinguishable.
\item \code{\link{fColConv}}: Encode colors to avoid symbol font, which gives problems in pdf-Output 
}}}}


\author{Thomas Wutzler}



\seealso{\code{\link{twMisc}}}
\examples{
barplot(structure(rep(1,10),names=names(twXlscol07())),col=unlist(twXlscol07()), horiz=TRUE, las=1 )
#twBlindCol <- list(blue="#0072B2",orange="#D55E00",yellow="#F0E442",lightblue="#56B4E9",lightorange="#E69F00"
#,pink="#CC79A7",green="#2B9F78",black="#000000")
#barplot(structure(rep(1,8),names=names(twBlindCol)),col=unlist(twBlindCol), horiz=TRUE, las=1 )
}
