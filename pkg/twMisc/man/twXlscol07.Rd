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
}
