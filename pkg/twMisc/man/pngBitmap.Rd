\name{pngBitmap}
\alias{pngBitmap}
\title{pngBitmap}
\description{savely opening a png Device}
\usage{pngBitmap(fileName, ...)}
\arguments{
  \item{fileName}{string scalar: the output file name}
  \item{\dots}{further arguments to png or bitmap}
}
\details{On unix systems without X11 \code{\link{png}} using a graphics window fails, 
while on Windows systems \code{\link{bitmap}} using possibly missing ghostscript fails.

\code{pngBitmap} tries both approaches.
}


\author{Thomas Wutzler}




\examples{
if( FALSE ){    # do not execute on INSTALL
    pngBitmap("test1.png")
    plot(1:5,5:1)
    dev.off()
}
}
