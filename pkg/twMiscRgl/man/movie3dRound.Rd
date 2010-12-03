\name{movie3dRound}
\alias{movie3dRound}
\title{movie3dRound}
\description{Generating a movie of a full round}
\usage{
movie3dRound(movie = "movie", frameTime = 1, duration = frameTime * 12, dir = tempdir(), ...)
}
\arguments{
  \item{movie}{
}
  \item{frameTime}{number of seconds for displying a frame}
  \item{duration}{number of seconds for the full rotation}
  \item{dir}{A directory in which to create temporary files for each frame of the movie}
  \item{\dots}{further arguments to \code{\link{movie3d}}}
}
\details{The default parameterization provides 12 views, in analogy of the clock

In contrast to play3dRound this methods does not set standard position. 
See examples how to do this.}
\value{The directory where the movie was generated 	}

\author{Thomas Wutzler <twutz@bgc-jena.mpg.de>}





