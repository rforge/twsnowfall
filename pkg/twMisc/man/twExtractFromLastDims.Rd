\name{twExtractFromLastDims}
\alias{twExtractFromLastDims}
\title{twExtractFromLastDims}
\description{Extract slices i from array Aext keeping the all first dimensions.}
\usage{
twExtractFromLastDims(Aext,
i,
dPrev = c(1))
}
\arguments{
  \item{Aext}{array to extract from}
  \item{i}{indices in matrix of last dimensions}
  \item{dPrev}{dimensions in front to keep, defaults to rows}
}



\author{Thomas Wutzler}



\seealso{\link{twExtractDim}, \link{twMisc}}
\examples{
(A <- matrix(1:6, ncol=2))
(Aext <- abind( lapply(1:4, function(i){(i)/10+A}), along=0 ))
# Note that the second and third dimension of Aext correspond to A 

# Now we whish to extract from Aext based on 
# a criterion for the second and third dimension 
(B <- diag(3)[,1:2])
i <- which(B != 0)
(A[i])
# how to index Aext to obtain those indices?
resExp <- matrix(0,nrow=nrow(Aext),ncol=length(i) )
for( ii in seq(along.with=i) )
	resExp[,ii] <- Aext[,(i[ii]-1) \%\% 3+1,(i[ii]-1) \%/\% 3+1]
(res <- twExtractFromLastDims(Aext,i))
identical( resExp, structure(res, dimnames=NULL) )
}
