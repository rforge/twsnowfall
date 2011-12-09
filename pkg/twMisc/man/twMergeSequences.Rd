\name{twMergeSequences}
\alias{twMergeSequences}
\title{twMergeSequences}
\description{merge several sequences to a single sequence}
\usage{
twMergeSequences(nItem,
mergeMethod = c(random = "random",
stack = "stack",
slice = "slice"),
nInSlice = 4)
}
\arguments{
  \item{nItem}{integer vector (nSeq) number of items in each sequence}
  \item{mergeMethod}{method of merging}
  \item{nInSlice}{length of a slice for mergeMethod slice}
}



\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
nItem = c(3,5,8)
twMergeSequences(nItem, "stack")
twMergeSequences(nItem)
twMergeSequences(nItem, "slice")
}
