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
  \item{mergeMethod}{method of merging\describe{
\item{random}{sequences are merged randomly}
\item{stack}{stack sequences}
\item{slice}{slices of the sequences occur distributed across resulting sequence}
}}
  \item{nInSlice}{length of a slice for mergeMethod slice}
}

\value{integer vector of length (sum(n)), which each position specifying the index of the originating sequence }

\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
nItem = c(3,5,8)
twMergeSequences(nItem, "stack")
twMergeSequences(nItem)
twMergeSequences(nItem, "slice")
}
