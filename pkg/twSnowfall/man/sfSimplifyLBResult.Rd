\name{sfSimplifyLBResult}
\alias{sfSimplifyLBResult}
\title{sfSimplifyLBResult}
\description{Transform the list resulting from \code{\link{sfClusterApplyLB}} to vector or matrix, similar as apply. }
\usage{sfSimplifyLBResult(resl, caseNames = NULL)}
\arguments{
  \item{resl}{
}
  \item{caseNames}{list of column names, name corresponds to dimension name for columns, i.e as obtained from dimnames(X)[1]}
}

\value{Depending on components (result of FUN) \describe{
\item{scalar}{vector}
\item{vector}{matrix with cases in columns}
\item{matrix}{each column corresponds to as.vector(matrix), i.e. stacked to one vector }
\item{list}{list of lists. May transform to dataframe by: do.call(rbind, lapply(.res,data.frame)))}
\item{data.frame}{list of data.frames}}}

\author{Thomas Wutzler}



\seealso{\code{\link{twSnowfall}}}

