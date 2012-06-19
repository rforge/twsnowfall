\name{twSnowfall-package}
\alias{twSnowfall-package}
\title{tw snowfall extension}\alias{twSnowfall}
\description{snowfall extension for \enumerate{\item extended load balancing and \item dependencies of arguments to remote function on former results, and \item Simplifying debugging of remote errors.}%enumerateMost important function of the package is \code{\link{sfRemoteWrapper}}, to trace errors in a remote process.Further functionality of the package includes: \itemize{\item{ providing more than one argument to load balanced apply: \code{\link{sfFArgsApplyLB}}  }\item{ providing result of a previous call to FUN in load balancing: \code{\link{sfFArgsApplyDep}} }\item{ simplifying the list of results of \code{\link{sfClusterApply}} similar to \code{\link{apply}}: \code{\link{sfSimplifyLBResult}}  }\item{ load balanced parallel application of FUN to rows or columns of matrix X: \code{\link{sfApplyMatrixLB}}  }\item{ executing a list of functions distributed across cpus via sfApply: \code{\link{sfPar}}  }}%itemize}%description

\author{Thomas Wutzler}

\keyword{ package }


