\name{userError}
\alias{userError}
\title{userError}
\description{Generates an error of class userError}
\usage{userError(message, class = character(0), call = NULL)}
\arguments{
  \item{message}{error msg}
  \item{class}{character vector: the classes hyrarchy below class userError. First entry is the most specialized}
  \item{call}{call expression.}
}
\details{Creates an error of subclass of error.
This class can be used in \code{\link{tryCatch}} 
to handle different error sources}
\value{error of class c(class,"userError","error")}

\author{Thomas Wutzler}



\seealso{\link{twMisc}}
\examples{
res <- tryCatch({
		#stop( userError("My error message") )
		stop( userError(class="argsError", "My error message") )
	}
	,argsError=function(e){ 
		cat("Handle my very specialized error of class argsError/userError/error\n"); 
		print(e)
	}
	,userError=function(e){ 
		cat("Handle my specialized errors of class userError/error\n"); 
		print(e)
	}
	,error=function(e){
		cat("Handle all the other errors\n")
		print(e)
	}
	,finally=function(){
		cat("Finally clear up.")
	}
) # end of tryCatch
}
