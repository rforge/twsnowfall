userError <- function(
        ### Generates an error of class userError
         message             ##<< error msg 
        , class=character(0) ##<< character vector: the classes hyrarchy below class userError. First entry is the most specialized 
        , call=NULL          ##<< call expression.
){
    ##seealso<< \link{twMisc}
    
    ##details<< 
    ## Creates an error of subclass of error.
    ## This class can be used in \code{\link{tryCatch}} 
    ## to handle different error sources
    classList <- c(class, "userError", "error", "condition")
    structure(list(message = as.character(message), call = call), 
            class = classList)
    ### error of class c(class,"userError","error")
}
attr(userError,"ex") <- function(){
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
