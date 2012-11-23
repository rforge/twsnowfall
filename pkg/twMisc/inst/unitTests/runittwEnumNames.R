.example.twEnumNames <- function(){
    vec <- c(a="foo", b="bar", "some test"="fasel")
    rm( list=names(vec) )
    i_b <- 4
    myFun <- function(inherits=TRUE){
        #do some fast repeated access to vec
        twEnumNames(vec, inherits)
        vec[i_some.test]
        vec[i_b]
    }
    checkException(myFun()) #existence of i_b
    res <- NULL
    #mtrace(twEnumNames)
    res <- myFun(inherits=FALSE)    # i_b does not exist in myFun
    checkEquals(vec["b"], res )
    checkTrue( !exists("a"))        # twEnumNames was called within myFun, so the variables went out of scope
}