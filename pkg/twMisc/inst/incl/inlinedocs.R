`inlinedocExample<-` <- function (
    ### Attaching example code to attribute 'ex'.
    f           ##<< the function, to which to attach code for section 'examples' in documentation
    , value     ##<< the example code, usually the body of a function (see example)
) {
    # see parsers.R examples.in.attr
    attr (f, "ex") <- value
    f
}
inlinedocExample(`inlinedocExample<-`) <- function(){
    ### Simple Hello-World function.
    helloWorld <- function(){ cat('Hello World!\n')}
    inlinedocExample(helloWorld) <- function(){
        # all text including comments inside this block 
        # will go to the examples section of 
        # function helloWorld
        helloWorld()    # prints Hello World
    }   
}
