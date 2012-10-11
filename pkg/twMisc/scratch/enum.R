vec <- c(a="foo", b="bar")
nms <- names(vec)
nms <- make.names(nms)
for( i in seq(along.with=vec) ){
	if( exists(nms[i]) )
		stop("tried to override existing variable")
	assign( nms[i], i  )
}


a <- 1
#b <- function(){ a<-5 }
#b <- function(){ exists("a",inherits=FALSE); }
b <- function(){ assign("a",a+1,pos=parent.frame() ); a }
b()
a

exists("ls", 2) # true even though ls is in pos=3


