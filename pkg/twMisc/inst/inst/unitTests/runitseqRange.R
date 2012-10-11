.setUp <- function () {}
.tearDown <- function () {}

test.one <- function() {
	z <- rnorm(10)
	zGrid <- seqRange(range(z), length.out=11)
	checkEquals(range(z), range(zGrid))
	checkEquals(11, length(zGrid))
	checkEqualsNumeric( rep(diff(range(z))/10,10),  diff(zGrid) )
}

