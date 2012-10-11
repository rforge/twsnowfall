.setUp <- function () {}
.tearDown <- function () {}

test.tmp.fTested <- function() {
	checkEquals(tw.fTestDummy(2,3), 6)
	checkEquals(tw.fTestDummy(50,2), 100)
	## check that an error is created for a bogus argument
	checkException(tw.fTestDummy("xx"))
}

test.errordemo <- function() {
	tmp.int <- function(){
		stop("this is just to show what an error looks like as opposed to a failure")
	}
	tmp.int()
}

test.failure <- function(){
	checkTrue(FALSE,"this is to show how failures look like.")
}

