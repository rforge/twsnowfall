msg <- "Hello World"

testList <- structure( list(
	scalarItem = 1
	,vectorItem = 1:3
	,subList = structure( list( vectorItem = 1:5 )
		,desc='subList with cid', cid='subList1')
)
,desc='top list item')

f1 = structure( function(){ c(loca, testList$vectorItem*2) }
,desc='function accessing open binding loca and variables in local frame.')

f2 = structure( function(){ cid('subList1')$vectorItem }
,desc='function accessing function cid to return an element given by an id.')
