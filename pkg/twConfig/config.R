# test R configuration file, to be parsed in R/config.R 

msg <- "Hello World"

testList <- structure( list(
	scalarItem = structure(1
		,desc='subItem with cid', cid='subItem1')
	,vectorItem = 1:3
	,subList = structure( list( vectorItem = 1:5 )
		,desc='subList with cid', cid='subList1')
)
,desc='top list item')

f1 = structure( function(loca){ c(loca, testList$vectorItem*2) }
,desc='function accessing open binding loca and variables in local frame.')

f2 = structure( function(){ cid('subList1')$vectorItem }
,desc='function accessing function cid to return an element given by an id.')
