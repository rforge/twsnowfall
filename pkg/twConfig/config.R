# test R configuration file, to be parsed in R/config.R 

msg <- "Hello World"

testList <- list( list(desc='top list item')
	,list(
		vectorItem1 = 1:3
		,vectorItem2 = list( list(desc='subItem with cid', cid='subItem1')
			,1:3)
		,subList = list( list(desc='subList with cid', cid='subList1') 
				,list( 
					vectorItem = 1:5
					,subSubItem = "desc of subSubItem"
				))
			
	))

f1 = list( list(desc='function accessing open binding loca and variables in local frame. Note that us must use list(...)')
		,function(...){ with( list(...),{c(loca, testList$vectorItem2*2)}) })

f2 = list( list(desc='function accessing function cid to return an element given by an id.')
		,function(){ cid('subList1')$vectorItem })

