## Test unit 'calcDEMCTemp'

.setUp <-function () {
	#print("Config .setup")
	unitVars <- list(
		unitDir = if( basename(getwd())=="twConfig") "inst/unitTests" else system.file("unitTests",package="twConfig")
	)
	attach(unitVars)
}

.tearDown <- function () {
	detach()
}


test.yaml1 <- function (){
	#print(unitDir)
} 

test.yaml_cidLabelChanged <- function (){
	checkException(cfg <- loadConfig(file.path(,"cidLabelChanged.yml")))	# default function name is cid
	suppressWarnings({ 
		cfg <- loadConfig(file.path(unitDir,"cidLabelChanged.yml")
			, cidFunctionName="ids") # issues a warning cid not found
		checkTrue( is.null(getCid(cfg,"refItem"))) # also issues a warning
		checkEquals( "refItem=", getv(cfg,"referencingItem")[[2]] )	# no warning, because substitution is done at load time
	})
	cfg <- loadConfig(file.path(unitDir,"cidLabelChanged.yml")
		, cidFunctionName="ids"
		, cidLabel="idc"
		, descLabel="help"
	) # issues a warning cid not found
	checkEquals("refContents",getCid(cfg,"refItem"))
	checkEquals("refItem=refContents",getv(cfg,"referencingItem"))
	#
	checkEquals( 2, length(getDesc(cfg) ))
	checkEquals( 2, length(getDesc(cfg,"ref") ))
	checkEquals( 0, length(getDesc(cfg,"Item") ))
	checkEquals( 2, length(getDesc(cfg,pattern="Item") ))
	checkEquals( 1, length(getDesc(cfg,"referencingItem") ))
} 

test.unnamedItemsWithCid <- function (){
	cfg1 <- loadConfig(c(file.path(unitDir,"unnamedItemsWithCid.yml") ))	# throw one warning
	checkEquals(3, length(getv(cfg1,"sequence1")) )			# first entry out of 4, the properties map has been stripped
	checkEquals( 3, length(getDesc(cfg1)) )		# including description of the subItem
	checkEquals( 2, length(getDesc(cfg1,"sequence1")) )		# including description of the subItem
	#str3(getv(cfg1))
	checkEquals("refContents",getCid(cfg1,"refItem"))
	cfg2 <- loadConfig(c(file.path(unitDir,"unnamedItemsWithCid.yml"),file.path(unitDir,"unnamedItemsWithCid2.yml") ))	# throw two warnings, cid now gone
	checkEquals( 1, length(getDesc(cfg2,"sequence1")) )		# descritpion of the top item is still there
	checkTrue( is.null(getCid(cfg2,"refItem")) )	# also throw warning and return NULL: refItem not there any more
}

test.copySubConfig <- function (){
	cfg1 <- loadConfig(c(file.path(unitDir,"copySubConfig.yml") ))
	checkEquals( "subEntry1_value", getCid(cfg1,"subEntry1") )
	checkEquals( "subEntry1=subEntry1_value", getv(cfg1,"list1$referencingItem") )
	#object <- cfg1; path="list1"
	#
	cfg2 <- copySubConfig(cfg1)		# makes a copy	
	checkEquals( getv(cfg1), getv(cfg2) )
	cfg2@env$newVar <- "newVar"
	checkEquals( getv(cfg2,"newVar"), "newVar" )
	checkException( getv(cfg1,"newVar") )		# throws and error: indeed distinct environments
	#
	cfg2 <- copySubConfig(cfg1,"nonExisting")	
	checkEquals(names(getv(cfg2)),"cid")		# empty unless the cid function
	#
	#trace("copySubConfig", recover, signature="twConfig")
	cfg2 <- copySubConfig(cfg1,"list1")
	tmp <- getv(cfg2)
	checkTrue( all( names(getv(cfg1)$list1) %in% names(getv(cfg2)) ) )
	checkEquals( "subEntry1_value", getCid(cfg2,"subEntry1") )
	checkEquals( "subEntry1=subEntry1_value", getv(cfg2,"referencingItem"))
} 



#testCases or vignette
# simple key value pairs
# self documentation
# nested structures (accessing trees and elements, also of documentation)
# backtick substitution
# referencing items by in tree, by name (future imports) 
# cascading demonstrating changing docu, value and both
#    referencing item in another config file loaded afterwards

