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

test.unnamedItemsWithCid.yml <- function (){
	cfg1 <- loadConfig(c(file.path(unitDir,"unnamedItemsWithCid.yml") ))	# throw one warning
	checkEquals(3, length(getv(cfg1,"sequence1")) )			# first entry out of 4, the properties map has been stripped
	#str3(getv(cfg1))
	checkEquals("refContents",getCid(cfg1,"refItem"))
	cfg2 <- loadConfig(c(file.path(unitDir,"unnamedItemsWithCid.yml"),file.path(unitDir,"unnamedItemsWithCid2.yml") ))	# throw two warnings, cid now gone
	checkTrue( is.null(getCid(cfg2,"refItem")) )	# also throw warning
} 


#testCases or vignette
# simple key value pairs
# self documentation
# nested structures (accessing trees and elements, also of documentation)
# backtick substitution
# referencing items by in tree, by name (future imports) 
# cascading demonstrating changing docu, value and both
#    referencing item in another config file loaded afterwards

