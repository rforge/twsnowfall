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

