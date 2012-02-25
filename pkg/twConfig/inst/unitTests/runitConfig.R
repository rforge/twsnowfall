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
	cfg <- new(Config)
	
} 

