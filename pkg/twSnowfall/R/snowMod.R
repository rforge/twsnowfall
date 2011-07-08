newSOCKnode <- function (
	### adapted from snow 0.3-3, modified to work with pahts that include white spaces
	machine = "localhost", ..., options = defaultClusterOptions) 
{
	options <- addClusterOptions(options, list(...))
	if (is.list(machine)) {
		options <- addClusterOptions(options, machine)
		machine <- machine$host
	}
	outfile <- getClusterOption("outfile", options)
	if (machine == "localhost") 
		master <- "localhost"
	else master <- getClusterOption("master", options)
	port <- getClusterOption("port", options)
	manual <- getClusterOption("manual", options)
	homogeneous <- getClusterOption("homogeneous", options)
	if (getClusterOption("useRscript", options)) {
		if (homogeneous) {
			# twutz: changed to use quotes around rscript and snowlib 
			# in order to work with Path that include white spaces
			quot <- function(x) paste('\"',x,'\"',sep="")
			rscript <- quot(getClusterOption("rscript", options))
			snowlib <- quot(getClusterOption("snowlib", options))
			script <- file.path(snowlib, "snow", "RSOCKnode.R")
			env <- paste("MASTER=", master, " PORT=", port," OUT=", outfile, " SNOWLIB=", snowlib, sep = "")
			cmd <- paste(rscript, script, env)
		}
		else {
			script <- "RunSnowWorker RSOCKnode.R"
			env <- paste("MASTER=", master, " PORT=", port, 
				" OUT=", outfile, sep = "")
			cmd <- paste(script, env)
		}
	}
	else {
		if (homogeneous) {
			scriptdir <- getClusterOption("scriptdir", options)
			script <- file.path(scriptdir, "RSOCKnode.sh")
			rlibs <- paste(getClusterOption("rlibs", options), 
				collapse = ":")
			rprog <- getClusterOption("rprog", options)
			env <- paste("MASTER=", master, " PORT=", port, 
				" OUT=", outfile, " RPROG=", rprog, " R_LIBS=", 
				rlibs, sep = "")
		}
		else {
			script <- "RunSnowNode RSOCKnode.sh"
			env <- paste("MASTER=", master, " PORT=", port, 
				" OUT=", outfile, sep = "")
		}
		cmd <- paste("env", env, script)
	}
	if (manual) {
		cat("Manually start worker on", machine, "with\n    ", 
			cmd, "\n")
		flush.console()
	}
	else {
		if (machine != "localhost") {
			rshcmd <- getClusterOption("rshcmd", options)
			user <- getClusterOption("user", options)
			cmd <- paste(rshcmd, "-l", user, machine, cmd)
		}
		if (.Platform$OS.type == "windows") {
			system(cmd, wait = FALSE, input = "")
		}
		else system(cmd, wait = FALSE)
	}
	timeout <- getClusterOption("timeout")
	old <- options(timeout = timeout)
	on.exit(options(old))
	con <- socketConnection(port = port, server = TRUE, blocking = TRUE, 
		open = "a+b")
	structure(list(con = con, host = machine), class = "SOCKnode")
}
#mtrace(newSocknode)

