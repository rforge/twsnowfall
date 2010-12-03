npreg.rbandwidth <- function (bws, txdat = stop("training data 'txdat' missing"), 
	tydat = stop("training data 'tydat' missing"), exdat, eydat, 
	gradients = FALSE, residuals = FALSE, ...) 
{
	no.ex = missing(exdat)
	no.ey = missing(eydat)
	txdat = toFrame(txdat)
	if (!(is.vector(tydat) | is.factor(tydat))) 
		stop("'tydat' must be a vector or a factor")
	if (!no.ex) {
		exdat = toFrame(exdat)
		if (!txdat %~% exdat) 
			stop("'txdat' and 'exdat' are not similar data frames!")
		if (!no.ey) {
			if (!(is.vector(eydat) | is.factor(eydat))) 
				stop("'eydat' must be a vector or a factor")
			if (dim(exdat)[1] != length(eydat)) 
				stop("number of evaluation data 'exdat' and dependent data 'eydat' do not match")
			if (!identical(coarseclass(eydat), coarseclass(tydat))) 
				stop("type of evaluation data 'eydat' does not match that of 'tydat'")
		}
	}
	else if (!no.ey) {
		if (dim(txdat)[1] != length(eydat)) 
			stop("number of training data 'txdat' and dependent data 'eydat' do not match")
	}
	if (length(bws$bw) != length(txdat)) 
		stop("length of bandwidth vector does not match number of columns of 'txdat'")
	ccon = unlist(lapply(txdat[, bws$icon, drop = FALSE], class))
	if ((any(bws$icon) && !all((ccon == class(integer(0))) | 
					(ccon == class(numeric(0))))) || (any(bws$iord) && !all(unlist(lapply(txdat[, 
							bws$iord, drop = FALSE], class)) == class(ordered(0)))) || 
		(any(bws$iuno) && !all(unlist(lapply(txdat[, bws$iuno, 
							drop = FALSE], class)) == class(factor(0))))) 
		stop("supplied bandwidths do not match 'txdat' in type")
	if (dim(txdat)[1] != length(tydat)) 
		stop("number of explanatory data 'txdat' and dependent data 'tydat' do not match")
	goodrows = 1:dim(txdat)[1]
	rows.omit = attr(na.omit(data.frame(txdat, tydat)), "na.action")
	goodrows[rows.omit] = 0
	if (all(goodrows == 0)) 
		stop("Training data has no rows without NAs")
	txdat = txdat[goodrows, , drop = FALSE]
	tydat = tydat[goodrows]
	if (!no.ex) {
		goodrows = 1:dim(exdat)[1]
		rows.omit = eval(parse(text = paste("attr(na.omit(data.frame(exdat", 
					ifelse(no.ey, "", ",eydat"), ")), \"na.action\")")))
		goodrows[rows.omit] = 0
		exdat = exdat[goodrows, , drop = FALSE]
		if (!no.ey) 
			eydat = eydat[goodrows]
		if (all(goodrows == 0)) 
			stop("Evaluation data has no rows without NAs")
	}
	if (residuals) {
		resid <- tydat - npreg(txdat = txdat, tydat = tydat, 
			bws = bws)$mean
	}
	tnrow = dim(txdat)[1]
	enrow = ifelse(no.ex, tnrow, dim(exdat)[1])
	ncol = dim(txdat)[2]
	if (is.factor(tydat)) {
		tydat <- adjustLevels(data.frame(tydat), bws$ydati)[, 
			1]
		tydat <- (bws$ydati$all.dlev[[1]])[as.integer(tydat)]
	}
	else tydat <- as.double(tydat)
	if (no.ey) 
		eydat <- double()
	else {
		if (is.factor(eydat)) {
			eydat <- adjustLevels(data.frame(eydat), bws$ydati, 
				allowNewCells = TRUE)
			eydat <- toMatrix(eydat)[, 1]
		}
		else eydat <- as.double(eydat)
	}
	txdat <- adjustLevels(txdat, bws$xdati)
	if (!no.ex) 
		exdat <- adjustLevels(exdat, bws$xdati, allowNewCells = TRUE)
	if (no.ex) 
		teval <- txdat
	else teval <- exdat
	txdat = toMatrix(txdat)
	tuno = txdat[, bws$iuno, drop = FALSE]
	tcon = txdat[, bws$icon, drop = FALSE]
	tord = txdat[, bws$iord, drop = FALSE]
	if (!no.ex) {
		exdat = toMatrix(exdat)
		euno = exdat[, bws$iuno, drop = FALSE]
		econ = exdat[, bws$icon, drop = FALSE]
		eord = exdat[, bws$iord, drop = FALSE]
	}
	else {
		euno = data.frame()
		eord = data.frame()
		econ = data.frame()
	}
	myopti = list(num_obs_train = tnrow, num_obs_eval = enrow, 
		num_uno = bws$nuno, num_ord = bws$nord, num_con = bws$ncon, 
		int_LARGE_SF = ifelse(bws$scaling, SF_NORMAL, SF_ARB), 
		BANDWIDTH_reg_extern = switch(bws$type, fixed = BW_FIXED, 
			generalized_nn = BW_GEN_NN, adaptive_nn = BW_ADAP_NN), 
		int_MINIMIZE_IO = ifelse(options("np.messages"), IO_MIN_FALSE, 
			IO_MIN_TRUE), kerneval = switch(bws$ckertype, gaussian = CKER_GAUSS + 
				bws$ckerorder/2 - 1, epanechnikov = CKER_EPAN + bws$ckerorder/2 - 
				1, uniform = CKER_UNI), ukerneval = switch(bws$ukertype, 
			aitchisonaitken = UKER_AIT, liracine = UKER_LR), 
		okerneval = switch(bws$okertype, wangvanryzin = OKER_WANG, 
			liracine = OKER_LR), ey_is_ty = no.ey, do_grad = gradients, 
		regtype = switch(bws$regtype, lc = REGTYPE_LC, ll = REGTYPE_LL), 
		no.ex = no.ex, mcv.numRow = attr(bws$xmcv, "num.row"))
	myout = .C("np_regression", as.double(tuno), as.double(tord), 
		as.double(tcon), as.double(tydat), as.double(euno), as.double(eord), 
		as.double(econ), as.double(eydat), as.double(c(bws$bw[bws$icon], 
				bws$bw[bws$iuno], bws$bw[bws$iord])), as.double(bws$xmcv), 
		as.double(attr(bws$xmcv, "pad.num")), as.integer(myopti), 
		mean = double(enrow), merr = double(enrow), g = double(ifelse(gradients, 
				enrow * ncol, 0)), gerr = double(ifelse(gradients, 
				enrow * ncol, 0)), xtra = double(6), PACKAGE = "np")[c("mean", 
			"merr", "g", "gerr", "xtra")]
	if (gradients) {
		myout$g = matrix(data = myout$g, nrow = enrow, ncol = ncol, 
			byrow = FALSE)
		rorder = numeric(ncol)
		rorder[c((1:ncol)[bws$icon], (1:ncol)[bws$iuno], (1:ncol)[bws$iord])] = 1:ncol
		myout$g = as.matrix(myout$g[, rorder])
		myout$gerr = matrix(data = myout$gerr, nrow = enrow, 
			ncol = ncol, byrow = FALSE)
		myout$gerr = as.matrix(myout$gerr[, rorder])
	}
	ev <- eval(parse(text = paste("npregression(bws = bws,", 
				"eval = teval,", "mean = myout$mean, merr = myout$merr,", 
				ifelse(gradients, "grad = myout$g, gerr = myout$gerr,", 
					""), ifelse(residuals, "resid = resid,", ""), "ntrain = tnrow,", 
				"trainiseval = no.ex,", "gradients = gradients,", "residuals = residuals,", 
				"xtra = myout$xtra, rows.omit = rows.omit)")))
	ev$call <- match.call(expand.dots = FALSE)
	environment(ev$call) <- parent.frame()
	return(ev)
}

npregbw.default <- function (xdat = stop("invoked without data 'xdat'"), ydat = stop("invoked without data 'ydat'"), 
	bws, bandwidth.compute = TRUE, nmulti, remin, itmax, ftol, 
	tol, small, regtype, bwmethod, bwscaling, bwtype, ckertype, 
	ckerorder, ukertype, okertype, ...) 
{
	xdat <- toFrame(xdat)
	if (!(is.vector(ydat) | is.factor(ydat))) 
		stop("'ydat' must be a vector")
	mc.names <- names(match.call(expand.dots = FALSE))
	margs <- c("regtype", "bwmethod", "bwscaling", "bwtype", 
		"ckertype", "ckerorder", "ukertype", "okertype")
	m <- match(margs, mc.names, nomatch = 0)
	any.m <- any(m != 0)
	tbw <- eval(parse(text = paste("rbandwidth(bws", ifelse(any.m, 
					",", ""), paste(mc.names[m], ifelse(any.m, "=", ""), 
					mc.names[m], collapse = ", "), ", nobs = dim(xdat)[1],", 
				"xdati = untangle(xdat),", "ydati = untangle(data.frame(ydat)),", 
				"xnames = names(xdat),", "ynames = deparse(substitute(ydat)),", 
				"bandwidth.compute = bandwidth.compute)")))
	mc.names <- names(match.call(expand.dots = FALSE))
	margs <- c("bandwidth.compute", "nmulti", "remin", "itmax", 
		"ftol", "tol", "small")
	m <- match(margs, mc.names, nomatch = 0)
	any.m <- any(m != 0)
	tbw <- eval(parse(text = paste("npregbw.rbandwidth(xdat=xdat, ydat=ydat, bws=tbw", 
				ifelse(any.m, ",", ""), paste(mc.names[m], ifelse(any.m, 
						"=", ""), mc.names[m], collapse = ", "), ")")))
	mc <- match.call(expand.dots = FALSE)
	environment(mc) <- parent.frame()
	tbw$call <- mc
	return(tbw)
}

npregbw.rbandwidth <- function (xdat = stop("invoked without data 'xdat'"), ydat = stop("invoked without data 'ydat'"), 
	bws, bandwidth.compute = TRUE, nmulti, remin = TRUE, itmax = 10000, 
	ftol = 1.19209e-07, tol = 1.49012e-08, small = 2.22045e-16, 
	...) 
{
	xdat <- toFrame(xdat)
	if (missing(nmulti)) {
		nmulti <- min(5, dim(xdat)[2])
	}
	if (!(is.vector(ydat) | is.factor(ydat))) 
		stop("'ydat' must be a vector")
	if (length(bws$bw) != dim(xdat)[2]) 
		stop("length of bandwidth vector does not match number of columns of 'xdat'")
	ccon = unlist(lapply(xdat[, bws$icon, drop = FALSE], class))
	if ((any(bws$icon) && !all((ccon == class(integer(0))) | 
					(ccon == class(numeric(0))))) || (any(bws$iord) && !all(unlist(lapply(xdat[, 
							bws$iord, drop = FALSE], class)) == class(ordered(0)))) || 
		(any(bws$iuno) && !all(unlist(lapply(xdat[, bws$iuno, 
							drop = FALSE], class)) == class(factor(0))))) 
		stop("supplied bandwidths do not match 'xdat' in type")
	if (dim(xdat)[1] != length(ydat)) 
		stop("number of regression data and response data do not match")
	goodrows = 1:dim(xdat)[1]
	rows.omit = attr(na.omit(data.frame(xdat, ydat)), "na.action")
	goodrows[rows.omit] = 0
	if (all(goodrows == 0)) 
		stop("Data has no rows without NAs")
	xdat = xdat[goodrows, , drop = FALSE]
	ydat = ydat[goodrows]
	nrow = dim(xdat)[1]
	ncol = dim(xdat)[2]
	if (is.factor(ydat)) 
		ydat <- dlev(ydat)[as.integer(ydat)]
	else ydat <- as.double(ydat)
	xdat = toMatrix(xdat)
	runo = xdat[, bws$iuno, drop = FALSE]
	rcon = xdat[, bws$icon, drop = FALSE]
	rord = xdat[, bws$iord, drop = FALSE]
	tbw <- bws
	if (bandwidth.compute) {
		myopti = list(num_obs_train = dim(xdat)[1], iMultistart = ifelse(nmulti == 
					0, IMULTI_FALSE, IMULTI_TRUE), iNum_Multistart = nmulti, 
			int_use_starting_values = ifelse(all(bws$bw == 0), 
				USE_START_NO, USE_START_YES), int_LARGE_SF = ifelse(bws$scaling, 
				SF_NORMAL, SF_ARB), BANDWIDTH_reg_extern = switch(bws$type, 
				fixed = BW_FIXED, generalized_nn = BW_GEN_NN, 
				adaptive_nn = BW_ADAP_NN), itmax = itmax, int_RESTART_FROM_MIN = ifelse(remin, 
				RE_MIN_TRUE, RE_MIN_FALSE), int_MINIMIZE_IO = ifelse(options("np.messages"), 
				IO_MIN_FALSE, IO_MIN_TRUE), bwmethod = switch(bws$method, 
				cv.aic = BWM_CVAIC, cv.ls = BWM_CVLS), kerneval = switch(bws$ckertype, 
				gaussian = CKER_GAUSS + bws$ckerorder/2 - 1, 
				epanechnikov = CKER_EPAN + bws$ckerorder/2 - 
					1, uniform = CKER_UNI), ukerneval = switch(bws$ukertype, 
				aitchisonaitken = UKER_AIT, liracine = UKER_LR), 
			okerneval = switch(bws$okertype, wangvanryzin = OKER_WANG, 
				liracine = OKER_LR), nuno = bws$nuno, nord = bws$nord, 
			ncon = bws$ncon, regtype = switch(bws$regtype, lc = REGTYPE_LC, 
				ll = REGTYPE_LL))
		myoptd = list(ftol = ftol, tol = tol, small = small)
		myout = .C("np_regression_bw", as.double(runo), as.double(rord), 
			as.double(rcon), as.double(ydat), as.integer(myopti), 
			as.double(myoptd), bw = c(bws$bw[bws$icon], bws$bw[bws$iuno], 
				bws$bw[bws$iord]), fval = double(2), PACKAGE = "np")[c("bw", 
				"fval")]
		rorder = numeric(ncol)
		rorder[c((1:ncol)[bws$icon], (1:ncol)[bws$iuno], (1:ncol)[bws$iord])] = 1:ncol
		tbw$bw <- myout$bw[rorder]
		tbw$fval <- myout$fval[1]
		tbw$ifval <- myout$fval[2]
	}
	tbw$sfactor <- tbw$bandwidth <- tbw$bw
	nfactor <- nrow^(-2/(2 * tbw$ckerorder + tbw$ncon))
	if (tbw$nuno > 0) {
		if (tbw$scaling) {
			tbw$bandwidth[tbw$xdati$iuno] <- tbw$bandwidth[tbw$xdati$iuno] * 
				nfactor
		}
		else {
			tbw$sfactor[tbw$xdati$iuno] <- tbw$sfactor[tbw$xdati$iuno]/nfactor
		}
	}
	if (tbw$nord > 0) {
		if (tbw$scaling) {
			tbw$bandwidth[tbw$xdati$iord] <- tbw$bandwidth[tbw$xdati$iord] * 
				nfactor
		}
		else {
			tbw$sfactor[tbw$xdati$iord] <- tbw$sfactor[tbw$xdati$iord]/nfactor
		}
	}
	if (tbw$ncon > 0) {
		dfactor <- EssDee(rcon) * nrow^(-1/(2 * tbw$ckerorder + 
					tbw$ncon))
		if (tbw$scaling) {
			tbw$bandwidth[tbw$xdati$icon] <- tbw$bandwidth[tbw$xdati$icon] * 
				dfactor
		}
		else {
			tbw$sfactor[tbw$xdati$icon] <- tbw$sfactor[tbw$xdati$icon]/dfactor
		}
	}
	tbw <- rbandwidth(bw = tbw$bw, regtype = tbw$regtype, bwmethod = tbw$method, 
		bwscaling = tbw$scaling, bwtype = tbw$type, ckertype = tbw$ckertype, 
		ckerorder = tbw$ckerorder, ukertype = tbw$ukertype, okertype = tbw$okertype, 
		fval = tbw$fval, ifval = tbw$ifval, nobs = tbw$nobs, 
		xdati = tbw$xdati, ydati = tbw$ydati, xnames = tbw$xnames, 
		ynames = tbw$ynames, sfactor = tbw$sfactor, bandwidth = tbw$bandwidth, 
		rows.omit = rows.omit, bandwidth.compute = bandwidth.compute)
	tbw
}


