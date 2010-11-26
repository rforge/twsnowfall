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
