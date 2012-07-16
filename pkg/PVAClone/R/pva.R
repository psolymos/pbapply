## here the magic happens
pva <- 
function(x, model, n.clones, ...)
{
    if (all(n.clones <= 1))
        stop("data cloning is required, set n.clones properly")
    xx <- x[!is.na(x)] # do not use NA in checks
    if (sum(is.na(x)))
        warning("missing values found in data")
    if (any(xx<0))
        stop("negative values not allowed")
    if (abs(sum(xx) - sum(as.integer(xx))) > 10^-6)
        warning("non-integer values found in data")
    if (is.character(model))
        model <- eval(as.name(model))
    if (is.function(model))
        model <- model()
    dcf <- makeDcFit(
        data = list(T=length(x), kk = 1),
        params = model@params,
        unchanged = "T",
        multiply = "kk",
        model = model@model)
    if (model@obs.error == "none")
        dcf@data$x <- dcdim(data.matrix(log(x)))
    if (model@obs.error == "normal")
        dcf@data$y <- dcdim(data.matrix(log(x)))
    if (model@obs.error == "poisson")
        dcf@data$O <- dcdim(data.matrix(x))
    fit <- dcmle(dcf, n.clones=n.clones, 
        nobs=as.integer(sum(!is.na(x))), ...)
    fit0 <- as(model@backtransf(as.mcmc.list(fit)), "dcmle")
    ## summary (coef/fullcoef) and vcov is on original scale
    ## mcmc.list and diagnostics are on transformed scale
    s0 <- summary(fit0)@coef
    s <- matrix(NA, length(model@varnames), 4)
    rownames(s) <- model@varnames
    colnames(s) <- colnames(s0)
    for (i in model@varnames) {
        if (i %in% rownames(s0)) {
            s[i,] <- s0[i,]
        } else {
            s[i,1] <- model@fixed[i]
        }
    }
    fit@fullcoef <- s[,1]
    fit@coef <- coef(fit0)
    fit@vcov <- vcov(fit0)
    fit@details <- as(fit0, "dcCodaMCMC")
    fit <- new("pva",
        fit,
        summary = s,
        observations = x,
        model = model)
    fit@model@predmodel <- suppressWarnings(eval(call(model@growth.model, 
        obs.error=model@obs.error, fixed=coef(fit)))@model)
    fit@dcdata <- dcf
    fit@call <- match.call()
    fit
}
