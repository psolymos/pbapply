## this is the heart of model selection and
## profile likelihood
pva.llr <- 
function(null, alt, pred)
{
    if (!inherits(null, "pva"))
        stop("'null' must be of class 'pva'")
    if (!inherits(alt, "pva"))
        stop("'alt' must be of class 'pva'")
    err0 <- null@model@obs.error
    err1 <- alt@model@obs.error
    if (err0 != "none" && err1 == "none")
        stop("switch null and alternative model")
    if (identical(null@model@growth.model, alt@model@growth.model) && 
        (length(null@coef) > length(alt@coef)))
            warning("Hint: check what is null and alternative ",
                "if models are nested")
    obs <- null@observations
    if (!identical(obs, alt@observations))
        stop("data in null amd alternative model must be identical")
    data0 <- switch(err0,
        "none" = log(obs),
        "normal" = log(obs),
        "poisson" = obs)
    data1 <- switch(err1,
        "none" = log(obs),
        "normal" = log(obs),
        "poisson" = obs)
    ## use here parApply with parallel package -- future stuff
    logd0 <- apply(pred, 1, null@model@logdensity, 
        mle=coef(null), data=data0, alt_obserror=err1 != "none")
    logd1 <- apply(pred, 1, alt@model@logdensity, 
        mle=coef(alt), data=data1, alt_obserror=FALSE)
    ## log likelihood ratio
    log(mean(exp(logd0 - logd1)))
}

## model selection
model.select <- 
function(null, alt, B=10^4)
{
    ## generateLatent takes care of the non-obs.err and
    ## missing value cases
    pred <- suppressWarnings(generateLatent(alt, 
        n.chains=1, n.iter=B))
    llr <- pva.llr(null, alt, pred)
    ## neffective can be different (e.g. Ricer vs. Gompertz)
    n0 <- null@model@neffective(null@observations)
    n1 <- alt@model@neffective(null@observations)
    p0 <- null@model@p - length(null@model@fixed)
    p1 <- alt@model@p - length(alt@model@fixed)
    rval <- data.frame(log_LR = llr,
        delta_AIC = -2*llr + 2*(p0-p1),
        delta_BIC = -2*llr + log(n0)*p0 - log(n1)*p1,
        delta_AICc= -2*llr + 2*((p0-p1) + 
            p0*(p0+1)/(n0-p0-1) - p1*(p1+1)/(n0-p1-1)))
    attr(rval, "fancy") <- list(
        data=fancyPVAmodel(null, "", part=2),
        null=fancyPVAmodel(null, "", part=1),
        alt=fancyPVAmodel(alt,  "", part=1))
    attr(rval, "models") <- list(
        null = list(name=substitute(null), n=n0, p=p0,
            growth.model=null@model@growth.model,
            obs.error = null@model@obs.error),
        alt = list(name=substitute(alt), n=n1, p=p1,
            growth.model=alt@model@growth.model,
            obs.error = alt@model@obs.error))
    class(rval) <- c("pvaModelSelect", "data.frame")
    rval
}

print.pvaModelSelect <- 
function(x, ...)
{
    f <- attr(x, "fancy")
    m <- attr(x, "models")
    cat("PVA Model Selection:\n")
    cat(f$data, "\n\n")
    cat("Null Model:", m$null$name, "\n  ", f$null, "\n\n")
    cat("Alternative Model:", m$alt$name, "\n  ", f$alt, "\n\n")
    z <- x
    class(z) <- "data.frame"
    print(z, ...)
    llr <- x$log_LR
    if (-llr < 0) {
        good <- "Null Model"
        bad <- "Alternative Model"
    } else {
        good <- "Alternative Model"
        bad <- "Null Model"
    }
    if (abs(llr) < 2) {
        qualifier <- "slightly better"
    } else if (abs(llr) < 8) {
        qualifier <- "better"
    } else qualifier <- "strongly"
    cat("\n", good, " is ", qualifier, " supported over the ", 
        bad, "\n\n", sep="")
    invisible(x)
}
