## see check results
## move as.mcmc.list generic to dcmle and test
## separate pva, classes and methods

## this declares S4 class pvamodel
setClass("pvamodel", 
    representation(
        growth.model="character", 
        obs.error="character",
        model="dcModel",
        predmodel="dcModel",
        p="integer",
        support="matrix",
        params="character",
        varnames="character",
        fixed="nClones",
        fancy="character",
#        transf="function",      # original --> diagn
        backtransf="function",  # diagn --> original
        logdensity="function",
        neffective="function"))

## this declares inheritance and extension for 'pva' S4 class
setClass("pva", 
    representation(
        observations="numeric", 
        nobs="integer",
        model="pvamodel",
        dcdata="dcFit", 
        vcov="matrix"),
    contains = c("dcMle"))

## this creates fancy model description for show/summary methods
fancyPVAmodel <- 
function(object, initial="PVA object:\n", part=1:2)
{
    fn <- object@model@fancy
    growth.model <- paste(fn[1], "growth model")
    obs.error <- if (is.na(fn[2])) {
        " without observation error" 
    } else {
        paste(" with", fn[2], "observation error")
    }
    part1 <- paste(initial, growth.model, obs.error, sep="")
    x <- object@observations
    part2 <- paste("Time series with ", length(x),
        " observations (missing: ", length(x[is.na(x)]), ")", sep="")
    if (length(part)==1 && part==1)
        return(paste(part1, sep=""))
    if (length(part)==1 && part==2)
        return(paste(part2, sep=""))
    paste(part1, "\n\n", part2, sep="")
}

## this prints the summary
setMethod("show", "pva", function(object) {
    getMethod("summary","dcMle")(as(object, "dcMle"), 
        fancyPVAmodel(object))
    invisible(object)
})

## this extracts mcmc info 
## and transforms it to original scale if desired
setGeneric("as.mcmc.list",
  function(x, ...)
    standardGeneric("as.mcmc.list")
)
setMethod("as.mcmc.list", "pva", 
function(x, backtransf=FALSE, ...) {
    if (backtransf)
        x@model@backtransf(x@mcmc) else x@mcmc
})

## coef method (takes into account fixed values)
setMethod("coef", "pva", function(object) {
    object@summary[,1]
})

## need to define this as well
setMethod("vcov", "pva", function(object) {
    vc <- object@vcov
    fx <- object@model@fixed
    if (is.null(fx))
        return(vc)
    vn <- object@model@varnames
    rv <- matrix(NA, length(vn), length(vn))
    dimnames(rv) <- list(vn, vn)
    i <- !(vn %in% names(fx))
    rv[i,i] <- vc
    rv
})
setMethod("confint", "pva", function(object) {
    ci <- confint(as.mcmc.list(object, backtransf=TRUE))
    fx <- object@model@fixed
    if (is.null(fx))
        return(ci)
    vn <- object@model@varnames
    rv <- matrix(NA, length(vn), 2)
    dimnames(rv) <- list(vn, colnames(ci))
    i <- !(vn %in% names(fx))
    rv[i,] <- ci
    rv
})
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
#    x <- as.integer(x)
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
    fit <- dcmle(dcf, n.clones=n.clones, ...)
    fit0 <- as(model@backtransf(fit@mcmc), "dcMle")
    ## modify summary stats 
    ## summary and vcov is on original scale
    ## mcmc.list and diagnostics are on transformed scale
    s0 <- fit0@summary
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
    fit@summary <- s
#    fit@mcmc <- fit0@mcmc
    fit <- as(fit, "pva")
    fit@observations <- x
    fit@vcov <- vcov(fit0)
    fit@nobs <- as.integer(length(x))
    fit@model <- model
    fit@model@predmodel <- eval(call(model@growth.model, 
        obs.error=model@obs.error, fixed=coef(fit)))@model
    fit@dcdata <- dcf
    fit
}
