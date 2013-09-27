## ---------------------
## methods with generic defined in base
## ---------------------

setMethod("as.matrix", "dcmle", function(x, ...) 
    as.matrix(as(x, "MCMClist"), ...))
setMethod("as.matrix", "codaMCMC", function(x, ...) 
    as.matrix(as(x, "MCMClist"), ...))

setMethod("as.array", "dcmle", function(x, ...) 
    array(x@details@values, 
        dim=c(x@details@niter, x@details@nvar, x@details@nchains),
        dimnames=list(NULL, x@details@varnames, NULL)))
setMethod("as.array", "codaMCMC", function(x, ...) 
    array(x@values, 
        dim=c(x@niter, x@nvar, x@nchains),
        dimnames=list(NULL, x@varnames, NULL)))

## ---------------------
## methods with generic defined in dclone
## ---------------------

setMethod("dcdiag", "dcmle", function(x, ...) x@details@dcdiag)
setMethod("dcdiag", "dcCodaMCMC", function(x, ...) x@dcdiag)
setMethod("dcdiag", "codaMCMC", function(x, ...) {
    dcdiag(as(x, "MCMClist"), ...)
})

setMethod("dctable", "dcmle", function(x, ...) x@details@dctable)
setMethod("dctable", "dcCodaMCMC", function(x, ...) x@dctable)
setMethod("dctable", "codaMCMC", function(x, ...) {
    dctable(as(x, "MCMClist"), ...)
})

setMethod("dcsd", "dcmle", function(object, ...) {
    sqrt(diag(vcov(object)))
})
setMethod("dcsd", "codaMCMC", function(object, ...) {
    dcsd(as(object, "MCMClist"), ...)
})

setMethod("nclones", "dcmle", function(x, ...) x@details@nclones)
setMethod("nclones", "dcCodaMCMC", function(x, ...) x@nclones)
setMethod("nclones", "codaMCMC", function(x, ...) NULL)

## ---------------------
## methods with generic defined in coda
## ---------------------

## coda type accessor functions and generics
setGeneric("nvar", function(x) standardGeneric("nvar"))
setGeneric("varnames", function(x, ...) standardGeneric("varnames"))
setGeneric("chanames", function(x, ...) 
    standardGeneric("chanames"))
setGeneric("nchain", function(x) standardGeneric("nchain"))
setGeneric("niter", function(x) standardGeneric("niter"))
#setGeneric("thin", function(x) standardGeneric("thin"))
setGeneric("crosscorr", function(x) standardGeneric("crosscorr"))
setGeneric("mcpar", function(x) standardGeneric("mcpar"))

setMethod("nvar", "dcmle", function(x) x@details@nvar)
setMethod("varnames", "dcmle", function(x, ...) x@details@varnames)
setMethod("chanames", "dcmle", function(x, ...) 
    chanames(as(x, "MCMClist"), ...))
setMethod("nchain", "dcmle", function(x) x@details@nchains)
setMethod("niter", "dcmle", function(x) x@details@niter)
setMethod("thin", "dcmle", function(x) x@details@thin)
setMethod("crosscorr", "dcmle", function(x) 
    crosscorr(as(x, "MCMClist")))
setMethod("mcpar", "dcmle", function(x) c(start(x), end(x), thin(x)))

setMethod("nvar", "codaMCMC", function(x) x@nvar)
setMethod("varnames", "codaMCMC", function(x, ...) x@varnames)
setMethod("chanames", "codaMCMC", function(x, ...) 
    chanames(as(x, "MCMClist"), ...))
setMethod("nchain", "codaMCMC", function(x) x@nchains)
setMethod("niter", "codaMCMC", function(x) x@niter)
setMethod("thin", "codaMCMC", function(x) x@thin)
setMethod("crosscorr", "codaMCMC", function(x) 
    crosscorr(as(x, "MCMClist")))
setMethod("mcpar", "codaMCMC", function(x) c(start(x), end(x), thin(x)))

## these are necessary due to virtual class mcmc.list
setMethod("nvar", "MCMClist", function(x) coda::nvar(x))
setMethod("varnames", "MCMClist", function(x, ...) coda::varnames(x, ...))
setMethod("chanames", "MCMClist", function(x, ...) 
    coda::chanames(as(x, "MCMClist"), ...))
setMethod("nchain", "MCMClist", function(x) coda::nchain(x))
setMethod("niter", "MCMClist", function(x) coda::niter(x))
setMethod("thin", "MCMClist", function(x) coda::thin(x))
setMethod("crosscorr", "MCMClist", function(x) 
    coda::crosscorr(x))
setMethod("mcpar", "MCMClist", function(x) c(start(x), end(x), thin(x)))

## ---------------------
## methods with generic defined in stats
## ---------------------

setMethod("coef", "dcmle", function(object, ...) object@coef)
setMethod("coef", "codaMCMC", function(object, ...) 
    coef(as(object, "MCMClist"), ...))

setMethod("vcov", "dcmle", function(object, ...) object@vcov)
setMethod("vcov", "codaMCMC", function(object, ...) 
    vcov(as(object, "MCMClist"), ...))

setMethod("confint", "dcmle", function(object, parm, level = 0.95, ...) {
    if (is.null(nclones(object)) || nclones(object) < 2)
        stop("'confint' method not defined for k=1")
    confint(as(object, "MCMClist"), parm, level, ...)
})
setMethod("confint", "dcCodaMCMC", function(object, parm, level = 0.95, ...) {
    if (is.null(nclones(object)) || nclones(object) < 2)
        stop("'confint' method not defined for k=1")
    confint(as(object, "MCMClist"), parm, level, ...)
})
setMethod("confint", "MCMClist", function(object, parm, level = 0.95, ...) {
    if (is.null(nclones(object)) || nclones(object) < 2)
        stop("'confint' method not defined for k=1")
    dclone::confint.mcmc.list.dc(object, parm, level, ...)
})
setMethod("confint", "codaMCMC", function(object, parm, level = 0.95, ...) {
    stop("'confint' method not defined for k=1")
})

setMethod("quantile", "MCMClist", function(x, ...) {
    dclone::quantile.mcmc.list(x, ...)
})
setMethod("quantile", "dcmle", function(x, ...) {
    quantile(as(x, "MCMClist"), ...)
})
setMethod("quantile", "codaMCMC", function(x, ...) {
    quantile(as(x, "MCMClist"), ...)
})

## ts like accessors for mcmc objects

#setGeneric("time", function(x, ...) standardGeneric("time"))
#setGeneric("start", function(x, ...) standardGeneric("start"))
#setGeneric("end", function(x, ...) standardGeneric("end"))

setMethod("start", "dcmle", function(x, ...) x@details@start)
setMethod("start", "codaMCMC", function(x, ...) x@start)

setMethod("end", "dcmle", function(x, ...) x@details@end)
setMethod("end", "codaMCMC", function(x, ...) x@end)

setMethod("frequency", "dcmle", function(x, ...) 1/thin(x))
setMethod("frequency", "codaMCMC", function(x, ...) 1/thin(x))
setMethod("frequency", "MCMClist", function(x, ...) 1/thin(x))

setMethod("time", "dcmle", function(x, ...) {
    val <- seq(start(x), end(x), by = thin(x))
    time(ts(data = val, 
        start = start(x), end = end(x), frequency = thin(x)), ...)
})
setMethod("time", "codaMCMC", function(x, ...) {
    val <- seq(start(x), end(x), by = thin(x))
    time(ts(data = val, 
        start = start(x), end = end(x), frequency = thin(x)), ...)
})

setMethod("window", "dcmle", function(x, ...) {
    as(coda:::window.mcmc.list(as.mcmc.list(x), ...), "dcmle")
})
setMethod("window", "codaMCMC", function(x, ...) {
    as(coda:::window.mcmc.list(as.mcmc.list(x), ...), "codaMCMC")
})

## no update for codaMCMC/dcCodaMCMC
setMethod("update", "dcmle", function (object, ...) {
    .local <- function (object, ..., evaluate = TRUE) 
    {
        call <- object@call
        extras <- match.call(expand.dots = FALSE)$...
        if (length(extras)) {
            existing <- !is.na(match(names(extras), names(call)))
            for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
            if (any(!existing)) {
                call <- c(as.list(call), extras[!existing])
                call <- as.call(call)
            }
        }
        if (evaluate) 
            eval(call, parent.frame())
        else call
    }
    .local(object, ...)
})

## ---------------------
## methods with generic defined in utils
## ---------------------

## stack
#setGeneric("stack", function(x) standardGeneric("stack"))
setMethod("stack", "dcmle", function(x, ...) {
    data.frame(
        iter=rep(time(x), nvar(x)*nchain(x)),
        variable=rep(rep(varnames(x), each=niter(x)), nchain(x)),
        chain=as.factor(rep(seq_len(nchain(x)), each=niter(x)*nvar(x))),
        value=x@details@values)
})
setMethod("stack", "codaMCMC", function(x, ...) {
    data.frame(
        iter=rep(time(x), nvar(x)*nchain(x)),
        variable=rep(rep(varnames(x), each=niter(x)), nchain(x)),
        chain=as.factor(rep(seq_len(nchain(x)), each=niter(x)*nvar(x))),
        value=x@values)
})

## this displays a compact structure (without detailed dctable)
#setGeneric("str", function(object, ...) standardGeneric("str"))
setMethod("str", "dcmle", function(object, max.level=5L, ...) 
    utils:::str.default(object, max.level=max.level, ...))
setMethod("str", "dcCodaMCMC", function(object, max.level=3L, ...) 
    utils:::str.default(object, max.level=max.level, ...))

setMethod("head", "dcmle", function(x, ...) 
    coda:::head.mcmc.list(as(x, "MCMClist"), ...))
setMethod("tail", "dcmle", function(x, ...) 
    coda:::tail.mcmc.list(as(x, "MCMClist"), ...))
setMethod("head", "codaMCMC", function(x, ...) 
    coda:::head.mcmc.list(as(x, "MCMClist"), ...))
setMethod("tail", "codaMCMC", function(x, ...) 
    coda:::tail.mcmc.list(as(x, "MCMClist"), ...))


## this is after mle class

setMethod("show", "codaMCMC", function(object) {
    str(object)
    invisible(object)
})

setMethod("show", "dcmle", function(object) {
    cat("Call:\n")
    print(object@call)
    cat("\nCoefficients:\n")
    print(coef(object))
})

setClass("summary.codaMCMC", 
    representation(
        settings = "integer",
        coef = "matrix"))

setClass("summary.dcCodaMCMC", 
    contains="summary.codaMCMC",
    representation(
        settings = "integer",
        coef = "matrix",
        convergence = "dcDiag"))

setClass("summary.dcmle", 
    contains="summary.dcCodaMCMC",
    representation(
        title="character",
        call = "language"))

setMethod("summary", "codaMCMC", function(object, ...) {
    k <- nclones(object)
    if (is.null(k))
        k <- 1L
    attributes(k) <- NULL
    settings <- c(
        start=start(object), 
        end=end(object), 
        thin=thin(object),
        n.iter=end(object)-start(object)+1,
        n.chains=nchain(object), 
        n.clones=k)
    storage.mode(settings) <- "integer"
    coefs <- coef(object)
    se <- dcsd(object)
    cmat <- cbind(coefs, se)
    colnames(cmat) <- c("Estimate", "Std. Deviation")
    new("summary.codaMCMC", 
        settings=settings,
        coef = cmat)
})

setMethod("summary", "dcCodaMCMC", function(object, ...) {
    k <- nclones(object)
    if (is.null(k))
        k <- 1L
    attributes(k) <- NULL
    settings <- c(
        start=start(object), 
        end=end(object), 
        thin=thin(object),
        n.iter=end(object)-start(object)+1,
        n.chains=nchain(object), 
        n.clones=k)
    storage.mode(settings) <- "integer"
    coefs <- coef(object)
    se <- dcsd(object)
    zstat <- coefs/se
    pval <- 2 * pnorm(-abs(zstat))
    cmat <- cbind(coefs, se, zstat, pval)
    colnames(cmat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    new("summary.dcCodaMCMC", 
        settings=settings,
        coef = cmat,
        convergence=dcdiag(object))
})

setMethod("summary", "dcmle", function(object, title, ...) {
    k <- nclones(object)
    if (is.null(k))
        k <- 1L
    attributes(k) <- NULL
    if (missing(title)) {
        title <- if (k > 1) {
            "Maximum likelihood estimation with data cloning\n\n"
        } else {
            "Bayesian estimation\n\n"
        }
    } else {
        title <- paste(title, "\n\n", sep="")
    }
    settings <- c(
        start=start(object), 
        end=end(object), 
        thin=thin(object),
        n.iter=end(object)-start(object)+1,
        n.chains=nchain(object), 
        n.clones=k)
    storage.mode(settings) <- "integer"
    coefs <- coef(object)
    se <- dcsd(object)
    zstat <- coefs/se
    pval <- 2 * pnorm(-abs(zstat))
    cmat <- cbind(coefs, se, zstat, pval)
    colnames(cmat) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
    new("summary.dcmle", 
        title=title,
        call = object@call, 
        settings=settings,
        coef = cmat,
        convergence=dcdiag(object))
})

setMethod("show", "summary.codaMCMC", function(object) {
    digits <- max(3, getOption("digits") - 3)
    cat("'codaMCMC' object\n")
    cat("\nSettings:\n")
    print(data.frame(t(object@settings)), 
        digits=digits, row.names=FALSE)
    cat("\nCoefficients:\n")
    printCoefmat(object@coef, 
        digits = digits, signif.legend = TRUE)
    invisible(object)
})

setMethod("show", "summary.dcCodaMCMC", function(object) {
    digits <- max(3, getOption("digits") - 3)
    cat("'dcCodaMCMC' object\n")
    cat("\nSettings:\n")
    print(data.frame(t(object@settings)), 
        digits=digits, row.names=FALSE)
    cat("\nCoefficients:\n")
    printCoefmat(object@coef, 
        digits = digits, signif.legend = TRUE)
    cat("\nConvergence:\n")
    print(object@convergence, 
        digits=digits, row.names=FALSE)
    invisible(object)
})

setMethod("show", "summary.dcmle", function(object) {
    digits <- max(3, getOption("digits") - 3)
    cat(object@title)
    cat("Call:\n")
    print(object@call)
    cat("\nSettings:\n")
    print(data.frame(t(object@settings)), 
        digits=digits, row.names=FALSE)
    cat("\nCoefficients:\n")
    printCoefmat(object@coef, 
        digits = digits, signif.legend = TRUE)
    cat("\nConvergence:\n")
    print(object@convergence, 
        digits=digits, row.names=FALSE)
    invisible(object)
})

## ---------------------
## subsetting methods (generic from base -- no import needed)
## ---------------------

setMethod("[[", signature(x = "codaMCMC"), function (x, i, j, ...) 
        as(as.mcmc.list(x)[i], "codaMCMC"))
setMethod("[[", signature(x = "dcCodaMCMC"), function (x, i, j, ...) 
        as(as.mcmc.list(x)[i], "dcCodaMCMC"))
setMethod("[[", signature(x = "dcmle"), function (x, i, j, ...) 
        as(as.mcmc.list(x)[i], "dcmle"))

setMethod("[",
    signature(x = "codaMCMC"),
    function (x, i, j, ..., drop = TRUE) 
    {
        if (missing(j))
            return(x[[i]])
        y <- as.mcmc.list(x)[i, j, drop]
        if (!inherits(y, "mcmc.list"))
            y else as(y, "codaMCMC")
    })
setMethod("[",
    signature(x = "dcCodaMCMC"),
    function (x, i, j, ..., drop = TRUE) 
    {
        if (missing(j))
            return(x[[i]])
        y <- as.mcmc.list(x)[i, j, drop]
        if (!inherits(y, "mcmc.list"))
            y else as(y, "dcCodaMCMC")
    })
setMethod("[",
    signature(x = "dcmle"),
    function (x, i, j, ..., drop = TRUE) 
    {
        if (missing(j))
            return(x[[i]])
        y <- as.mcmc.list(x)[i, j, drop]
        if (!inherits(y, "mcmc.list"))
            y else as(y, "dcmle")
    })


## ---------------------
## diagnostic functions from coda and dclone
## ---------------------

setGeneric("gelman.diag", function(x, ...) 
    standardGeneric("gelman.diag"))
setMethod("gelman.diag", "MCMClist", function(x, ...) 
    coda::gelman.diag(x, ...))
setMethod("gelman.diag", "codaMCMC", function(x, ...) 
    gelman.diag(as(x, "MCMClist"), ...))
setMethod("gelman.diag", "dcmle", function(x, ...) 
    gelman.diag(as(x, "MCMClist"), ...))

setGeneric("geweke.diag", function(x, ...) 
    standardGeneric("geweke.diag"))
setMethod("geweke.diag", "MCMClist", function(x, ...) 
    coda::geweke.diag(x, ...))
setMethod("geweke.diag", "codaMCMC", function(x, ...) 
    geweke.diag(as(x, "MCMClist"), ...))
setMethod("geweke.diag", "dcmle", function(x, ...) 
    geweke.diag(as(x, "MCMClist"), ...))

setGeneric("raftery.diag", function(x, ...) 
    standardGeneric("raftery.diag"))
setMethod("raftery.diag", "MCMClist", function(x, ...) 
    coda::raftery.diag(x, ...))
setMethod("raftery.diag", "codaMCMC", function(x, ...) 
    raftery.diag(as(x, "MCMClist"), ...))
setMethod("raftery.diag", "dcmle", function(x, ...) 
    raftery.diag(as(x, "MCMClist"), ...))

setGeneric("heidel.diag", function(x, ...) 
    standardGeneric("heidel.diag"))
setMethod("heidel.diag", "MCMClist", function(x, ...) 
    coda::heidel.diag(x, ...))
setMethod("heidel.diag", "codaMCMC", function(x, ...) 
    heidel.diag(as(x, "MCMClist"), ...))
setMethod("heidel.diag", "dcmle", function(x, ...) 
    heidel.diag(as(x, "MCMClist"), ...))

#setGeneric("autocorr.diag", function(mcmc.obj, ...) 
#    standardGeneric("autocorr.diag"))
setMethod("autocorr.diag", "MCMClist", function(mcmc.obj, ...) 
    coda:::autocorr.diag.mcmc.list(mcmc.obj, ...))
setMethod("autocorr.diag", "codaMCMC", function(mcmc.obj, ...) 
    autocorr.diag(as(mcmc.obj, "MCMClist"), ...))
setMethod("autocorr.diag", "dcmle", function(mcmc.obj, ...) 
    autocorr.diag(as(mcmc.obj, "MCMClist"), ...))

#setGeneric("chisq.diag", function(x, ...) 
#    standardGeneric("chisq.diag"))
setMethod("chisq.diag", "MCMClist", function(x, ...) 
    dclone::chisq.diag(x))
setMethod("chisq.diag", "codaMCMC", function(x, ...) 
    chisq.diag(as(x, "MCMClist"), ...))
setMethod("chisq.diag", "dcmle", function(x, ...) 
    chisq.diag(as(x, "MCMClist"), ...))

#setGeneric("lambdamax.diag", function(x, ...) 
#    standardGeneric("lambdamax.diag"))
setMethod("lambdamax.diag", "MCMClist", function(x, ...) 
    dclone::lambdamax.diag(x))
setMethod("lambdamax.diag", "codaMCMC", function(x, ...) 
    lambdamax.diag(as(x, "MCMClist"), ...))
setMethod("lambdamax.diag", "dcmle", function(x, ...) 
    lambdamax.diag(as(x, "MCMClist"), ...))

