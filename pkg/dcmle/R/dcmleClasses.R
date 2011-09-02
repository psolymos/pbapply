## virtual classes for S3 objects
setClass("custommodel", representation("VIRTUAL"))
setClass("mcmc", representation("VIRTUAL"))
setClass("mcmc.list", representation("VIRTUAL"))
setClass("mcmc.list.dc", representation("VIRTUAL"))
setClass("dctable", representation("VIRTUAL"))
setClass("dcdiag", representation("VIRTUAL"))

setClassUnion("nClones", c("NULL", "numeric"))
setClassUnion("dcDiag", c("NULL", "dcdiag"))
setClassUnion("dcTable", c("NULL", "dctable"))
setClassUnion("MCMClist", c("mcmc", "mcmc.list", "mcmc.list.dc"))
setClassUnion("dcArgs", c("NULL", "character"))
setClassUnion("dcFunction", c("NULL", "function"))
setClassUnion("dcInits", c("NULL", "list", "function"))
setClassUnion("dcModel", c("function", "character", "custommodel"))

setClass("gsFit", 
    representation(
        data = "list",
        inits = "dcInits",
        model = "dcModel",
        params = "dcArgs"),
    prototype = list(
        data = list(),
        inits = NULL,
        model = character(0)))
setClass("dcFit",
    representation(
        multiply = "dcArgs",
        unchanged = "dcArgs",
        update = "dcArgs",
        updatefun = "dcFunction",
        initsfun = "dcFunction",
        flavour = "character"),
    contains = "gsFit",
    prototype = list(
        params = NULL,
        multiply = NULL,
        unchanged = NULL,
        update = NULL,
        updatefun = NULL,
        initsfun = NULL,
        flavour = "jags"))
setClass("dcMle", 
    representation(
        mcmc = "MCMClist",
        summary = "matrix",
        dctable = "dcTable",
        dcdiag = "dcDiag",
        start = "numeric",
        end = "numeric",
        thin = "numeric",
        n.chains = "numeric",
        n.clones = "nClones"),
    prototype = list(
        mcmc = as.mcmc(matrix(0,0,0)),
        summary = matrix(0,0,0),
        diag = NULL,
        start = numeric(0),
        end = numeric(0),
        thin = numeric(0),
        n.chains = numeric(0),
        n.clones = NULL))
setAs(from = "gsFit", to = "dcFit", def = function(from) {
    out <- new("dcFit")
    out@data <- from@data
    out@model <- from@model
    out@inits <- from@inits
    out@params <- from@params
    out
})
setAs(from = "MCMClist", to = "dcMle", def = function(from) {
    rval <- new("dcMle")
    if (!is.null(nclones(from))) {
        coefs <- coef(from)
        se <- dcsd(from)
        zstat <- coefs/se
        pval <- 2 * pnorm(-abs(zstat))
        coefs <- cbind(coefs, se, zstat, pval)
        colnames(coefs) <- c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
        rval@summary <- coefs
    }
    dcd <- try(dcdiag(from), silent=TRUE)
    if (inherits(dcd, "try-error")) {
        dcd <- NULL
    } else {
        rownames(dcd) <- NULL
    }
    dct <- try(dctable(from), silent=TRUE)
    if (inherits(dct, "try-error")) {
        dct <- NULL
    } else {
        dct <- lapply(dct, function(z) {
            rownames(z) <- NULL
            z
        })
        class(dct) <- "dctable"
    }
    rval@mcmc <- from
    attr(rval@mcmc, "dcdiag") <- NULL
    attr(rval@mcmc, "dctable") <- NULL
    rval@dcdiag <- dcd
    rval@dctable <- dct
    rval@start <- start(from)
    rval@end <- end(from)
    rval@thin <- thin(from)
    rval@n.chains <- length(from)
    rval@n.clones <- nclones(from)#dclone:::nclones.default(from)
    rval
})
setAs(from = "dcMle", to = "MCMClist", def = function(from) {
    out <- from@mcmc
    attr(out, "dcdiag") <- from@dcdiag
    attr(out, "dctable") <- from@dctable
    out
})
dcmle <- function(x, params, n.clones=1, cl=NULL, ...) {
    x <- as(x, "dcFit")
    if (missing(params))
        params <- x@params
    if (length(n.clones) == 1) {
        inits <- x@inits
        if (n.clones > 1 && !is.null(x@initsfun)) {
            initsfun <- match.fun(x@initsfun)
            ARGS <- names(as.list(args(initsfun)))
            ARGS <- ARGS[1:max(2, length(ARGS)-1)]
            if (length(ARGS)==2)
                eval(parse(text = paste("inits <- ", 
                    deparse(substitute(initsfun)), "(", 
                    ARGS[2], "=n.clones)", sep = "")))
        }
        dat <- dclone(x@data, n.clones, x@multiply, x@unchanged)
        if (x@flavour == "jags" && is.null(cl))
            out <- jags.fit(dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && is.null(cl))
            out <- bugs.fit(dat, params, x@model, inits, ...)
        if (x@flavour == "jags" && !is.null(cl))
            out <- jags.parfit(cl, dat, params, x@model, inits, ...)
        if (x@flavour == "bugs" && !is.null(cl))
            stop("parallel chains with flavour='bugs' not supported")
    } else {
        if (is.null(cl)) {
            out <- dc.fit(x@data, params, x@model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        } else {
            out <- dc.parfit(cl, x@data, params, x@model, x@inits, 
                n.clones = n.clones, 
                multiply = x@multiply, 
                unchanged = x@unchanged,
                update = x@update,
                updatefun = x@updatefun,
                initsfun = x@initsfun,
                flavour = x@flavour, ...)
        }
    }
    as(out, "dcMle")
}
setMethod("show", "dcMle", function(object) {
    k <- object@n.clones
    if (is.null(k)) {
        print(summary(object@mcmc))
    } else {
        attributes(k) <- NULL
        n <- data.frame(start=object@start, end=object@end, thin=object@thin,
            n.iter=object@end-object@start+1,
            n.chains=object@n.chains, n.clones=k)
        digits <- max(3, getOption("digits") - 3)
#        cat("Object of class \"", class(object)[1L], "\"\n\n", sep="")
        print(n, digits=digits, row.names=FALSE)
        cat("\n")
        printCoefmat(object@summary, digits = digits, signif.legend = TRUE)
        cat("\n")
        print(object@dcdiag, digits=digits, row.names=FALSE)
#        cat("\n")
    }
    invisible(object)
})

setMethod("summary", "dcMle", function(object, title, ...) {
    if (missing(title))
        title <- paste("Object of class \"", class(object)[1L], "\"", sep="")
    cat(title, "\n\n")
    show(object)
    cat("\n")
})

setMethod("coef", "dcMle", function(object, ...) coef(object@mcmc, ...))
setMethod("vcov", "dcMle", function(object, ...) vcov(object@mcmc, ...))
setMethod("confint", "dcMle", function(object, parm, level = 0.95, ...) {
    if (!inherits(object@mcmc, "mcmc.list.dc"))
        stop("'confint' method not defined for k=1")
    confint(object@mcmc, parm, level, ...)
})

#setGeneric("quantile", function(x, ...) standardGeneric("quantile"))
#setGeneric("dcdiag", function(x, ...) standardGeneric("dcdiag"))
#setGeneric("dctable", function(x, ...) standardGeneric("dctable"))
#setGeneric("dcsd", function(object, ...) standardGeneric("dcsd"))
#setGeneric("nclones", function(x, ...) standardGeneric("nclones"))
setMethod("quantile", "dcMle", function(x, ...) quantile(x@mcmc, ...))
setMethod("dcdiag", "dcMle", function(x, ...) x@dcdiag)
setMethod("dctable", "dcMle", function(x, ...) x@dctable)
setMethod("dcsd", "dcMle", function(object, ...) dcsd(object@mcmc, ...))
setMethod("nclones", "dcMle", function(x, ...) x@n.clones)


