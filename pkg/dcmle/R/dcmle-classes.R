## ------------------
## CLASS DEFINITIONS
## ------------------

## virtual classes for S3 objects
#setClass("bugs", representation("VIRTUAL"))
setClass("mcmc", representation("VIRTUAL"))
setClass("mcmc.list", representation("VIRTUAL"))
setClass("mcmc.list.dc", representation("VIRTUAL"))
setClass("dctable", representation("VIRTUAL"))
setClass("dcdiag", representation("VIRTUAL"))

## class unions for slots
setClassUnion("dcDiag", c("NULL", "dcdiag"))
setClassUnion("dcTable", c("NULL", "dctable"))
setClassUnion("nClones", c("NULL", "numeric"))
setClassUnion("MCMClist", c("mcmc", "mcmc.list", "mcmc.list.dc"))

## this is an S4 class for mcmc.list of coda style
setClass("codaMCMC", 
    representation(
        values = "numeric",
        varnames = "character",
        start = "integer",
        end = "integer",
        thin = "integer",
        nchains = "integer",
        niter = "integer",
        nvar = "integer"),
    validity=function(object) {
        if (length(object@varnames) != object@nvar)
            stop("length of varnames must equal nvar")
        if (length(object@values) != object@niter*object@nchains*object@nvar)
            stop("number of values must equal prod(dim)")
        checkfun <- function(z) {
            length(z) > 1 || all(is.na(z)) || z < 0
        }
        if (checkfun(object@start))
            stop("inadequate start value")
        if (checkfun(object@end))
            stop("inadequate end value")
        if (checkfun(object@thin))
            stop("inadequate thin value")
        if (checkfun(object@nchains))
            stop("inadequate nchains value")
        if (checkfun(object@niter))
            stop("inadequate niter value")
        if (checkfun(object@nvar))
            stop("inadequate nvar value")
        if ((object@end-object@start+object@thin)/object@thin != object@niter)
            stop("thin, start and end values are incompatible with niter")
        TRUE
    })

## all DC info with MCMC list
setClass("dcCodaMCMC", 
    contains = "codaMCMC",
    representation(
        dctable = "dcTable",
        dcdiag = "dcDiag",
        nclones = "nClones"))

setClass("dcmle", 
    representation(
        call     = "language",
        coef     = "numeric",
        fullcoef = "numeric", 
        vcov     = "matrix",
        details  = "dcCodaMCMC",
        nobs     = "integer",
        method   = "character"))

## ------------------
## COERCION METHODS
## ------------------

## coercion method (ignores any DC info)
setAs(from = "MCMClist", to = "codaMCMC", def = function(from) {
    from <- as.mcmc.list(from)
    val <- unname(as.array(from))
    dim(val) <- NULL
    vn <- varnames(from)
    if (is.null(vn))
        vn <- paste("var", 1:nvar(from), sep="")
    new("codaMCMC",
        values = as.numeric(val),
        varnames = as.character(vn),
        start = as.integer(start(from)),
        end = as.integer(end(from)),
        thin = as.integer(thin(from)),
        nchains = as.integer(length(from)),
        niter = as.integer(niter(from)),
        nvar = as.integer(nvar(from)))
})
setAs(from = "MCMClist", to = "dcCodaMCMC", def = function(from) {
    k <- nclones(from)
    if (is.null(k))
        k <- NA
    dcd <- dcdiag(from)
    rownames(dcd) <- deparse(substitute(from))
    new("dcCodaMCMC", 
        as(from, "codaMCMC"),
        dcdiag = dcd,
        dctable = dctable(from),
        nclones = nclones(from))
})
setAs(from = "MCMClist", to = "dcmle", def = function(from) {
    details <- as(from, "dcCodaMCMC")
    cfs <- coef(from)
    vcv <- vcov(from)
    if (is.null(names(cfs))) {
        nam <- paste("var", 1:nvar(from), sep="")
        names(cfs) <- nam
        dimnames(vcv) <- list(nam, nam)
    }
    new("dcmle",
#        call     = match.call(),
        coef     = cfs,
        fullcoef = cfs, 
        vcov     = vcv,
        details  = details,
        nobs     = as.integer(NA),
        method   = "DataCloningMCMC")
})
## inverse coercion
setAs(from = "codaMCMC", to = "MCMClist", def = function(from) {
    a <- array(from@values, c(from@niter, from@nvar, from@nchains))
    m <- lapply(seq_len(from@nchains), function(i) {
        mcmc(a[,,i], start=from@start, end=from@end, thin=from@thin)
    })
    out <- as.mcmc.list(m)
    varnames(out) <- from@varnames
    out
})
setAs(from = "dcCodaMCMC", to = "MCMClist", def = function(from) {
    a <- array(from@values, c(from@niter, from@nvar, from@nchains))
    m <- lapply(seq_len(from@nchains), function(i) {
        mcmc(a[,,i], start=from@start, end=from@end, thin=from@thin)
    })
    out <- as.mcmc.list(m)
    varnames(out) <- from@varnames
    attr(out, "dcdiag") <- from@dcdiag
    attr(out, "dctable") <- from@dctable
    if (!is.null(from@nclones)) {
        attr(out, "n.clones") <- from@nclones
        class(out) <- c("mcmc.list.dc","mcmc.list")
    }
    out
})
setAs(from = "dcmle", to = "MCMClist", def = function(from) {
    as(from@details, "MCMClist")
})
setAs(from = "dcmle", to = "codaMCMC", def = function(from) {
    as(from@details, "codaMCMC")
})
setAs(from = "dcmle", to = "dcCodaMCMC", def = function(from) {
    from@details
})
#setAs(from = "codaMCMC", to = "dcmle", def = function(from) {
#    new("dcmle", details=as(from, "dcCodaMCMC"))
#})
#setAs(from = "dcCodaMCMC", to = "dcmle", def = function(from) {
#    new("dcmle", details=from)
#})
setAs(from = "codaMCMC", to = "dcmle", def = function(from) {
    details <- as(from, "dcCodaMCMC")
    cfs <- coef(from)
    vcv <- vcov(from)
    if (is.null(names(cfs))) {
        nam <- paste("var", 1:nvar(from), sep="")
        names(cfs) <- nam
        dimnames(vcv) <- list(nam, nam)
    }
    new("dcmle",
#        call     = match.call(),
        coef     = cfs,
        fullcoef = cfs, 
        vcov     = vcv,
        details  = details,
        nobs     = as.integer(NA),
        method   = "DataCloningMCMC")
})
setAs(from = "dcCodaMCMC", to = "dcmle", def = function(from) {
    cfs <- coef(from)
    vcv <- vcov(from)
    if (is.null(names(cfs))) {
        nam <- paste("var", 1:nvar(from), sep="")
        names(cfs) <- nam
        dimnames(vcv) <- list(nam, nam)
    }
    new("dcmle",
#        call     = match.call(),
        coef     = cfs,
        fullcoef = cfs, 
        vcov     = vcv,
        details  = from,
        nobs     = as.integer(NA),
        method   = "DataCloningMCMC")
})

## and as.mcmc.list method
setMethod("as.mcmc.list", "dcmle", function(x, ...) {
    as(x, "MCMClist")
})
setMethod("as.mcmc.list", "codaMCMC", function(x, ...) {
    as(x, "MCMClist")
})
