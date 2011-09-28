## virtual classes for S3 objects
setClass("custommodel", representation("VIRTUAL"))
setClass("mcmc", representation("VIRTUAL"))
setClass("mcmc.list", representation("VIRTUAL"))
setClass("mcmc.list.dc", representation("VIRTUAL"))
setClass("dctable", representation("VIRTUAL"))
setClass("dcdiag", representation("VIRTUAL"))

## class unions for slots
setClassUnion("nClones", c("NULL", "numeric"))
setClassUnion("dcDiag", c("NULL", "dcdiag"))
setClassUnion("dcTable", c("NULL", "dctable"))
setClassUnion("MCMClist", c("mcmc", "mcmc.list", "mcmc.list.dc"))
setClassUnion("dcArgs", c("NULL", "character"))
setClassUnion("dcFunction", c("NULL", "function"))
setClassUnion("dcInits", c("NULL", "list", "function"))
setClassUnion("dcModel", c("function", "character", "custommodel"))

## data/model templates
setClass("gsFit", 
    representation(
        data = "list",
        model = "dcModel",
        params = "dcArgs",
        inits = "dcInits"),
    prototype = list(
        data = list(),
        model = character(0),
        params = NULL,
        inits = NULL))
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
        flavour = getOption("dcmle.flavour")))

## coercion (reverse is automatoc based on inheritence)
setAs(from = "gsFit", to = "dcFit", def = function(from) {
    out <- new("dcFit")
    out@data <- from@data
    out@model <- from@model
    out@inits <- from@inits
    out@params <- from@params
    out
})

## fitted model opject
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

## coercion from mcmc.list/mcmc.list.dc
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
    rval@n.clones <- nclones(from)
    rval
})
## reverse coercion
setAs(from = "dcMle", to = "MCMClist", def = function(from) {
    out <- from@mcmc
    attr(out, "dcdiag") <- from@dcdiag
    attr(out, "dctable") <- from@dctable
    out
})

## creator function for gsFit
makeGsFit <- 
function(data, model, params=NULL, inits=NULL)
{
    x <- new("gsFit")
    x@data <- data
    x@model <- custommodel(model)
    x@params <- params
    x@inits <- inits
    x
}
## creator function for dcFit
makeDcFit <- 
function(data, model, params=NULL, inits=NULL,
multiply=NULL, unchanged=NULL, update=NULL,
updatefun=NULL, initsfun=NULL, flavour)
{
    if (missing(flavour))
        flavour <- getOption("dcmle.flavour")
    x <- makeGsFit(data, model, params, inits)
    x <- as(x, "dcFit")
    x@multiply <- multiply
    x@unchanged <- unchanged
    x@update <- update
    x@updatefun <- updatefun
    x@initsfun <- initsfun
    x@flavour <- flavour
    x
}
