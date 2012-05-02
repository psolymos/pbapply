## virtual classes for S3 objects
setClass("custommodel", representation("VIRTUAL"))

## class unions for slots
setClassUnion("dcArgs", c("NULL", "character"))
setClassUnion("dcParams", c("NULL", "character", "list"))
setClassUnion("dcFunction", c("NULL", "function"))
setClassUnion("dcInits", c("NULL", "list", "function"))
setClassUnion("dcModel", c("function", "character", "custommodel"))

## data/model templates
setClass("gsFit", 
    representation(
        data = "list",
        model = "dcModel",
        params = "dcParams",
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
        flavour = "jags"))

## coercion (reverse is automatic based on inheritence)
#setAs(from = "gsFit", to = "dcFit", def = function(from) {
#    new("dcFit", from)
#})

## creator function for gsFit
makeGsFit <- 
function(data, model, params=NULL, inits=NULL)
{
    new("gsFit",
        data = data,
        model = model,
        params = params,
        inits = inits)
}
## creator function for dcFit
makeDcFit <- 
function(data, model, params=NULL, inits=NULL,
multiply=NULL, unchanged=NULL, update=NULL,
updatefun=NULL, initsfun=NULL, flavour)
{
    if (missing(flavour))
        flavour <- getOption("dcmle.flavour")
    new("dcFit",
        makeGsFit(data, model, params, inits),
        multiply = multiply,
        unchanged = unchanged,
        update = update,
        updatefun = updatefun,
        initsfun = initsfun,
        flavour = flavour)
}

## show for **Fit objects
setMethod("show", "dcFit", function(object) {
    str(object)
    invisible(object)
})
setMethod("show", "gsFit", function(object) {
    str(object)
    invisible(object)
})

## generic after coda
#setGeneric("as.mcmc.list",
#  function(x, ...)
#    standardGeneric("as.mcmc.list")
#)

## generic after utils
#setGeneric("stack",
#  function(x, ...)
#    standardGeneric("stack")
#)
