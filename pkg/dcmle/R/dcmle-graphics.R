## plot methods (coda and lattice)

setMethod("plot", signature(x="MCMClist", y="missing"), function(x, ...) 
    plot(as(x, "mcmc.list"), ...))
setMethod("plot", signature(x="codaMCMC", y="missing"), function(x, ...) 
    plot(as(x, "MCMClist"), ...))
setMethod("plot", signature(x="dcmle", y="missing"), function(x, ...) 
    plot(as(x, "MCMClist"), ...))

#setGeneric("traceplot", function(x, ...) standardGeneric("traceplot"))
setMethod("traceplot", "MCMClist", function(x, ...) 
    coda::traceplot(x, ...))
setMethod("traceplot", "codaMCMC", function(x, ...) 
    traceplot(as(x, "MCMClist"), ...))
setMethod("traceplot", "dcmle", function(x, ...) 
    traceplot(as(x, "MCMClist"), ...))

#setGeneric("densplot", function(x, ...) standardGeneric("densplot"))
setMethod("densplot", "MCMClist", function(x, ...) 
    coda::densplot(x, ...))
setMethod("densplot", "codaMCMC", function(x, ...) 
    densplot(as(x, "MCMClist"), ...))
setMethod("densplot", "dcmle", function(x, ...) 
    densplot(as(x, "MCMClist"), ...))

#setGeneric("pairs", function(x, ...) standardGeneric("pairs"))
setMethod("pairs", "MCMClist", function(x, ...) 
    dclone::pairs.mcmc.list(x, ...))
setMethod("pairs", "codaMCMC", function(x, ...) 
    pairs(as(x, "MCMClist")))
setMethod("pairs", "dcmle", function(x, ...) 
    pairs(as(x, "MCMClist")))

#setGeneric("densityplot", function(x, ...) standardGeneric("densityplot"))
setMethod("densityplot", "MCMClist", 
    function(x, data, ...) 
        densityplot(as(x, "mcmc.list"), data, ...))
setMethod("densityplot", "codaMCMC", 
    function(x, data, ...) 
        densityplot(as(x, "MCMClist"), data, ...))
setMethod("densityplot", "dcmle", 
    function(x, data, ...) 
        densityplot(as(x, "MCMClist"), data, ...))

#setGeneric("qqmath", function(x, ...) standardGeneric("qqmath"))
setMethod("qqmath", "MCMClist", 
    function(x, data, ...) 
        qqmath(as(x, "mcmc.list"), data, ...))
setMethod("qqmath", "codaMCMC", 
    function(x, data, ...) 
        qqmath(as(x, "MCMClist"), data, ...))
setMethod("qqmath", "dcmle", 
    function(x, data, ...) 
        qqmath(as(x, "MCMClist"), data, ...))

#setGeneric("xyplot", function(x, ...) standardGeneric("xyplot"))
setMethod("xyplot", "MCMClist", 
    function(x, data, ...) 
        xyplot(as(x, "mcmc.list"), data, ...))
setMethod("xyplot", "codaMCMC", 
    function(x, data, ...) 
        xyplot(as(x, "MCMClist"), data, ...))
setMethod("xyplot", "dcmle", 
    function(x, data, ...) 
        xyplot(as(x, "MCMClist"), data, ...))

#setGeneric("acfplot", function(x, ...) standardGeneric("acfplot"))
setMethod("acfplot", "MCMClist", 
    function(x, data, ...) 
        acfplot(as(x, "mcmc.list"), data, ...))
setMethod("acfplot", "codaMCMC", 
    function(x, data, ...) 
        acfplot(as(x, "MCMClist"), data, ...))
setMethod("acfplot", "dcmle", 
    function(x, data, ...) 
        acfplot(as(x, "MCMClist"), data, ...))

## this plots only mcmc (one chain at a time)
#setGeneric("levelplot", function(x, ...) standardGeneric("levelplot"))
#setMethod("levelplot", "MCMClist", 
#    function(x, ...) 
#        coda:::levelplot.mcmc.list(x, ...))
#setMethod("levelplot", "dcmle", 
#    function(x, ...) 
#        levelplot(as(x, "MCMClist"), data, ...))

setGeneric("crosscorr.plot", function(x, ...) 
    standardGeneric("crosscorr.plot"))
setMethod("crosscorr.plot", "dcmle", function(x, ...) 
    crosscorr.plot(as(x, "MCMClist"), ...))
setMethod("crosscorr.plot", "codaMCMC", function(x, ...) 
    crosscorr.plot(as(x, "MCMClist"), ...))
setMethod("crosscorr.plot", "MCMClist", function(x, ...) 
    coda::crosscorr.plot(x, ...))

setGeneric("gelman.plot", function(x, ...) 
    standardGeneric("gelman.plot"))
setMethod("gelman.plot", "dcmle", function(x, ...) 
    gelman.plot(as(x, "MCMClist"), ...))
setMethod("gelman.plot", "codaMCMC", function(x, ...) 
    gelman.plot(as(x, "MCMClist"), ...))
setMethod("gelman.plot", "MCMClist", function(x, ...) 
    coda::gelman.plot(x, ...))
