jags.parfit <-
function(cl, data, params, model, inits = NULL, n.chains = 3, ...)
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    trace <- getOption("dcoptions")$verbose
    ## eval args
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    ## write model
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        ## write model only if SOCK cluster (shared memory)
        if (inherits(cl, "SOCKcluster")) {
            model <- write.jags.model(model)
            on.exit(try(clean.jags.model(model)))
        }
    }
    ## generating initial values and RNGs if needed
    if ("lecuyer" %in% list.modules()) {
        mod <- parListModules(cl)
        for (i in 1:length(mod)) {
            if (!("lecuyer" %in% mod[[i]]))
                stop("'lecuyer' module must be loaded on workers")
        }
    }
    inits <- parallel.inits(inits, n.chains)
#    inits <- jags.fit(data, params, model, inits, n.chains,
#        n.adapt=0, n.update=0, n.iter=0)$state(internal=TRUE)
    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel function to evaluate by snowWrapper
    jagsparallel <- function(i, ...)   {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        jags.fit(data=cldata$data, params=cldata$params, 
            model=cldata$model, 
            inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcoptions")$LB)
        "load" else "none"
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        lib="dclone", balancing=balancing, size=1, 
        rng.type=getOption("dcoptions")$RNG, cleanup=TRUE, dir=dir, 
        unload=FALSE, ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))
    ## attaching attribs and return
    n.clones <- nclones(data)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
