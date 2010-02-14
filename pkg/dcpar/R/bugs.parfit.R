bugs.parfit0 <-
function(cl, data, params, model, inits, n.chains = 3, bugs.seed, ...)
{
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    trace <- getOption("dclone.verbose")
    ## eval args
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (!is.null(list(...)$format) && list(...)$format == "bugs")
        stop("cant't use parallel with 'format=\"bugs\"'")
    if (missing(bugs.seed))
        bugs.seed <- 100*1:n.chains
    if (length(unique(bugs.seed)) == 1)
        stop("provide different seed values for each chain")
    if (missing(inits))
        stop("provide initial values")
    if (is.function(inits))
        inits <- lapply(1:n.chains, inits)
    if (length(inits) != n.chains)
        stop("provide initial values for each chains")
    ## writing data file
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }
    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel function to evaluate by snowWrapper
    bugsparallel <- function(i, ...)   {
        bugs.fit(data=cldata$data, params=cldata$params, model=cldata$model,
        inits=cldata$inits[i], n.chains=1, bugs.seed=bugs.seed[i], ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcpar.LB"))
        "load" else "none"
    rng <- c("Wichmann-Hill", "Marsaglia-Multicarry",
        "Super-Duper", "Mersenne-Twister")
    mcmc <- snowWrapper(cl, 1:n.chains, bugsparallel, cldata, lib="dcpar", 
        balancing=balancing, size=1, seed=1000*1:length(cl), 
        kind=rng[1:length(cl)], dir=getwd(), ...)

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

