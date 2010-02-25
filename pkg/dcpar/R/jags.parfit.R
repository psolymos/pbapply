jags.parfit <-
function(cl, data, params, model, inits, n.chains = 3, ...)
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
    rng <- c("Wichmann-Hill", "Marsaglia-Multicarry",
        "Super-Duper", "Mersenne-Twister")

    if (n.chains > 4 && missing(inits))
        stop("provide initial values")

    if (!missing(inits)) {
        if (is.function(inits))
            inits <- lapply(1:n.chains, inits)
        if (length(inits) != n.chains)
            stop("provide initial values for each chains")
        if (!all(c(".RNG.name", ".RNG.seed") %in% unique(unlist(lapply(inits, names)))))
            stop("'.RNG.name' and '.RNG.seed' is missing from 'inits'")
        if (length(unique(sapply(inits, function(z) z[[".RNG.seed"]]))) == 1)
            stop("provide different '.RNG.seed' for each chain")
    } else {
        ## generating initial values
        inits <- jags.fit(data, params, model, inits=NULL, n.chains,
            n.adapt=0, n.update=0, n.iter=0)$state()
        seed <- 999*1:n.chains
        for (i in 1:n.chains) {
            inits[[i]][[".RNG.name"]] <- paste("base::", rng[i], sep="")
            inits[[i]][[".RNG.seed"]] <- seed[i]
        }
    }

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
    jagsparallel <- function(i, ...)   {
        jags.fit(data=cldata$data, params=cldata$params, 
        model=cldata$model, 
        ## model=cldata$model[[i]], 
        inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcpar.LB"))
        "load" else "none"
    mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, lib="dcpar", 
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

