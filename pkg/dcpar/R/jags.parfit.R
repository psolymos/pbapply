jags.parfit <-
function(cl, data, params, model, inits, n.chains = 3, ...)
{
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    ## eval args
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (n.chains > 4) {
        if (missing(inits))
            stop("provide initial values")
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
        if (missing(inits)) {
            initsval <- suppressWarnings(jags.fit(data, params, model, 
                n.adapt=1, n.iter=1, updated.model=FALSE))
            flush.console()
            inits <- lapply(lapply(initsval, as.list), function(z) {
                names(z) <- varnames(initsval)
                z})
            rng <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
                "base::Super-Duper", "base::Mersenne-Twister")
            seed <- 99*1:n.chains
            for (i in 1:n.chains) {
                inits[[i]][[".RNG.name"]] <- rng[i]
                inits[[i]][[".RNG.seed"]] <- seed[i]
            }
        }
    }
    ## common data to cluster
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model0 <- match.fun(model)
        modnam <- paste("model.cl", 1:n.chains, ".bug", sep="")
        model <- rep("", n.chains)
        for (i in 1:n.chains) {
            model[i] <- write.jags.model(model0, modnam[i])
        }
        on.exit(try(clean.jags.model(model)))
    } else {
        if (length(model) != n.chains)
            stop("provide 'n.chains' number of model files")
    }
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel function to evaluate by snowWrapper
    jagsparallel <- function(i, ...)   {
        jags.fit(data=cldata$data, params=cldata$params, model=cldata$model[[i]], 
        inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
    }
    ## parallel computations
    mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, lib="dcpar", 
        balancing="none", size=1, seed=100*1:length(cl), ...)
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

