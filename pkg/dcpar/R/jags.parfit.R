jags.parfit <-
function(cl, data, params, model, inits, n.chains = 3, ...)
{
    ## eval args
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
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
    ## parallel function to evaluate by cluster.wrapper
    jagsparallel <- function(i, ...)   {
        jags.fit(data=cldata$data, params=cldata$params, model=cldata$model, 
        inits=cldata$inits[[i]], n.chains=1, ...)
    }
    ## common data
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel computations
    mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, lib="dcpar", 
        load.balancing=getOption("dclone.cluster")$load.balancing, size=1, ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))
    ## updated models
    if (!is.null(attr(mcmc[[1]], "updated.model"))) {
        attr(res, "updated.model") <- lapply(mcmc, function(z) attr(z, "updated.model"))
    }
    ## attaching attribs and return
    n.clones <- nclones(data)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

