bugs.pfit <-
function(cl, data, params, model, inits, n.chains = 3, bugs.seed=1:n.chains, ...)
{
    ## eval args
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (!is.null(list(...)$format) && list(...)$format == "bugs")
        stop("cant't use parallel with 'format=\"bugs\"'")
    if (missing(bugs.seed))
        stop("provide seed values")
    if (length(bugs.seed) != n.chains)
        stop("provide seed values for each chains")
    if (length(unique(bugs.seed)) == 1)
        stop("provide different seed values for each chains")
    if (missing(inits))
        stop("provide initial values")
    if (length(inits) != n.chains)
        stop("provide initial values for each chains")
    ## parallel function to evaluate by cluster.wrapper
    bugsparallel <- function(i, ...)   {
        jags.fit(data=cldata$data, params=cldata$params, model=cldata$model,
        inits=cldata$inits[[i]], n.chains=1, bugs.seed=bugs.seed[[i]], ...)
    }
    ## common data
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel computations
    mcmc <- cluster.wrapper(cl, 1:n.chains, bugsparallel, cldata, lib="dclone", 
        load.balancing=getOption("dclone.cluster")$load.balancing, ...)
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
