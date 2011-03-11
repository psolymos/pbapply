jags.parfit <-
function(cl, data, params, model, inits = NULL, n.chains = 3, ...)
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    trace <- getOption("dcoptions")$verbose
    ## eval args
    if (!is.null(list(...)$n.iter))
        if (list(...)$n.iter == 0)
            stop("'n.iter = 0' is not supported for parallel computations")
    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")

    ## write model
    if (dcoptions()$single.par.model) {
        if (is.function(model) || inherits(model, "custommodel")) {
            if (is.function(model))
                model <- match.fun(model)
            Model <- write.jags.model(model)
            on.exit(try(clean.jags.model(Model)))
        }
        model <- rep(Model, n.chains)
    } else {
        writefun <- function(i) {
            if (is.function(clmodel) || inherits(clmodel, "custommodel")) {
                if (is.function(clmodel))
                    model <- match.fun(clmodel)
                model <- write.jags.model(model, paste("clmodel", i, ".bug", sep=""))
            }
            model
        }
        model <- unlist(snowWrapper(cl, 1:length(cl), writefun, 
            cldata=model, name="clmodel", lib="dclone"))
        on.exit(try(parLapply(cl, model, clean.jags.model)))
    }

    ## generating initial values and RNGs if needed
    inits <- jags.fit(data, params, model[1], inits, n.chains,
        n.adapt=0, n.update=0, n.iter=0)$state(internal=TRUE)
    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits)
    ## parallel function to evaluate by snowWrapper
    jagsparallel <- function(i, ...)   {
        jags.fit(data=cldata$data, params=cldata$params, 
            model=cldata$model[i], 
            inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcoptions")$LB)
        "load" else "none"
    mcmc <- snowWrapper(cl, 1:n.chains, jagsparallel, cldata, lib="dclone", 
        balancing=balancing, size=1, dir=getwd(), rng.type=getOption("dcoptions")$RNG, ...)
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

