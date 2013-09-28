parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    ## stop if rjags not found
    requireNamespace("rjags")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (!is.character(model))
        model <- as.character(model) # deparse(substitute(model))
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=model)
    jagsparallel <- function(i, ...) {
        cldata <- pullDcloneEnv("cldata", type = "model")
        if (!existsDcloneEnv(cldata$name, type = "results"))
            return(NULL)
        res <- pullDcloneEnv(cldata$name, type = "results")
        n.clones <- nclones(res)
        out <- rjags::coda.samples(res, variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, ...)
        ## jags model is pushed back to .env, mcmc.list is returned
        pushDcloneEnv(cldata$name, res, type = "results")
        if (!is.null(n.clones) && n.clones > 1) {
            attr(out, "n.clones") <- n.clones
        }
        out
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    res <- parDosa(cl, 1:length(cl), jagsparallel, cldata, 
        lib = c("dclone", "rjags"), balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload=FALSE, ...)
    res <- res[!sapply(res, is.null)]
    n.clones <- lapply(res, nclones)
    if (length(unique(unlist(n.clones))) != 1L) {
        n.clones <- NULL
        warnings("inconsistent 'n.clones' values, set to NULL")
    } else n.clones <- n.clones[[1]]
    for (i in 1:length(res)) {
        attr(res, "n.clones") <- NULL
    }
    res <- as.mcmc.list(lapply(res, as.mcmc))
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
