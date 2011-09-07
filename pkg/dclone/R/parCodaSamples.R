parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=deparse(substitute(model)))
    jagsparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- get(cldata$name, envir=.GlobalEnv)
        n.clones <- nclones(res)
        res <- coda.samples(res, variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, ...)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
#            class(res) <- c("mcmc.list.dc", class(res))
        }
        res
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    res <- snowWrapper(cl, 1:length(cl), jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload=FALSE, ...)
    n.clones <- lapply(res, nclones)
    if (length(unique(unlist(n.clones))) != 1L) {
        n.clones <- NULL
        warnings("inconsistent 'n.clones' values, set to NULL")
    } else n.clones <- n.clones[[1]]
    for (i in 1:length(res)) {
        attr(res, "n.clones") <- NULL
#        class(res) <- class(res)[class(res) != "mcmc.list.dc"]
    }
    res <- as.mcmc.list(lapply(res, as.mcmc))
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

