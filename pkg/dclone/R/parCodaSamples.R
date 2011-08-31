parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=deparse(substitute(model)))
    jagsparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- get(cldata$name, envir=.GlobalEnv)
        res <- coda.samples(res, variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, ...)
        res
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    res <- snowWrapper(cl, 1:length(cl), jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload=FALSE, ...)
    as.mcmc.list(lapply(res, as.mcmc))
}

