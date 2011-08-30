parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=deparse(substitute(model)))
    jagsparallel <- function(i, ...) {
        res <- get(.DcloneEnv$name, envir=.GlobalEnv)
        res <- coda.samples(res, variable.names=.DcloneEnv$variable.names,
            n.iter=.DcloneEnv$n.iter, thin=.DcloneEnv$thin, ...)
        res
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    res <- snowWrapper(cl, 1:length(cl), jagsparallel, cldata, name=NULL,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = FALSE, dir = dir, unload=FALSE, ...)
    as.mcmc.list(lapply(res, as.mcmc))
}

