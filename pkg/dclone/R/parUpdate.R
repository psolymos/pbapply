parUpdate <-
function(cl, object, n.iter = 1, ...) 
{
    cldata <- list(n.iter=n.iter, name=deparse(substitute(object)))
    jagsparallel <- function(i, ...) {
        res <- get(.DcloneEnv$name, envir=.GlobalEnv)
        rjags:::update.jags(res, n.iter=.DcloneEnv$n.iter, ...)
        assign(.DcloneEnv$name, res, envir=.GlobalEnv)
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    snowWrapper(cl, 1:length(cl), jagsparallel, cldata, name=NULL,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = FALSE, dir = dir, unload=FALSE, ...)
}

