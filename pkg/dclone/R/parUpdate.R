parUpdate <-
function(cl, object, n.iter = 1, ...) 
{
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (!character(object))
        object <- deparse(substitute(object))
    cldata <- list(n.iter=n.iter, name=object)
    jagsparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- get(cldata$name, envir=.GlobalEnv)
        rjags:::update.jags(res, n.iter=cldata$n.iter, ...)
        assign(cldata$name, res, envir=.GlobalEnv)
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    snowWrapper(cl, 1:length(cl), jagsparallel, cldata,
        name=NULL, use.env=TRUE,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload = FALSE, ...)
}

