parUpdate <-
function(cl, object, n.iter = 1, ...) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (!is.character(object))
        object <- as.character(object) # deparse(substitute(object))
    cldata <- list(n.iter=n.iter, name=object)
    jagsparallel <- function(i, ...) {
        ## cldata is pushed by parDosa
        cldata <- pullDcloneEnv("cldata", type = "model")
        if (existsDcloneEnv(cldata$name, type = "results")) {
            res <- pullDcloneEnv(cldata$name,
                type = "results")
            rjags:::update.jags(res, n.iter=cldata$n.iter, ...)
            pushDcloneEnv(cldata$name, res, type = "results")
        }
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    parDosa(cl, 1:length(cl), jagsparallel, cldata,
        lib = c("dclone", "rjags"), balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload = FALSE, ...)
}
