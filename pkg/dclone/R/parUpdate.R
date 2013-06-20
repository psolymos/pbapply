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
#        if (exists(cldata$name, envir=.GlobalEnv)) {
        if (exists(cldata$name, envir=globalenv())) {
#            cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
            cldata <- as.list(get(".DcloneEnv", envir=globalenv()))
#            res <- get(cldata$name, envir=.GlobalEnv)
            res <- get(cldata$name, envir=globalenv())
            rjags:::update.jags(res, n.iter=cldata$n.iter, ...)
#            assign(cldata$name, res, envir=.GlobalEnv)
            assign(cldata$name, res, envir=globalenv())
        }
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

