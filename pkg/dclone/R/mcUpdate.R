## need to test this: update works for the list or not
mcUpdate <-
function(object, n.iter = 1, ...) 
{
    n.clones <- nclones(object)
    if (!character(object))
        object <- deparse(substitute(object))
    cldata <- list(n.iter=n.iter, name=object)
    jagsparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- get(cldata$name, envir=.GlobalEnv)
        rjags:::update.jags(res[[i]], n.iter=cldata$n.iter, ...)
        res
    }
    mc <- length(object)
    rval <- snowWrapper(mc, 1:length(mc), jagsparallel, cldata,
        name=NULL, use.env=TRUE,
        balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = getwd(), ...)
    class(rval) <- "mc.jags"
    if (!is.null(n.clones) && n.clones > 1) {
        attr(rval, "n.clones") <- n.clones
    }
    assign(cldata$name, rval, envir=parent.frame())
    invisible(NULL)
}
