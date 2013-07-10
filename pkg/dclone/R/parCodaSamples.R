parCodaSamples <-
function(cl, model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (!is.character(model))
        model <- as.character(model) # deparse(substitute(model))
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=model)
    jagsparallel <- function(i, ...) {
        if (!exists(cldata$name, envir=.parDcloneEnv))
            return(NULL)
        cldata <- as.list(get(".DcloneEnv", envir=globalenv()))
        res <- get(cldata$name, envir=.parDcloneEnv)
        n.clones <- nclones(res)
        out <- coda.samples(res, variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, ...)
        assign(cldata$name, out, envir=.parDcloneEnv)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(out, "n.clones") <- n.clones
        }
        out
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    res <- snowWrapper(cl, 1:length(cl), jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        lib = "dclone", balancing = "none", size = 1, 
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
