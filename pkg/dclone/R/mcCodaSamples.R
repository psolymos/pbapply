mcCodaSamples <-
function(model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    if (!character(model))
        model <- deparse(substitute(model))
    cldata <- list(variable.names=variable.names,
        n.iter=n.iter, thin=thin, name=model)
    jagsparallel <- function(i, ...) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- get(cldata$name, envir=.GlobalEnv)
        n.clones <- nclones(res)
        out <- coda.samples(res[[i]], variable.names=cldata$variable.names,
            n.iter=cldata$n.iter, thin=cldata$thin, ...)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
        attach(out, "updated.model") <- res
        out
    }
    mc <- length(model)
    res <- snowWrapper(mc, 1:length(mc), jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = getwd(), ...)
    um <- lapply(res, updated.model)
    class(um) <- "mc.jags"
    n.clones <- list(nclones(model), lapply(res, nclones))
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
        attr(um, "n.clones") <- n.clones
    }
    assign(deparse(substitute(model)), um, envir=parent.frame())
    res
}
