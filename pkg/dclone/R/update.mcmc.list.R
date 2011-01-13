update.mcmc.list <-
function(object, fun, times = 1, n.update = 0, 
n.iter, thin, ...)
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    ## eval of args
    if (is.null(updated.model(object)))
        stop("updated model not found")
    mod <- object
    ## update can go even if fun is missing
    if (missing(fun))
        fun <- function(z) FALSE
    fun <- match.fun(fun)
    fval <- fun(mod)
    if (!is.logical(fval))
        stop("'fun' must return logical")
    if (length(fval) > 1)
        stop("'fun' must return a single value")
    if (fval)
        return(object)
    ## what to sample
    params <- varnames(mod)
    ## missing args
    if (missing(thin))
        thin <- coda:::thin(object)
    if (missing(n.iter))
        n.iter <- niter(object) * coda:::thin(object)
    ## n.update/n.iter vs. times
    if (length(n.update) < times)
        n.update <- rep(n.update, times)[1:times]
    if (length(n.iter) < times)
        n.iter <- rep(n.iter, times)[1:times]
    ## auto-updating
    for (i in 1:times) {
        if (n.update[i] > 0)
            update(updated.model(object), n.update[i], ...)
        mod <- coda.samples(updated.model(object), params, n.iter[i], thin, ...)
        if (fun(mod))
            break
    }
    ## put things together
    attr(mod, "updated.model") <- updated.model(object)
    n.clones <- nclones(object)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(mod, "n.clones") <- n.clones
        class(mod) <- c("mcmc.list.dc", class(mod))
    }
    mod
}
