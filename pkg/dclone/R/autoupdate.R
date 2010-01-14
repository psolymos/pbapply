autoupdate <-
function(object, fun, times, n.update = 0, 
n.iter = niter(object) * thin(object), thin = thin(object), ...)
{
    ## eval of args
    upm <- updated.model(object)
    if (is.null(upm))
        stop("updated model not found")
    mod <- object
    attr(mod, "updated.model") <- NULL
    ## update can go even if fun is missing
    if (missing(fun))
        fun <- function(z) FALSE
    fun <- match.fun(fun)
    fval <- fun(mod)
    if (!is.logical(fval))
        stop("'fun' must return logical")
    if (fval)
        return(object)
    ## what to sample
    params <- varnames(mod)
    ## n.update/n.iter vs. times
    if (length(n.update) < times)
        n.update <- rep(n.update, times)[1:times]
    if (length(n.iter) < times)
        n.iter <- rep(n.iter, times)[1:times]
    ## auto-updating
    for (i in 1:times) {
        if (i > 0)
            update(upm, n.update[i], ...)
        mod <- coda.samples(upm, params, n.iter[i], thin, ...)
        if (fun(mod))
            break
    }
    ## put things together
    attr(mod, "updated.model") <- upm
    n.clones <- nclones(object)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(mod, "n.clones") <- n.clones
        class(mod) <- c("mcmc.list.dc", class(mod))
    }
    mod
}

