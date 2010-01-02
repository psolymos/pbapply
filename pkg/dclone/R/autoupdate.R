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
    fun <- match.fun(fun)
    fval <- fun(mod)
    if (!is.logical(fval))
        stop("'fun' must return logical")
    if (fval)
        return(object)
    ## what to sample
    params <- varnames(mod)
    ## n.update vs. times eval
    if (length(n.update) == 1)
        n.update <- rep(n.update, times)
    ## auto-updating
    for (i in n.update) {
        if (i > 0)
            update(upm, i, ...)
        mod <- coda.samples(upm, params, n.iter, thin, ...)
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

