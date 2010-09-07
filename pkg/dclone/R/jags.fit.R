jags.fit <-
function(data, params, model, inits=NULL, n.chains=3, n.adapt=1000, n.update=0, thin=1, n.iter=5000, 
updated.model=TRUE, ...)
{
    ## inital steps
    n.clones <- nclones.list(data)
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }
    ## handling inits arg, model initialization
    m <- if (is.null(inits)) {
        jags.model(model, data, n.chain=n.chains, n.adapt=n.adapt)
    } else {
        jags.model(model, data, inits, n.chain=n.chains, n.adapt=n.adapt)
    }
    if (is.null(list(...)$progress.bar)) {
        trace <- if (getOption("dcoptions")$verbose)
            getOption("jags.pb") else "none"
    } else trace <- list(...)$progress.bar
    byval <- if (!is.null(list(...)$by))
        list(...)$by else floor(min(n.iter/50, 100))
    ## model updating
    if (n.update > 0) {
        update(m, n.update, progress.bar=trace, by=byval)
    }
    ## coda samples
    if (n.iter > 0) {
        res <- coda.samples(m, params, n.iter=n.iter, thin=thin, 
            progress.bar=trace, by=byval)
    } else {
        return(m)
    }
    ## jags.model attribute
    if (updated.model)
        attr(res, "updated.model") <- m
    ## n.clones attr
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

