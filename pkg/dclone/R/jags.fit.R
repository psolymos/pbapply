jags.fit <-
function(data, params, model, inits=NULL, n.chains=3, n.adapt=1000, n.update=0, thin=1, n.iter=5000, ...)
{
    ## inital steps
    n.clones <- nclones.list(data)
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(clean.jags.model(model))
    }
    ## handling inits arg, model initialization
    m <- if (is.null(inits)) {
        jags.model(model, data, n.chain=n.chains, n.adapt=n.adapt)
    } else {
        jags.model(model, data, inits, n.chain=n.chains, n.adapt=n.adapt)
    }
    ## model updating
    if (n.update > 0) {
        update(m, n.update, ...)
    }
    ## coda samples
    res <- coda.samples(m, params, n.iter=n.iter, thin=thin, ...)
    ## n.clones attr
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}

