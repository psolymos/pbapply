codaSamples <-
function(model, variable.names = NULL, n.iter, thin = 1, ...) 
{
    n.clones <- nclones(model)
    res <- coda.samples(model=model, variable.names=variable.names,
        n.iter=n.iter, thin=thin, ...)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    res
}
