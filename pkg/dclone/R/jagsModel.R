jagsModel <-
function(file, data=sys.frame(sys.parent()), 
inits, n.chains = 1, n.adapt=1000, quiet=FALSE) 
{
    if (is.function(file) || inherits(file, "custommodel")) {
        if (is.function(file))
            file <- match.fun(file)
        file <- write.jags.model(file)
        on.exit(try(clean.jags.model(file)))
    }
    res <- jags.model(file=file, data=data, 
        inits=inits, n.chains=n.chains,
        n.adapt=n.adapt, quiet=quiet)
    n.clones <- dclone:::nclones.list(data)
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
    }
    res
}
