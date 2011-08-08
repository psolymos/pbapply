jags.engine <-
function(data, params, model, inits=NULL, cl=NULL, ...)
{
    require(dclone)
    ## basic settings
    val <- unlist(getOption("detect.mcmc.control"))
    nam <- names(val)
#    nam <- c("n.chains", "n.adapt", "n.update", "thin", "n.iter")
#    val <- c(3, 1000, 0, 1, 5000)
    dots <- list(...)
    z <- lapply(1:5, function(i) {
        if (is.null(dots[[nam[i]]]))
            val[i] else dots[[nam[i]]]
    })
    names(z) <- nam
    ## sequential chains
    if (is.null(cl)) {
        rval <- jags.fit(data, params, model, inits, 
            n.chains=z$n.chains, n.adapt=z$n.adapt, 
            n.update=z$n.update, thin=z$thin, n.iter=z$n.iter,
            updated.model=FALSE)
    ## parallel chains
    } else {
        rval <- jags.parfit(cl, data, params, model, inits, 
            n.chains=z$n.chains, n.adapt=z$n.adapt, 
            n.update=z$n.update, thin=z$thin, n.iter=z$n.iter)
    }
    rval
}

