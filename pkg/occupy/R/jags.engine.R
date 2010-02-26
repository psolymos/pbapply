jags.engine <-
function(data, params, model, inits=NULL, cl=NULL, ...)
{
    ## basic settings
    nam <- c("n.chains", "n.adapt", "n.update", "thin", "n.iter")
    val <- c(3, 1000, 0, 1, 5000)
    dots <- list(...)
    z <- lapply(1:5, function(i) {
        if (is.null(dots[[nam[i]]]))
            val[i] else dots[[nam[i]]]
    })
    names(z) <- nam
    ## sequential chains
    if (is.null(cl)) {
        if (!(z$n.chains %in% 1:4))
            stop("'n.chains' must be in 1:4")
#        require(dclone)
        rval <- jags.fit(data, params, model, inits, 
            n.chains=z$n.chains, n.adapt=z$n.adapt, 
            n.update=z$n.update, thin=z$thin, n.iter=z$n.iter,
            updated.model=FALSE)
    ## parallel chains
    } else {
        if (!(z$n.chains %in% 2:4))
            stop("'n.chains' must be in 2:4 for parallel computations")
        require(dcpar)
        if (is.null(inits))
            inits <- lapply(1:z$n.chains, function(i) list())
        rng <- c("base::Wichmann-Hill", "base::Marsaglia-Multicarry",
            "base::Super-Duper", "base::Mersenne-Twister")
        for (i in 1:z$n.chains) {
            inits[[i]][[".RNG.name"]] <- rng[i]
            inits[[i]][[".RNG.seed"]] <- 999*i
        }
        rval <- jags.parfit(cl, data, params, model, inits, 
            n.chains=z$n.chains, n.adapt=z$n.adapt, 
            n.update=z$n.update, thin=z$thin, n.iter=z$n.iter)
    }
    rval
}
