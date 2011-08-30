parJagsModel <-
function(cl, name, file, data=sys.frame(sys.parent()), 
inits, n.chains = 1, n.adapt=1000, quiet=FALSE) 
{
    if (length(cl) != n.chains)
        stop("length(cl) must equal n.chains")
    inits <- jags.model(file, data, inits, n.chains, 
        n.adapt = 0)$state(internal = TRUE)
    cldata <- list(file=file, data=as.list(data), inits=inits,
        n.adapt=n.adapt, name=deparse(substitute(name)), quiet=quiet)
    jagsparallel <- function(i) {
        res <- jags.model(file=cldata$file, data=cldata$data, 
            inits=cldata$inits[[i]], n.chains=1,
            n.adapt=cldata$n.adapt, quiet=cldata$quiet)
        assign(cldata$name, res, envir=.GlobalEnv)
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    snowWrapper(cl, 1:n.chains, jagsparallel, cldata, 
        lib = "rjags", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = FALSE, dir = dir)
}

