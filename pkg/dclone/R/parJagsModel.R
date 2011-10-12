parJagsModel <-
function(cl, name, file, data = sys.frame(sys.parent()), 
inits, n.chains = 1, n.adapt = 1000, quiet = FALSE) 
{
    if (length(cl) != n.chains)
        stop("length(cl) must equal n.chains")
    if (is.function(file) || inherits(file, "custommodel")) {
        if (is.function(file))
            file <- match.fun(file)
        file <- write.jags.model(file)
        on.exit(try(clean.jags.model(file)))
    }
    n.clones <- dclone:::nclones.list(as.list(data))
    inits <- jags.model(file, data, inits, n.chains, 
        n.adapt = 0)$state(internal = TRUE)
    cldata <- list(file=file, data=as.list(data), inits=inits,
        n.adapt=n.adapt, name=deparse(substitute(name)), quiet=quiet,
        n.clones=n.clones)
    jagsparallel <- function(i) {
        cldata <- as.list(get(".DcloneEnv", envir=.GlobalEnv))
        res <- jags.model(file=cldata$file, data=cldata$data, 
            inits=cldata$inits[[i]], n.chains=1,
            n.adapt=cldata$n.adapt, quiet=cldata$quiet)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
        assign(cldata$name, res, envir=.GlobalEnv)
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    snowWrapper(cl, 1:n.chains, jagsparallel, cldata, 
        name=NULL, use.env=TRUE,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload=FALSE)
}
