parJagsModel <-
function(cl, name, file, data=sys.frame(sys.parent()), 
inits, n.chains = 1, n.adapt=1000, quiet=FALSE) 
{
    if (length(cl) != n.chains)
        stop("length(cl) must equal n.chains")
    if (is.function(file) || inherits(file, "custommodel")) {
        if (is.function(file))
            file <- match.fun(file)
        file <- write.jags.model(file)
        on.exit(try(clean.jags.model(file)))
    }
    inits <- jags.model(file, data, inits, n.chains, 
        n.adapt = 0)$state(internal = TRUE)
    cldata <- list(file=file, data=as.list(data), inits=inits,
        n.adapt=n.adapt, name=deparse(substitute(name)), quiet=quiet)
#    .DcloneEnv <- new.env(hash = FALSE, parent = .GlobalEnv)
    jagsparallel <- function(i) {
        .DcloneEnv <- get(".DcloneEnv", envir=.GlobalEnv)
        res <- jags.model(file=.DcloneEnv$file, data=.DcloneEnv$data, 
            inits=.DcloneEnv$inits[[i]], n.chains=1,
            n.adapt=.DcloneEnv$n.adapt, quiet=.DcloneEnv$quiet)
        assign(.DcloneEnv$name, res, envir=.GlobalEnv)
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    snowWrapper(cl, 1:n.chains, jagsparallel, cldata, name=NULL,
        lib = "dclone", balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = FALSE, dir = dir, unload=FALSE)
}
