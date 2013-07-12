parJagsModel <-
function(cl, name, file, data = sys.frame(sys.parent()), 
inits, n.chains = 1, n.adapt = 1000, quiet = FALSE) 
{
    ## stop if rjags not found
    if (!suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    cl <- evalParallelArgument(cl, quit=TRUE)
    if (!inherits(cl, "cluster"))
        stop("cl must be of class 'cluster'")
    if (length(cl) < n.chains)
        stop("length(cl) < n.chains")
    if (is.function(file) || inherits(file, "custommodel")) {
        if (is.function(file))
            file <- match.fun(file)
        if (inherits(cl, "SOCKcluster")) {
            file <- write.jags.model(file)
            on.exit(try(clean.jags.model(file)))
        }
    }
    n.clones <- dclone:::nclones.list(as.list(data))
    ## inits and RNGs
    if ("lecuyer" %in% list.modules()) {
        mod <- parListModules(cl)
        for (i in 1:length(mod)) {
            if (!("lecuyer" %in% mod[[i]]))
                stop("'lecuyer' module must be loaded on workers")
        }
    }
    inits <- if (missing(inits))
        parallel.inits(n.chains=n.chains) else parallel.inits(inits, n.chains)
#    inits <- jags.model(file, data, inits, n.chains, 
#        n.adapt = 0)$state(internal = TRUE)
    if (!is.character(name))
        name <- as.character(name) # deparse(substitute(name))
    cldata <- list(file=file, data=as.list(data), inits=inits,
        n.adapt=n.adapt, name=name, quiet=quiet,
        n.adapt=n.adapt, quiet=quiet,
        n.clones=n.clones)
    jagsparallel <- function(i) {
        cldata <- pullDcloneEnv("cldata", type = "model")
        res <- jags.model(file=cldata$file, data=cldata$data, 
            inits=cldata$inits[[i]], n.chains=1,
            n.adapt=cldata$n.adapt, quiet=cldata$quiet)
        if (!is.null(n.clones) && n.clones > 1) {
            attr(res, "n.clones") <- n.clones
        }
        pushDcloneEnv(cldata$name, res, type = "results")
        NULL
    }
    dir <- if (inherits(cl, "SOCKcluster")) 
        getwd() else NULL
    parDosa(cl, 1:n.chains, jagsparallel, cldata, 
        lib = c("dclone", "rjags"), balancing = "none", size = 1, 
        rng.type = getOption("dcoptions")$RNG, 
        cleanup = TRUE, dir = dir, unload=FALSE)
}
