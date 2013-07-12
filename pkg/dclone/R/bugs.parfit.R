bugs.parfit <-
function(cl, data, params, model, inits=NULL, 
n.chains = 3, seed,
program=c("winbugs", "openbugs", "brugs"), ...) ## only mcmc.list format is supported when cl is not NULL
{
    ## get defaults right for cl argument
    cl <- evalParallelArgument(cl, quit=TRUE)
    ## sequential evaluation falls back on jags.fit
    if (is.null(cl)) {
        return(bugs.fit(data, params, model, 
            inits = inits, n.chains = n.chains, seed=seed, ...))
    }
    ## parallel evaluation starts here
    ## not case sensitive evaluation of program arg
    program <- match.arg(tolower(program), c("winbugs", "openbugs", "brugs"))

    if (program %in% c("winbugs", "brugs") && !suppressWarnings(require(R2WinBUGS)))
        stop("there is no package called 'R2WinBUGS'")
    if (program == "brugs" && !suppressWarnings(require(BRugs)))
        stop("there is no package called 'BRugs'")
    if (program == "openbugs" && !suppressWarnings(require(R2OpenBUGS)))
        stop("there is no package called 'R2OpenBUGS'")

    if (is.environment(data)) {
        warnings("'data' was environment: it was coerced into a list")
        data <- as.list(data)
    }
    if (!is.null(list(...)$format))
        if (list(...)$format == "bugs")
            stop("only 'mcmc.list' format is supported for parallel parallel computations")

    if (n.chains == 1)
        stop("no need for parallel computing with 1 chain")
    if (missing(seed))
        stop("'seed' must be provided")
    if (is.null(seed))
        stop("'seed' must be not be NULL")
    ## if all seed are the same
    if (length(unique(seed)) != n.chains)
        stop("provide 'seed' for each chain")
    if (length(unique(seed)) != n.chains)
        stop("'seed' must have 'n.chains' unique values")

    trace <- getOption("dcoptions")$verbose
    ## retrieves n.clones
    n.clones <- dclone:::nclones.list(data)
    ## removes n.clones attr from each element of data
    data <- lapply(data, function(z) {
        attr(z, "n.clones") <- NULL
        z
    })

    ## write model
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        ## write model only if SOCK cluster or multicore (shared memory)
        if (is.numeric(cl) || inherits(cl, "SOCKcluster")) {
            model <- write.jags.model(model)
            on.exit(try(clean.jags.model(model)))
        }
    }

    
    if (is.null(inits))
        inits <- lapply(1:n.chains, function(i) NULL)
    if (is.function(inits))
        inits <- lapply(1:n.chains, function(i) inits)
    if (length(inits) != n.chains)
        stop("inits length incompatible with n.chains")

    ## common data to cluster
    cldata <- list(data=data, params=params, model=model, inits=inits, 
        seed=seed, program=program)
    ## parallel function to evaluate by snowWrapper
    bugsparallel <- function(i, ...)   {
        cldata <- pullDcloneEnv("cldata", type = "model")
        bugs.fit(data=cldata$data, params=cldata$params, 
            model=cldata$model, 
            inits=cldata$inits[[i]], n.chains=1, 
            seed=cldata$seed[i], 
            program=cldata$program, format="mcmc.list", ...)
    }
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel computations
    balancing <- if (getOption("dcoptions")$LB)
        "load" else "none"
    dir <- if (inherits(cl, "SOCKcluster"))
        getwd() else NULL
    LIB <- c("dclone")
    if (program == "winbugs")
        LIB <- c(LIB, "R2WinBUGS")
    if (program == "brugs")
        LIB <- c(LIB, "R2WinBUGS", "BRugs")
    if (program == "openbugs")
        LIB <- c(LIB, "R2OpenBUGS")

    mcmc <- parDosa(cl, 1:n.chains, bugsparallel, cldata, 
        lib=LIB, 
        balancing=balancing, size=1, 
        rng.type=getOption("dcoptions")$RNG, cleanup=TRUE, dir=dir, 
        unload=FALSE, ...)
    ## binding the chains
    res <- as.mcmc.list(lapply(mcmc, as.mcmc))

    ## adding n.clones attribute, and class attr if mcmc.list
    if (!is.null(n.clones) && n.clones > 1) {
        attr(res, "n.clones") <- n.clones
        class(res) <- c("mcmc.list.dc", class(res))
    }
    if (program == "winbugs")
        warnings("seed settings in WinBUGS are not best suited for parallel computations")
    if (program == "brugs")
        warning("program = 'openbugs' is the preferred interface")
    res
}
