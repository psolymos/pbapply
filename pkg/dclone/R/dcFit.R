.dcFit <- 
function(data, params, model, inits, n.clones, multiply = NULL, unchanged = NULL, 
update = NULL, updatefun = NULL, initsfun = NULL, flavour = c("jags", "bugs"), 
n.chains=3, cl = NULL, parchains = FALSE, ...)
{
    flavour <- match.arg(flavour)
    ## stop if rjags not found
    if (flavour=="jags" && !suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    if (parchains && flavour=="bugs")
        stop("flavour='bugs' not supported with parallel chains")
    if (parchains && is.null(cl))
        stop("cl cannot be NULL with parchains=TRUE")
    ## initail evals
    if (missing(n.clones))
        stop("'n.clones' argument must be provided")
    if (identical(n.clones, 1))
        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    ## determine k
    k <- n.clones[order(n.clones)]
    k <- unique(k)
    times <- length(k)
    rhat.crit <- getOption("dcoptions")$rhat
    trace <- getOption("dcoptions")$verbose
    ## evaluate updating
    if (!is.null(update) != !is.null(updatefun))
        stop("both 'update' and 'updatefun' must be provided")
    if (!is.null(update)) {
        unchanged <- unique(c(unchanged, update))
        updatefun <- match.fun(updatefun)
        UPARGS <- length(names(formals(updatefun))) < 2
    }
    ## evaluate inits
    if (missing(inits))
        inits <- NULL
    if (!is.null(initsfun)) {
        initsfun <- match.fun(initsfun)
        ian <- length(names(as.list(args(initsfun))))-1
        if (ian == 0)
            stop("'initsfun' must have at least one argument")
        if (ian > 2)
            warnings("arguments of 'initsfun' after position 2 are ingnored")
        INIARGS <- ian < 2
    }
    ## list for dcdiag results
    dcdr <- list()
    ## iteration starts here
    for (i in 1:times) {
        tmpch <- if (k[i] == 1) "clone" else "clones"
        if (trace) {
            cat("\nFitting model with", k[i], tmpch, "\n\n")
            flush.console()
        }
        jdat <- dclone(data, k[i], multiply=multiply, unchanged=unchanged)
        if (flavour == "jags") {
            if (parchains) {
                mod <- jags.parfit(cl, jdat, params, model, inits, n.chains, ...)
            } else {
                mod <- jags.fit(jdat, params, model, inits, n.chains, ...)
            }
        } else {
            mod <- bugs.fit(jdat, params, model, inits, 
                n.chains=n.chains, format="mcmc.list", ...)
        }
        ## dctable evaluation
        if (i == 1) {
            vn <- varnames(mod)
            dcts <- list()
            ## note: quantiles must remain unchanged, because these values are
            ## defined in extractdctable.default
            quantiles <- c(0.025, 0.25, 0.5, 0.75, 0.975)
            dcts0 <- matrix(0, times, 4 + length(quantiles))
            dcts0[,1] <- k
            colnames(dcts0) <- c("n.clones", "mean", "sd", names(quantile(0, probs=quantiles)), "r.hat")
            for (j in 1:length(vn))
                dcts[[vn[j]]] <- dcts0
        }
        ## updating
        if (i < times) {
            if (!is.null(update))
                jdat[[update]] <- if (UPARGS) 
                    updatefun(mod) else updatefun(mod, k[i+1])
            if (!is.null(initsfun))
                inits <- if (INIARGS)
                    initsfun(mod) else initsfun(mod, k[i+1])
        }
        dctmp <- dclone:::extractdctable.default(mod)
        dcdr[[i]] <- dclone:::extractdcdiag.default(mod)
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[j,]
        }
    }
    ## warning if R.hat < crit
    rhat.problem <- any(dctmp[,"r.hat"] >= rhat.crit)
    if (any(is.na(rhat.problem))) {
        rhat.problem[is.na(rhat.problem)] <- FALSE
    }
    if (nchain(mod) > 1 && rhat.problem)
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    dcts <- lapply(dcts, function(z) as.data.frame(z))
    class(dcts) <- "dctable"
    attr(mod, "dctable") <- dcts
    ## finalizing dcdiag attribute
    dcd <- t(as.data.frame(dcdr))
    rownames(dcd) <- 1:length(dcdr)
    dcd <- data.frame(dcd)
    colnames(dcd) <- c("n.clones", "lambda.max", "ms.error", "r.squared", "r.hat") # went to dcdiag.default
    class(dcd) <- c("dcdiag", class(dcd))
    attr(mod, "dcdiag") <- dcd
    mod
}
