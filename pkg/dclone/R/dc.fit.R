## stop.if.converged = FALSE arg to break when all dcdiag criteria are met
dc.fit <- 
function(data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, flavour = c("jags", "bugs"), ...)
{
    ## initail evals
    flavour <- match.arg(flavour)
    if (identical(n.clones, 1))
        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    if (is.environment(data))
        stop("'data' should be list, not environment")
    ## determine k
    k <- n.clones[order(n.clones)]
    k <- unique(k)
    times <- length(k)
#    dcoptions <- getOption("dclone")
    rhat.crit <- getOption("dclone.rhat") # dcoptions$r.hat$crit
    trace <- getOption("dclone.verbose") # dcoptions$verbose
    ## evaluate updating
    if (!is.null(update) != !is.null(updatefun))
        stop("both 'update' and 'updatefun' must be provided")
    if (!is.null(update)) {
        unchanged <- c(unchanged, update)
        updatefun <- match.fun(updatefun)
    }
    ## evaluate inits
    if (missing(inits))
        inits <- NULL
    if (!is.null(initsfun))
        initsfun <- match.fun(initsfun)
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
        mod <- if (flavour == "jags") {
            jags.fit(jdat, params, model, inits, ...)
        } else {
            bugs.fit(jdat, params, model, inits, format="mcmc.list", DIC=FALSE, ...)
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
        } else {
            if (!is.null(update))
                jdat[[update]] <- updatefun(mod)
            if (!is.null(initsfun))
                inits <- initsfun(mod)
        }
        dctmp <- extractdctable.default(mod)
        dcdr[[i]] <- extractdcdiag.default(mod)
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[j,]
        }
    }
    ## warning if R.hat < crit
    if (nchain(mod) > 1 && any(dctmp[,"r.hat"] >= rhat.crit))
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    dcts <- lapply(dcts, function(z) as.data.frame(z))
    class(dcts) <- "dctable"
    attr(mod, "dctable") <- dcts
    ## finalizing dcdiag attribute
    dcd <- t(as.data.frame(dcdr))
    rownames(dcd) <- 1:length(dcdr)
    dcd <- data.frame(dcd)
    class(dcd) <- c("dcdiag", class(dcd))
    attr(mod, "dcdiag") <- dcd
    mod
}
