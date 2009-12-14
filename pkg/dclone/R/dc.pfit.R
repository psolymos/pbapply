dc.pfit <- 
function(cl, data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, trace=1, flavour = c("jags", "bugs"), ...)
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
    crit <- getOption("dclone.crit")
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

    ## evaluate if parallel computing is needed
    if ((!is.null(update) && (times-2) < 2) || (is.null(update) && (times-1) < 2))
        stop("no need for parallel computing")

    #### first model fitting
    if (!is.null(update)) {
        tmpch <- if (k[1] == 1) "clone" else "clones"
        if (trace)
            cat("\nFitting model with", k[1], tmpch, "\n\n")
        jdat <- dclone(data, k[1], multiply=multiply, unchanged=unchanged)
        mod <- if (flavour == "jags") {
            jags.fit(jdat, params, model, inits, ...)
        } else {
            bugs.fit(jdat, params, model, inits, format="mcmc.list", DIC=FALSE, ...)
        }
        ## dctable of first model
        dc.first <- extractdctable(mod)
        ## update priors and inits here
        jdat[[update]] <- updatefun(mod)
        if (!is.null(initsfun))
            inits <- initsfun(mod)
        ## kseq determines the clones in parallel part
        kseq <- k[-c(1, times)]
    } else kseq <- k[-times]

    #### parallel part
    if (trace) {
        cat("\nParallel computation in progress\n\nFitting models with", kseq, "clones\n\n")
        flush.console()
    }
    ## parallel function
    dcparallel <- function(i, ...) {
        jdat <- dclone(cldata$data, i, multiply=cldata$multiply, unchanged=cldata$unchanged)
        mod <- if (cldata$flavour == "jags") {
            jags.fit(data=jdat, params=cldata$params, model=cldata$model, inits=cldata$inits, ...)
        } else {
            bugs.fit(data=jdat, params=cldata$params, model=cldata$model, inits=cldata$inits,
                format="mcmc.list", DIC=FALSE, ...)
        }
        extractdctable(mod)
    }
    ## common data
    cldata <- list(data=data, params=params, model=model, inits=inits,
        multiply=multiply, unchanged=unchanged, flavour=flavour, kseq=kseq)
    ## parallel computations
    pdct <- mcmc.cluster(cl, cldata$kseq, dcparallel, cldata, lib="dclone", 
        load.balancing=getOption("dclone.cluster")$load.balancing, ...)

    #### last model fitting
    tmpch <- if (k[times] == 1) "clone" else "clones"
    if (trace)
        cat("\nFitting model with", k[times], tmpch, "\n\n")
    jdat <- dclone(data, k[times], multiply=multiply, unchanged=unchanged)
    mod <- if (flavour == "jags") {
        jags.fit(jdat, params, model, inits, ...)
    } else {
        bugs.fit(jdat, params, model, inits, format="mcmc.list", DIC=FALSE, ...)
    }
    dc.last <- extractdctable(mod)

    ## dctable skeleton
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
    ## assembling dctable
    dctmp <- if (!is.null(update))
        c(list(dc.first), pdct, list(dc.last)) else c(pdct, list(dc.last))
    for (i in 1:times) {
        for (j in 1:length(vn)) {
            dcts[[j]][i,-1] <- dctmp[[i]][j,]
        }
    }

    ## warning if R.hat < crit
    if (nchain(mod) > 1 && any(dc.last[,"r.hat"] >= crit$r.hat))
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    dcts <- lapply(dcts, function(z) as.data.frame(z))
    class(dcts) <- "dctable"
    attr(mod, "dctable") <- dcts
    mod
}
