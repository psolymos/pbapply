dc.parfit <- 
function(cl, data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL,
flavour = c("jags", "bugs"), ...)
{
    ## initail evals
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    flavour <- match.arg(flavour)
    if (missing(n.clones))
        stop("'n.clones' argument must be provided")
    if (identical(n.clones, 1))
        stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
    if (is.environment(data))
        stop("'data' should be list, not environment")
    ## determine k
    k <- n.clones[order(n.clones)]
    k <- unique(k)
    times <- length(k)
    if (times < 2)
        stop("no need for parallel computing")
    ## globel options
    rhat.opts <- getOption("dcoptions")$rhat
    trace <- getOption("dcoptions")$verbose
    ## evaluate inits
    if (missing(inits))
        inits <- NULL

    ## write model
    if (is.function(model) || inherits(model, "custommodel")) {
        if (is.function(model))
            model <- match.fun(model)
        model <- write.jags.model(model)
        on.exit(try(clean.jags.model(model)))
    }

    #### parallel part
    if (trace) {
        cat("\nParallel computation in progress\n\n")
        flush.console()
    }
    ## parallel function
    dcparallel <- function(i, ...) {
        jdat <- dclone(cldata$data, i, multiply=cldata$multiply, unchanged=cldata$unchanged)
        mod <- if (flavour == "jags") {
            jags.fit(data=jdat, params=cldata$params, model=cldata$model, inits=cldata$inits, ...)
        } else {
            bugs.fit(data=jdat, params=cldata$params, model=cldata$model, inits=cldata$inits, 
                format="mcmc.list", ...)
        }
        if (i == max(k))
            return(mod) else return(list(dct=dclone:::extractdctable(mod), dcd=dclone:::extractdcdiag(mod)))
    }

    ## common data
    cldata <- list(data=data, params=params, model=model, inits=inits,
        multiply=multiply, unchanged=unchanged, k=k)
    ## parallel computations
    rng <- c("Wichmann-Hill", "Marsaglia-Multicarry",
        "Super-Duper", "Mersenne-Twister")
    rng <- rep(rng, length(cl))[1:length(cl)]
    balancing <- if (getOption("dcoptions")$LB)
        "size" else "both"
    pmod <- snowWrapper(cl, k, dcparallel, cldata, lib="dclone", 
        balancing=balancing, size=k, dir=getwd(), set.rng=TRUE, ...)
    mod <- pmod[[times]]

    ## dctable
    dct <- lapply(1:(times-1), function(i) pmod[[i]]$dct)
    dct[[times]] <- extractdctable(mod)
    rnam <- lapply(dct, rownames)
    nam <- rnam[[1]]
    dct2 <- vector("list", length(nam))
    names(dct2) <- rownames(dct[[1]])
    for (i in 1:length(nam)) {
        dct2[[i]] <- cbind(n.clones = k, t(sapply(dct, function(z) z[i, ])))
    }
    dct2 <- lapply(dct2, function(z) as.data.frame(z))

    ## dcdiag
    dcd <- lapply(1:(times-1), function(i) pmod[[i]]$dcd)
    dcd[[times]] <- extractdcdiag(mod)
    dcd2 <- as.data.frame(matrix(unlist(dcd), nrow=length(dcd), byrow=TRUE))
    colnames(dcd2) <- names(dcd[[1]])

    ## warning if R.hat < crit
    if (nchain(mod) > 1 && any(dct[[times]][,"r.hat"] >= rhat.opts))
        warning("chains convergence problem, see R.hat values")
    ## finalizing dctable attribute
    class(dct2) <- "dctable"
    attr(mod, "dctable") <- dct2
    ## finalizing dcdiag attribute
    class(dcd2) <- c("dcdiag", class(dcd2))
    attr(mod, "dcdiag") <- dcd2
    mod
}
