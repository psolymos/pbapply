dc.parfit <- 
function(cl, data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, flavour = c("jags", "bugs"), ...)
{
    flavour <- match.arg(flavour)
    ## stop if rjags not found
    if (flavour=="jags" && !suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    ## initail evals
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    ## get parallel chains option value
    parchains <- getOption("dcoptions")$parchains
    ## some arguments are ignored with size balancing
    if (!parchains) {
        if (!is.null(updatefun))
            warnings("'updatefun' argument is ignored when parchains option is FALSE")
        if (!is.null(initsfun))
            warnings("'initsfun' argument is ignored when parchains option is FALSE")
        if (!is.null(update))
            warnings("'update' argument is ignored when parchains option is FALSE")
    }
    ## multiple parallel chains
    if (parchains) {
        mod <- dclone:::dcFit(data, params, model, inits, n.clones, 
            multiply=multiply, unchanged=unchanged, 
            update=update, updatefun=updatefun, 
            initsfun=initsfun, flavour = flavour, 
            cl=cl, parchains=TRUE, ...)
    ## size balancing
    } else {
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
        ## global options
        rhat.opts <- getOption("dcoptions")$rhat
        trace <- getOption("dcoptions")$verbose
        ## evaluate inits
        if (missing(inits))
            inits <- NULL
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
        ## write model
        if (is.function(model) || inherits(model, "custommodel")) {
            if (is.function(model))
                model <- match.fun(model)
            ## write model only if SOCK cluster (shared memory)
            if (inherits(cl, "SOCKcluster")) {
                model <- write.jags.model(model)
                on.exit(try(clean.jags.model(model)))
            }
        }
        ## common data
        cldata <- list(data=data, params=params, model=model, inits=inits,
            multiply=multiply, unchanged=unchanged, k=k)
        ## parallel computations
        balancing <- if (!getOption("dcoptions")$LB)
            "size" else "both"
        dir <- if (inherits(cl, "SOCKcluster"))
            getwd() else NULL
        ## get loaded modules
        jm <- if (flavour == "jags")
            paste("load.module('", list.modules(), "')", sep="") else NULL
        ## load rjags so that modules are cleaned up properly
        libs <- jm <- if (flavour == "jags")
            c("dclone", "rjags") else "dclone"
        ## do the work
        pmod <- snowWrapper(cl, k, dcparallel, cldata, lib=libs, 
            balancing=balancing, size=k, 
            rng.type=getOption("dcoptions")$RNG, cleanup=TRUE, dir=dir, evalq=jm, ...)
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
    }
    mod
}
