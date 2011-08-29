dc.parfit <- 
function(cl, data, params, model, inits, n.clones, multiply=NULL, unchanged=NULL, 
update=NULL, updatefun=NULL, initsfun=NULL, flavour = c("jags", "bugs"), 
partype=c("balancing", "parchains", "both"), ...)
{
    flavour <- match.arg(flavour)
    ## stop if rjags not found
    if (flavour=="jags" && !suppressWarnings(require(rjags)))
        stop("there is no package called 'rjags'")
    ## initail evals
    if (!inherits(cl, "cluster"))
        stop("'cl' must be a 'cluster' object")
    ## get parallel type
    partype <- match.arg(partype)
    ## some arguments are ignored with size balancing
    if (partype != "parchains") {
        if (!is.null(updatefun))
            warnings("'updatefun' argument is ignored when parchains option is FALSE")
        if (!is.null(initsfun))
            warnings("'initsfun' argument is ignored when parchains option is FALSE")
        if (!is.null(update))
            warnings("'update' argument is ignored when parchains option is FALSE")
    }
    if (partype != "balancing" && flavour == "bugs")
        stop("flavour='bugs' supported for 'balancing' type only")
    if (length(n.clones) < 2 && partype=="balancing") {
        warnings("no need for parallel computing with 'balancing'")
    }
    ## multiple parallel chains
    if (partype == "parchains") {
        mod <- dclone:::dcFit(data, params, model, inits, n.clones, 
            multiply=multiply, unchanged=unchanged, 
            update=update, updatefun=updatefun, 
            initsfun=initsfun, flavour = flavour, 
            cl=cl, parchains=TRUE, ...)
    ## size balancing and balancing+parchains
    } else {
        if (missing(n.clones))
            stop("'n.clones' argument must be provided")
        if (identical(n.clones, 1))
            stop("'n.clones = 1' gives the Bayesian answer, no need for DC")
#        if (is.environment(data))
#            stop("'data' should be list, not environment")
        if (is.environment(data)) {
            warnings("'data' was environment: it was coerced into a list")
            data <- as.list(data)
        }
        ## determine k
        k <- n.clones[order(n.clones)]
        k <- unique(k)
        times <- length(k)
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
        ## dclone loaded only if not yet there
        lib <- if ("dclone" %in% clusterEvalQ(cl, .packages())[[1]])
            NULL else "dclone"

## TODO: handle jags.fit arguments to avoid 'matched by multiple actual arguments'

        ## size balancing
        if (partype == "balancing") {
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
                    return(mod) else return(list(dct=dclone:::extractdctable(mod), 
                        dcd=dclone:::extractdcdiag(mod)))
            }
            pmod <- snowWrapper(cl, k, dcparallel, cldata, lib=lib, 
                balancing=balancing, size=k, 
                rng.type=getOption("dcoptions")$RNG, cleanup=TRUE, dir=dir, ...)
            mod <- pmod[[times]]
            ## dctable
            dct <- lapply(1:(times-1), function(i) pmod[[i]]$dct)
            dct[[times]] <- extractdctable(mod)
            ## dcdiag
            dcd <- lapply(1:(times-1), function(i) pmod[[i]]$dcd)
            dcd[[times]] <- extractdcdiag(mod)
        ## balancing+parchains
        } else {
            ## RNG and initialization
            dcinits <- function(i, ...) {
                jdat <- dclone(cldata$data, i, multiply=cldata$multiply, unchanged=cldata$unchanged)
                jags.fit(data=jdat, params=cldata$params, model=cldata$model, inits=cldata$inits,
                    n.adapt=0, n.update=0, n.iter=0)$state(internal=TRUE)
            }
            ## snowWrapper with cleanup (but cldata changes, has to be passed again)
            pini <- snowWrapper(cl, k, dcinits, cldata, lib=lib, 
                balancing=balancing, size=k, 
                rng.type=getOption("dcoptions")$RNG, cleanup=FALSE, dir=dir, ...)
            cldata$inits <- do.call("c", pini)
            nch <- list(...)$n.chains
            if (is.null(nch))
                nch <- 3
            cldata$k <- rep(k, each=nch)
            ## parallel function to evaluate by snowWrapper
            dcparallel <- function(i, ...) {
                jdat <- dclone(cldata$data, cldata$k[i], multiply=cldata$multiply, unchanged=cldata$unchanged)
                jags.fit(data=jdat, params=cldata$params, model=cldata$model, 
                    inits=cldata$inits[[i]], n.chains=1, updated.model=FALSE, ...)
            }
            ## no dclone loaded as it is there
            pmod <- snowWrapper(cl, 1:(times*nch), dcparallel, cldata, lib=NULL, 
                balancing=balancing, size=cldata$k, 
                rng.type=getOption("dcoptions")$RNG, cleanup=TRUE, dir=dir, ...)
            ## binding the chains for each k value
            assemblyfun <- function(mcmc) {
                n.clones <- nclones(mcmc)
                res <- as.mcmc.list(lapply(mcmc, as.mcmc))
                if (!is.null(n.clones) && n.clones > 1) {
                    attr(res, "n.clones") <- n.clones
                    class(res) <- c("mcmc.list.dc", class(res))
                }
                res
            }
            i.end <- 1:times*nch
            i.start <- i.end+1-nch
            pmod <- lapply(1:times, function(i) assemblyfun(pmod[i.start[i]:i.end[i]]))
            mod <- pmod[[times]]
            ## dctable
            dct <- lapply(pmod, extractdctable)
            ## dcdiag
            dcd <- lapply(pmod, extractdcdiag)
        }
        ## warning if R.hat < crit
        if (nchain(mod) > 1 && any(dct[[times]][,"r.hat"] >= rhat.opts))
            warning("chains convergence problem, see R.hat values")
        ## finalizing dctable attribute
        rnam <- lapply(dct, rownames)
        nam <- rnam[[1]]
        dct2 <- vector("list", length(nam))
        names(dct2) <- rownames(dct[[1]])
        for (i in 1:length(nam)) {
            dct2[[i]] <- cbind(n.clones = k, t(sapply(dct, function(z) z[i, ])))
        }
        dct2 <- lapply(dct2, function(z) as.data.frame(z))
        class(dct2) <- "dctable"
        attr(mod, "dctable") <- dct2
        ## finalizing dcdiag attribute
        dcd2 <- as.data.frame(matrix(unlist(dcd), nrow=length(dcd), byrow=TRUE))
        colnames(dcd2) <- names(dcd[[1]])
        class(dcd2) <- c("dcdiag", class(dcd2))
        attr(mod, "dcdiag") <- dcd2
    }
    mod
}

