rsf.fit <-
function(X, Y, m, link = "logit", B = 99, 
inits, method = "Nelder-Mead", control, ...)
{
    if (!identical(m, 0))
        stop(paste("\nMatched use-available design is", 
        "not yet available in this package.",
        "If interested, please contact",
        "Subhash Lele <slele@ualberta.ca>\n\n", sep="\n"))
    ## internal function for optim
    nll.fun <- function(parms, boot=id.all) {
#        if (link == "log")
#            parms <- c(0, parms)
        P.used <- drop(linkinvfun(X.used %*% parms))
        P.avail1 <- drop(linkinvfun(X.avail %*% parms))
        if (is.null(m.avail)) {
            P.avail <- rep(sum(P.avail1), N.used)
        } else {
            P.avail <- aggregate(P.avail1, list(m.avail), sum)
            P.avail <- P.avail[match(m.used, P.avail$Group),2]
#            P.avail <- sapply(m.avail, function(z) sum(P.avail1[m.avail==z]))
#            dim(P.avail1) <- c(m, length(P.avail1) / m)
#            P.avail <- colSums(P.avail1)
        }
        ll.vec <- log(pmax(P.used / (P.used + P.avail), .Machine$double.eps))
        -sum(ll.vec[boot])
    }
    if (missing(control))
        control <- getOption("rspf.optim.control")

    ## inverse link function
    linkinvfun <- binomial(link=make.link(link))$linkinv
    if (link=="probit" && !getOption("rspf.robust.probit"))
        linkinvfun <- as.function(stats:::pnorm)

    ## initial values from GLM if not defined in call
    if (missing(inits)) {
        inits <- suppressWarnings(glm.fit(X, Y, family=binomial())$coef)
        inits[is.na(inits)] <- 0
    }
    ## handling Exponential case
    np <- ncol(X)
#    if (link == "log" && length(inits) == np)
#        inits <- inits[-1]
#    if (link == "log")
#        np <- np - 1
    nam <- colnames(X)[(ncol(X)+1-np):ncol(X)]
    ## objects needed for optim
    X.used <- data.matrix(X[Y==1,])
    X.avail <- data.matrix(X[Y==0,])
    N.used <- nrow(X.used)
    N.avail <- nrow(X.avail)
    id.all <- 1:N.used

    ## this might change if matched point definition changes in the future
    if (missing(m))
        m <- 0
#        stop("'m' must be provided")
    m.avail <- NULL
    if (length(m) == 1) {
        if (m > 0) {
            m.used <- 1:N.used
            m.avail <- rep(1:N.used, each=m)
            if (length(m.avail) != N.avail)
                stop("'m' value incompatible with available points")
        }
    } else {
        if (length(m) != N.used + N.avail)
            stop("inappropriate length for 'm'")
        if (!all(m[Y==1] %in% m[Y==0]))
            stop("each used point must have matched available points in 'm'")
        if (!all(m[Y==0] %in% m[Y==1]))
            stop("matched points without used points detected in 'm'")
        m.used <- m[Y==1]
        m.avail <- m[Y==0]
    }

## not needed any more
#    ones.m <- if (m == 0)
#        data.matrix(rep(1, N.avail)) else data.matrix(rep(1, m))
#    nll.fun <- if (matched)
#        nll.matched else nll.nonmatched
    ## optimization, point estimates
    results <- optim(inits, nll.fun, method = method, hessian = TRUE, control = control, boot = id.all)
    ## log likelihood
    ll <- -results$value
    ## point estimates
    cfs <- results$par
    names(cfs) <- nam
    ## checking Hessian, producing Std Errors
    if (rcond(results$hessian) <= 1e-06)
        ses <- rep(NA, np)
    if (rcond(results$hessian) > 1e-06) {
        ## due to negLogLik, we take H^-1 and not -H^-1
        opvar <- diag(solve(results$hessian))
        if (any(opvar < 0)) {
            opvar[opvar < 0] <- NA
            warning("negative variance values in optim, NAs produced")
        }
        ses <- sqrt(opvar)
    }
#    ses <- sqrt(diag(solve(results$hessian)))
    names(ses) <- nam
    ## optimization with bootstrap (this can be used for boostrap CI)
    if (B > 0) {
        id.boot <- lapply(1:B, function(i)
            sample(1:N.used, N.used, replace=TRUE))
        ## parallel computation can be added here
        boot.out <- if (require(pbapply)) {
            pbsapply(id.boot, function(z) optim(cfs, nll.fun, 
                hessian = FALSE, method = method, control = control, boot = z)$par)
        } else {
            sapply(id.boot, function(z) optim(cfs, nll.fun, 
                hessian = FALSE, method = method, control = control, boot = z)$par)
        }
    } else boot.out <- NULL

    ## return value assembly
    if (link=="log") {
        cfs2 <- cfs[-1]
        ses2 <- ses[-1]
        np <- np - 1
    } else {
        cfs2 <- cfs
        ses2 <- ses
    }
#        c(0, cfs) else cfs
    out <- list(call = match.call(),
        y = Y,
        coefficients = cfs2,
        std.error = ses2,
        loglik = ll,
        results = results,
        link = link,
        control = control,
        inits = inits,
        m = m,
        np = np,
        fitted.values = linkinvfun(drop(X %*% cfs)),
        nobs = N.used,
#        n = N.used,
#        df.null = N.used - 1,
#        df.residual = N.used - np, 
        bootstrap = boot.out,
        converged = results$convergence == 0)
    if (link == "log")
        out$fitted <- exp(drop(X %*% c(0, cfs2)))
    out
}

