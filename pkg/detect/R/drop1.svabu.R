drop1.svabu <-
function (object, scope, model, criter = c("AIC", "BIC"), test = FALSE, k = 2, control, ...)
{
    if (!is.null(object$terms$zif))
        stop(paste("'drop1' works only for BZIP models without covariates\n",
        "HINT: fit BZIP without covariates and later build the ZI part of the model", sep=""))
    if (missing(control))
        control <- object$control
    if (!is.null(control)) {
        ## saving current settings
        old.optim.control <- getOption("detect.optim.control")
        old.optim.method <- getOption("detect.optim.method")
        old.dc.control <- getOption("detect.dc.control")
        old.mcmc.control <- getOption("detect.mcmc.control")
        ## applying controls from fit
        options("detect.optim.control"=control$optim.control)
        options("detect.optim.method"=control$optim.method)
        options("detect.dc.control"=control$dc.control)
        options("detect.mcmc.control"=control$mcmc.control)
        ## restoring settings
        on.exit(options("detect.optim.control"=old.optim.control), add=TRUE)
        on.exit(options("detect.optim.method"=old.optim.method), add=TRUE)
        on.exit(options("detect.dc.control"=old.dc.control), add=TRUE)
        on.exit(options("detect.mcmc.control"=old.mcmc.control), add=TRUE)
    }
    if (missing(model))
        stop("'model' argument must be supplied")
    model <- match.arg(model, c("sta", "det"))
    criter <- match.arg(criter)
    x <- model.matrix(object, model)
    n <- nrow(x)
    asgn <- attr(x, "assign")
    tl <- attr(object$terms[[model]], "term.labels")
    if (missing(scope)) {
        scope <- drop.scope.svisit(object, model=model)
    } else {
        if (!is.character(scope)) {
            scope <- attr(terms(update.formula(object$formula[[model]], scope)), 
                "term.labels")
        }
        if (!all(match(scope, tl, 0L) > 0L))
            stop("scope is not a subset of term labels")
    }
    ## evaluating overlap of the 2 sides
    ## evaluate if there is one continuous covariate left
    revmod <- c("sta", "det")[c("sta", "det") != model]
    xother <- model.matrix(object, revmod)
    scopeother <- drop.scope.svisit(object, model=revmod)
    overlap <- intersect(scope, scopeother)
    allvars <- union(scope, scopeother)
    Xlevels <- names(object$levels)
    Excl <- if (sum(!allvars %in% Xlevels) < 2)
        scope[which(!scope %in% Xlevels)] else NULL
    Excl <- if (length(scope) == length(overlap) + 1)
        union(Excl, scope[!scope %in% overlap]) else NULL
## not very elegant solution here
    ## original drop1 goes on from here again
    ndrop <- match(scope, tl)
    ns <- length(scope)
    rdf <- object$df.residual
    dfs <- numeric(ns)
#    dev <- numeric(ns)
    llik <- numeric(ns)
    y <- object$y
    if (is.null(y)) 
        y <- model.response(model.frame(object), "numeric")
## is weight necessary??? maybe later ???
#    wt <- object$prior.weights
#    if (is.null(wt)) 
#        wt <- rep.int(1, n)
    for (i in 1:ns) {
        ii <- seq_along(asgn)[asgn == ndrop[i]]
        jj <- setdiff(seq(ncol(x)), ii)

## if options are reset, that might lead to inconsistencies --> need to store optim/DC settings
#        n.clones <- if (object$method == "dc")
#            nclones(object$results$mle) else 1000
        if (model == "sta") {
                z <- svabu.fit(y, x[, jj, drop = FALSE], xother, 
                    N.max=object$N.max, zeroinfl=object$zeroinfl, ...)
            } else {
                z <- svabu.fit(y, xother, x[, jj, drop = FALSE],
                    N.max=object$N.max, zeroinfl=object$zeroinfl, ...)
            }
#            if (inherits(z, "try-error"))
#                return(z)

            dfs[i] <- z$nobs - z$df.residual
            llik[i] <- z$loglik
    }
    scope <- c("<none>", scope)
    dfs <- c(object$nobs - object$df.residual, dfs)
    llik <- c(object$loglik, llik)
    ## NOTE: this is unscaled deviance
    if (test)
        dev <- 2 * llik
    if (criter == "AIC") {
        aic <- -2 * llik + k * dfs
        critpart <- data.frame(AIC=aic, Delta.AIC=aic - aic[1])
    }
    if (criter == "BIC") {
        bic <- -2 * llik + dfs * log(object$nobs)
        critpart <- data.frame(BIC=bic, Delta.BIC=bic - bic[1])
    }
    dfs <- dfs[1] - dfs
    dfs[1] <- NA
#    aod <- data.frame(Df = dfs, Deviance = dev, AIC = aic, row.names = scope,
#        check.names = FALSE)
    aod <- data.frame(Df = dfs, critpart, row.names = scope, check.names = FALSE)
    if (test) {
        dev <- pmax(0, -2 * (llik - llik[1]))
        dev[1] <- NA
        nas <- !is.na(dev)
        LRT <- "unscaled dev." # "LRT"
        aod[, LRT] <- dev

        Safe_pchisq <- function (q, df, ...) {
            df[df <= 0] <- NA
            pchisq(q = q, df = df, ...)
        }

        dev[nas] <- Safe_pchisq(dev[nas], aod$Df[nas], lower.tail = FALSE)
        aod[, "Pr(Chi)"] <- dev
    }
    ## Exclude (make it NA) lines where we should keep covariates
    if (!is.null(Excl))
        aod[Excl,] <- NA
    mpart <- if (model == "sta") "abundance" else "detection"
    head <- c("Single visit abundance model",
        paste("Single term deletion on the", mpart, "side", collapse=" ", sep=" "),
        "\nModel:", deparse(as.vector(formula(object)$full)), 
        "Dispersion parameter is taken to be 1", "\n")  # because of unscaled deviance
    class(aod) <- c("anova", "data.frame")
    attr(aod, "heading") <- head
    return(aod)
}

