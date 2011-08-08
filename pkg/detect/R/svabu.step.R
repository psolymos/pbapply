svabu.step <-
function (object, model, trace = 1, steps = 1000, criter = c("AIC", "BIC"), test = FALSE, k = 2, 
control, ...)
{
    if (!is.null(object$terms$zif))
        stop("'drop1' works only for BZIP models without covariates\n
        hint: fit without covariates and later build the ZI part of the model")

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
    model <- match.arg(model, c("sta","det"))
    ## this is for formatting printed results
    cut.string <- function(string) {
        if (length(string) > 1)
            string[-1] <- paste("\n", string[-1], sep = "")
        string
    }
    ## this adds the anove table to the final model fit
    ## but for singleocc, deviance is not well defined so it was taken out
    ## only AIC changes are returned
    step.results <- function(models, fit, object) {
        change <- sapply(models, "[[", "change")
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, diff(rdf))
#        AIC <- sapply(models, "[[", "AIC")
        AIC <- sapply(models, "[[", criter)
        heading <- c("Single visit abundance model\n Stepwise Model Path",
            "\nInitial Model:", deparse(as.vector(formula(object))),
            "\nFinal Model:", deparse(as.vector(formula(fit))),
            "\n")
        aod <- data.frame(Step = I(change), Df = ddf, 
            "Resid. Df" = rdf, AIC = AIC,
            check.names = FALSE)
        if (criter != "AIC")
            colnames(aod)[colnames(aod) == "AIC"] <- criter
        attr(aod, "heading") <- heading
        fit$anova <- aod
        fit
    }
    criter <- match.arg(criter)
    ## extract model terms from onle side of the formula based on 'model'
    Terms <- terms(object, model)
    object$formula[[model]] <- Terms
## this call$formula step is unclear, however the call for fit seem to be o.k.
#    object$call$formula <- object$formula[[model]] <- Terms
## this part can be considered later if add1 is implemented
#    md <- missing(direction)
#    direction <- match.arg(direction)
#    backward <- direction == "both" | direction == "backward"
#    forward <- direction == "both" | direction == "forward"
## scope is also needed for add1
#    if (missing(scope)) {
        fdrop <- numeric(0)
        fadd <- attr(Terms, "factors")
#        if (md)
            forward <- FALSE
#    }
#    else {
#        if (is.list(scope)) {
#            fdrop <- if (!is.null(fdrop <- scope$lower))
#                attr(terms(update.formula(object, fdrop)), "factors")
#            else numeric(0)
#            fadd <- if (!is.null(fadd <- scope$upper))
#                attr(terms(update.formula(object, fadd)), "factors")
#        }
#        else {
#            fadd <- if (!is.null(fadd <- scope))
#                attr(terms(update.formula(object, scope)), "factors")
#            fdrop <- numeric(0)
#        }
#    }
    ## create vectors for bookkeeping results
    models <- vector("list", steps)
    ## a copy to return and some initial values
    fit <- object
    n <- object$nobs
    bAIC <- extractAIC(fit, k = k, ...)
    edf <- bAIC[1]
    if (criter == "AIC")
        bAIC <- bAIC[2]
    if (criter == "BIC")
        bAIC <- BIC(fit)
    nm <- 1
    Terms <- fit$terms[[model]]
    ## initial printout
    if (trace)
        cat("Start:  ", criter, "=", format(round(bAIC, 2)), "\n",
            cut.string(deparse(as.vector(formula(fit)$full))), "\n\n", sep = "")
    models[[nm]] <- list(deviance = extractAIC(fit, k = k, ...), df.resid = n -
        edf, change = "", AIC = bAIC)
    if (criter != "AIC")
        names(models[[nm]])[names(models[[nm]]) == "AIC"] <- criter
    ## this should be removed if add1 is implemented
    backward <- TRUE
    while (steps > 0) {
        steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if (backward && length(scope$drop)) {
            ## control is set up here, so drop1 get NULL
            aod <- drop1.svabu(fit, scope$drop, model,
                criter = criter, k = k, test = test, control=NULL, ...)

            if (inherits(aod, "try-error"))
                return(aod)

            rn <- row.names(aod)
            row.names(aod) <- c(rn[1], paste("-", rn[-1], sep = " "))
            if (any(aod$Df == 0, na.rm = TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                change <- rev(rownames(aod)[zdf])[1]
            }
        }
        if (is.null(change)) {
## this part can be considered later if add1 is implemented
#            if (forward && length(scope$add)) {
#                aodf <- add1(fit, scope$add, scale = scale, trace = trace,
#                  k = k, ...)
#                rn <- row.names(aodf)
#                row.names(aodf) <- c(rn[1], paste("+", rn[-1],
#                  sep = " "))
#                aod <- if (is.null(aod))
#                  aodf
#                else rbind(aod, aodf[-1, , drop = FALSE])
#            }
            attr(aod, "heading") <- NULL
            nzdf <- if (!is.null(aod$Df))
                aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if (is.null(aod) || ncol(aod) == 0)
                break
#            nc <- match("AIC", names(aod))
            nc <- match(criter, names(aod))
            nc <- nc[!is.na(nc)][1]
            o <- order(aod[, nc])
            if (trace)
                print(aod[o, ])
            if (o[1] == 1)
                break
            change <- rownames(aod)[o[1]]
        }
        ttt <- if (model == "sta")
            paste(". ~ .", change, "| .") else paste(". ~ . | .", change)
        fit <- update.svisit(fit, as.formula(ttt), evaluate = FALSE)
        fit <- eval.parent(fit)
        if (length(fit$fitted) != n)
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit, model)
        bAIC <- extractAIC(fit, k = k, ...)
        edf <- bAIC[1]
#        bAIC <- bAIC[2]

        if (criter == "AIC")
            bAIC <- bAIC[2]
        if (criter == "BIC")
            bAIC <- BIC(fit)

        if (trace)
            cat("\nStep:  ", criter, "=", format(round(bAIC, 2)), "\n",
                cut.string(deparse(as.vector(formula(fit)$full))),
                "\n\n", sep = "")
        if (bAIC >= AIC + 1e-07)
            break
        nm <- nm + 1
        models[[nm]] <- list(deviance = extractAIC(fit, k = k, ...), df.resid = n -
            edf, change = change, AIC = bAIC)
        if (criter != "AIC")
            names(models[[nm]])[names(models[[nm]]) == "AIC"] <- criter
    }
    step.results(models = models[seq(nm)], fit, object)
}

