## this is the new ver with PMLE, but still not intended to be directly user callable
`singleocc.fit` <-
function(obs, occ, det, link.occ = "logit", link.det = "logit", penalized = FALSE, auc = FALSE,
method = c("optim", "dc"), n.clones=1, inits, prec=0.1, cl=NULL, ...)
{
    ## internal funs
    `singleocc.MLE` <-
    function(parameters, observations, X, Z, link1, link2)
    {
        par.occ <- parameters[1:NCOL(X)]
        par.det <- parameters[-(1:NCOL(X))]
        linkinvfun.occ <- binomial(link1)$linkinv
        linkinvfun.det <- binomial(link2)$linkinv
        phi <- linkinvfun.occ(X %*% par.occ)
        detec <- linkinvfun.det(Z %*% par.det)
        q <- detec * phi
        loglik.bin <- sum(observations * log(q)) + sum((1 - observations) * log(1 - q))
        if (is.na(loglik.bin))
            loglik.bin <- -1e+05
        return(loglik.bin)
    }
    #-------------------------------------------------------------------------------------------
    #Name: LikelihoodPenalizedSingleSurvey.fn
    #Function: To calculate the penalized log likelihood for a single survey occupancy model
    #Input parameters:
    #   parameters: a vector with the value of the parameters for the occupancy and detection model.
    #   observations:vector of size n.sites that contains 1 if the specied was observed,0 otherwise
    #   X:matrix that contains the value of the covariates used in the occupancy model
    #   Z:matrix that contains the value of the covariates used in the detection model
    #   bmax:naive estimated parameters for the occupancy model
    #   dmax:naive estimated parameters for the detection model
    #   psi0:mean estimated occupancy obtained from the MLE
    #   d0:mean estimated detection obtained from the MLE
    #   link1:Link function for the occupancy model
    #   link2:Link function for the detection model
    #   v1:Square root of the sum of the estimated variance for the detection parameters obtained by the MLE
    #   v2:Square root of the sum of the estimated variance for the occupancy parameters obtained by the MLE
    #Output: Value of the penalized log likelihood function evaluated at the value of the parameters
    #-------------------------------------------------------------------------------------------
    `singleocc.PMLE` <-
    function(parameters, observations, X, Z, bmax, dmax, psi0, d0, link1, link2, v1, v2)
    {
        param.occup <- parameters[1:NCOL(X)]
        param.detec <- parameters[-(1:NCOL(X))]
        linkinvfun1 <- binomial(link1)$linkinv
        linkinvfun2 <- binomial(link2)$linkinv
        occup <- linkinvfun1(X %*% param.occup)
        occup.max <- linkinvfun1(X %*% bmax)
        detec <- linkinvfun2(Z %*% param.detec)
        detec.max <- linkinvfun2(Z %*% dmax)
        p <- occup * detec
        LogLikZIB <- (observations * log(p)) + ((1 - observations) * log(1 - p))

        penalty.occupancy <- (1 - mean(occup.max)) * d0 * v1 * sum(abs(param.occup - bmax))
        penalty.detection <- (1 - mean(detec.max)) * psi0 * v2 * sum(abs(param.detec - dmax))
        penalization <- penalty.occupancy + penalty.detection
        LogLik <- sum(LogLikZIB) - penalization
        return(LogLik)
    }
    ## BUGS model for 2^3 kinds of link pairs
    mcmcSS.all <- c("model {",
        "for (i in 1:N.sites) {",
            "Y[i] ~ dbern(p.det[i] * W[i])",
            "logit(p.occ[i]) <- inprod(X[i, ], beta)",
            "logit(p.det[i]) <- inprod(Z[i, ], theta)",
            "probit(p.occ[i]) <- inprod(X[i, ], beta)",
            "probit(p.det[i]) <- inprod(Z[i, ], theta)",
            "cloglog(p.occ[i]) <- inprod(X[i, ], beta)",
            "cloglog(p.det[i]) <- inprod(Z[i, ], theta)",
            "W[i] ~ dbern(p.occ[i])",
        "}",
        "for (j in 1:num.cov.occ) {",
            "beta[j] ~ dnorm(prior.occ[j,1], prior.occ[j,2])",
        "}",
        "for (j in 1:num.cov.det) {",
            "theta[j] ~ dnorm(prior.det[j,1], prior.det[j,2])",
        "}",
    "}")

    ## evaluation starts here
    method <- match.arg(method)
    X <- occ
    Z <- det
    Y <- obs
    observations <- Y
    N.sites <- length(Y)
    num.cov.occ <- NCOL(X)
    num.cov.det <- NCOL(Z)
    linkinvfun.occ <- binomial(link.occ)$linkinv
    linkinvfun.det <- binomial(link.det)$linkinv
    ## GLM estimates
    glm.occ <- glm.fit(X, Y, family = binomial(link.occ))
    glm.det <- glm.fit(Z, Y, family = binomial(link.det))
    coef.occ <- coef(glm.occ)
    coef.occ[is.na(coef.occ)] <- 0
    coef.det <- coef(glm.det)
    coef.det[is.na(coef.det)] <- 0

    control.optim <- getOption("occupy.optim.control")
#    control.mcmc <- getOption("occupy.mcmc.control")
    opmeth <- getOption("occupy.optim.method")
    pmle.problem <- FALSE
    converged <- c(mle=NA, pmle=NA)

    if (missing(inits)) {
        inits <- if (method=="dc")
            NULL else c(coef.occ, coef.det)
    }

    ## MLE from MCMC
    if (method=="dc") {
        ## prior specifications
        prior.occ <- cbind(coef.occ, rep(prec, length(coef.occ)))
        prior.det <- cbind(coef.det, rep(prec, length(coef.det)))
        excl.occ <- switch(link.occ,
            "logit"=c(6,8),
            "probit"=c(4,8),
            "cloglog"=c(4,6))
        excl.det <- switch(link.det,
            "logit"=c(7,9),
            "probit"=c(5,9),
            "cloglog"=c(5,7))
        model <- dclone:::custommodel(mcmcSS.all, c(excl.occ, excl.det))
        dat <- dclone(list(Y=obs, X=occ, Z=det,
            N.sites=N.sites, num.cov.occ=num.cov.occ, num.cov.det=num.cov.det,
            prior.occ=prior.occ, prior.det=prior.det), n.clones,
            unchanged=c("num.cov.occ","num.cov.det","prior.occ","prior.det"), 
            multiply="N.sites")
        ## old specification, depending on options
#        mle.res <- jags.fit(dat, c("beta", "theta"), model, inits,
#            n.chains=control.mcmc$n.chains, n.adapt=control.mcmc$n.adapt, 
#            n.update=control.mcmc$n.update, n.iter=control.mcmc$n.iter, thin=control.mcmc$thin, ...)
        ## new specification
        mle.res <- jags.engine(dat, c("beta", "theta"), model, inits, cl=cl, ...)
    }
    ## MLE from optim
    if (method=="optim") {
        mle.res <- optim(inits, singleocc.MLE, gr = NULL, method = opmeth, hessian = TRUE,
            control = control.optim,
            observations = observations, X = X, Z = Z, link1 = link.occ, link2 = link.det)
        if (rcond(mle.res$hessian) < 1e-06 && penalized) {
            warning("cannot calculate PMLE, condition number too small")
            penalized <- FALSE
        }
    }
    ## PMLE
    if (penalized) {
        if (method=="dc") {
            mle.parameters <- coef(mle.res)
            vv <- dcsd(mle.res)^2
        }
        if (method=="optim") {
            mle.parameters <- mle.res$par
            vv <- -diag(solve(mle.res$hessian))
        }
        mle.par.occ <- mle.parameters[1:num.cov.occ]
        mle.par.det <- mle.parameters[-(1:num.cov.occ)]
        mle.phi <- linkinvfun.occ(X %*% mle.par.occ)
        mle.delta <- linkinvfun.det(Z %*% mle.par.det)
        ## sometimes negative variance values occurr (strange but true)
        ## in this case MLE is returned, not PMLE, but attr is there to remember this unintended change
        if (any(vv < 0)) {
            pmle.res <- NULL
            result <- mle.res
            penalized <- FALSE
            pmle.problem <- TRUE
            warning("negative variance values in optim, penalization not used")
        } else {
            vv.occ <- sqrt(sum(vv[1:num.cov.occ]))
            vv.det <- sqrt(sum(vv[-(1:num.cov.occ)]))
            # parameters, observations, X, Z, bmax, dmax, psi0, d0, link1, link2, v1, v2
            # v1:Square root of the sum of the estimated variance for the detection parameters obtained by the MLE
            # v2:Square root of the sum of the estimated variance for the occupancy parameters obtained by the MLE
            pmle.res <- optim(inits, singleocc.PMLE, gr = NULL, method = "BFGS", hessian = TRUE,
                    control = control.optim, 
                    observations = observations, X = X, Z = Z, 
                    bmax = coef.occ, dmax = coef.det,
                    psi0 = mean(mle.phi), d0 = mean(mle.delta),
                    link1=link.occ, link2=link.det,
                    v1 = vv.occ, v2 = vv.det)
            result <- pmle.res
        }
    } else {
        pmle.res <- NULL
        result <- mle.res
    }

    ## final evaluation of results
    if (method=="dc" && !penalized) {
        std.error <- dcsd(mle.res)
        parameters <- coef(mle.res)
        loglik <- singleocc.MLE(parameters, observations, X, Z, link1 = link.occ, link2 = link.det)
    } else {
        if (rcond(result$hessian) <= 1e-06)
            std.error <- rep(NA, num.cov.occ + num.cov.det)
        if (rcond(result$hessian) > 1e-06) {
            opvar <- -diag(solve(result$hessian))
            if (any(opvar < 0)) {
                opvar[opvar < 0] <- NA
                ## if pmle.problem=TRUE, this warning would be repeated
                if (!pmle.problem)
                    warning("negative variance values in optim, NAs produced")
            }
            std.error <- sqrt(opvar)
        }
        parameters <- result$par
        loglik <- result$value
    }

    converged[1] <- if (method=="dc")
        gelman.diag(mle.res)$mpsrf < 1.1 else mle.res$convergence == 0
    if (penalized)
        converged[2] <- pmle.res$convergence == 0
    if (any(converged[!is.na(converged)]==FALSE))
        warning("model did not converge\n")

    ## separating occ and det results
    par.occ <- parameters[1:num.cov.occ]
    par.det <- parameters[-(1:num.cov.occ)]
    phi <- linkinvfun.occ(X %*% par.occ)
    delta <- linkinvfun.det(Z %*% par.det)
    se.occ <- std.error[1:num.cov.occ]
    se.det <- std.error[-(1:num.cov.occ)]
    temp <- (phi * (1 - delta))/((1 - phi) + (phi * (1 - delta)))
    phi.obs <- ((observations == 1) * 1) + ((observations == 0) * temp)
    names(par.occ) <- names(se.occ) <- colnames(X)
    names(par.det) <- names(se.det) <- colnames(Z)

    ## AUC calculation
    if (auc) {
        zp <- (phi * delta)[observations == 0]
        op <- (phi * delta)[observations == 1]
        u <- sum(unlist(lapply(op, function(z) sum(zp < z))))
        auc.out <- u / (sum(observations) * (length(observations) - sum(observations)))
    }

    ## assembling return object
    out <- list(coefficients = list(occ = par.occ, det = par.det),
        std.error = list(occ = se.occ, det = se.det), 
        estimated.probabilities = as.numeric(temp), 
        fitted.values = as.numeric(phi.obs), 
        method = method,
        link = c(occ=link.occ, det=link.det),
        n = N.sites, 
        df.null = N.sites - 2,
        df.residual = N.sites - num.cov.occ - num.cov.det, 
        start = inits, 
        phi = as.numeric(phi),
        delta = as.numeric(delta), 
        loglik = loglik, 
        results = list(glm.occ = glm.occ, glm.det = glm.det, mle = mle.res, pmle = pmle.res),
        converged = converged, 
        penalized = penalized,
        y = observations)
    if (auc)
        out$auc <- auc.out
    return(out)
}
