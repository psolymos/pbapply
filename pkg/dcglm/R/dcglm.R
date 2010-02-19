dcglm <-
function(formula, data = parent.frame(), family=c("poisson", "binomial"), n.clones=5, ...){
    glm.model <- c("model {",
                   "    for (i in 1:n) {",
                   "        Y[i] ~ dpois(lambda[i])",
                   "        Y[i] ~ dbin(p[i], k)",
                   "        log(lambda[i]) <- inprod(X[i,], beta[1,])",
                   "        logit(p[i]) <- inprod(X[i,], beta[1,])",
                   "    }",
                   "    for (j in 1:np) {",
                   "        beta[1,j] ~ dnorm(0, 0.001)",
                   "    }",
                   "}")
    family <- match.arg(family)
    lhs <- formula[[2]]
    Y <- eval(lhs, data)
    formula[[2]] <- NULL
    rhs <- model.frame(formula, data)
    X <- model.matrix(attr(rhs, "terms"), rhs)

    if (family == "poisson") {
        dat <- list(n = length(Y), Y = Y, X = X, np = ncol(X))
        dcdat <- dclone(dat, n.clones, multiply = "n", unchanged = "np")
        model <- model <- dclone:::custommodel(glm.model, c(4,6))
    } else {
        dat <- list(n = length(Y), Y = Y, X = X, np = ncol(X), k = 1)
        dcdat <- dclone(dat, n.clones, multiply = c("Y","k"), unchanged = c("n", "np", "X"))
        model <- dclone:::custommodel(glm.model, c(3,5))
    }
    mod <- jags.fit(dcdat, "beta", model, ...)
    COEF <- coef(mod)
    SE <- dcsd(mod)
    names(COEF) <- names(SE) <- colnames(X)
    mu <- X %*% COEF
    if (family == "poisson") {
        fitval <- exp(mu)
        ll <-  sum(dpois(Y, fitval, log=TRUE))
    } else {
        fitval <- exp(mu) / (1 + exp(mu))
        ll <-  sum(dbinom(Y, 1, fitval, log=TRUE))
    }
    rval <- list(call=match.call(),
        mcmc = mod,
        y = Y,
        x = rhs, 
        model = X,
        fitted.values = fitval,
        linear.predictors = mu,
        formula = formula,
        coefficients = COEF,
        std.error = SE,
        loglik = ll,
        family = family,
        df.residual = length(Y) - length(COEF),
        df.null = length(Y) - 1)
    class(rval) <- c("dcglm")
    rval
}

