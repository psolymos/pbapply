predict.dcglm <-
function(object, newdata = NULL, type = c("link", "response"), se = FALSE, ...){
    glm.pred <- c("model {",
           "    for (i in 1:n) {",
           "        Y[i] ~ dpois(z[i])",
           "        Y[i] ~ dbin(z[i], k)",
           "        log(z[i]) <- mu[i]",
           "        logit(z[i]) <- mu[i]",
           "        mu[i] <- inprod(X[i,], beta[1,])",
           "    }",
           "    beta[1,1:np] <- mvn[1:np]",
           "    mvn[1:np] ~ dmnorm(coefs[], prec[,])",
           "    }")
    prec <- make.symmetric(solve(vcov(object)))
    coefs <- coef(object)
    if (is.null(newdata)) {
        X <- object$model
    } else {
        rhs <- model.frame(object$formula, newdata)
        X <- model.matrix(attr(rhs, "terms"), rhs)
    }
    type <- match.arg(type)
    params <- switch(type,
        "link" = "mu",
        "response" = "z")
    model <- switch(object$family,
        "poisson" = dclone:::custommodel(glm.pred, c(4,6)),
        "binomial" = dclone:::custommodel(glm.pred, c(3,5)))
    prdat <- list(n = nrow(X), X = X, 
        np = ncol(X), k = 1, coefs = coefs, prec = prec)
    prval <- jags.fit(prdat, params, model, ...)
    if (!se) {
        rval <- coef(prval)
    } else {
        rval <- list(fit = coef(prval), 
            se.fit = mcmcapply(prval, sd))
    }
    rval
}

