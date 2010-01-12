library(dclone)

## data generation
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)

beta1 <- c(2, -1)
mu1 <- X %*% beta1
Y1 <- rpois(n, exp(mu1))

beta2 <- c(0, -1)
mu2 <- X %*% beta2
Y2 <- rbinom(n, 1, exp(mu2) / (1 + exp(mu2)))

## GLM and logLik
m1 <- glm(Y1 ~ x, family=poisson)
m2 <- glm(Y2 ~ x, family=binomial)
lambda.hat <- exp(X %*% coef(m1))
sum(log(lambda.hat^Y1 * exp(-lambda.hat)) - log(factorial(Y1)))
logLik(m1)
p.hat <- exp(X %*% coef(m2)) / (1 + exp(X %*% coef(m2)))
sum(log(choose(1, Y2) * p.hat^Y2 * (1-p.hat)^(1-Y2)))
logLik(m2)

## Poisson case
glm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
#        Y[i] ~ dbin(p[i], N[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
#        logit(p[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}

n.clones <- 5
dat <- list(n = length(Y1), Y = Y1, X = X, np = ncol(X))
dcdat <- dclone(dat, n.clones, multiply = "n", unchanged = "np")
mod1 <- jags.fit(dcdat, "beta", glm.model)

cbind(glm.estimates=coef(m1), glm.se=summary(m1)$coefficients[,2],
    dc.estimates=coef(mod1), dc.se=dcsd(mod1))

## Binomial case
glm.model <- function() {
    for (i in 1:n) {
#        Y[i] ~ dpois(lambda[i])
        Y[i] ~ dbin(p[i], k)
#        log(lambda[i]) <- inprod(X[i,], beta[1,])
        logit(p[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}

n.clones <- 5
dat <- list(n = length(Y2), Y = Y2, k=1, X = X, np = ncol(X))
dcdat <- dclone(dat, n.clones, multiply = c("Y","k"), unchanged = c("n", "np", "X"))
mod2 <- jags.fit(dcdat, "beta", glm.model)

cbind(glm.estimates=coef(m2), glm.se=summary(m2)$coefficients[,2],
    dc.estimates=coef(mod2), dc.se=dcsd(mod2))

glm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        Y[i] ~ dbin(p[i], k)
        log(lambda[i]) <- inprod(X[i,], beta[1,])
        logit(p[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}

custommodel(glm.model, c(4,6))
custommodel(glm.model, c(3,5))

dcglm <- function(formula, data = parent.frame(), family=c("poisson", "binomial"), n.clones=5, ...){
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
    dat <- list(n = length(Y), Y = Y, X = X, np = ncol(X), k = 1)
    if (family == "poisson") {
        model <- model <- custommodel(glm.model, c(4,6))
        dcdat <- dclone(dat, n.clones, multiply = "n", unchanged = "np")
    } else {
        model <- custommodel(glm.model, c(3,5))
        dcdat <- dclone(dat, n.clones, multiply = c("Y","k"), unchanged = c("n", "np", "X"))
    }
    mod <- jags.fit(dcdat, "beta", model, ...)
    COEF <- coef(mod)
    SE <- dcsd(mod)
    names(COEF) <- names(SE) <- colnames(X)
    mu <- X %*% COEF
    if (family == "poisson") {
        fitval <- exp(mu)
        ll <- sum(log(fitval^Y * exp(-fitval)) - log(factorial(Y)))
    } else {
        fitval <- exp(mu) / (1 + exp(mu))
        ll <- sum(log(choose(1, Y) * fitval^Y * (1-fitval)^(1-Y)))
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

dcm1 <- dcglm(Y1 ~ x)
dcm2 <- dcglm(Y2 ~ x, family = "binomial")

coef.dcglm <- function(object, ...) object$coefficients

fitted.dcglm <- function(object, ...) object$fitted.values

logLik.dcglm <- function (object, ...)
    structure(object$loglik,
        df = object$df.null + 1 - object$df.residual,
        nobs = object$df.null + 1,
        class = "logLik")

confint.dcglm <- function(object, parm, level = 0.95, ...) {
    rval <- confint(object$mcmc, parm, level, ...)
    rownames(rval) <- names(coef(object))
    rval
}

vcov.dcglm <- function(object, ...) {
    rval <- vcov(object$mcmc, ...)
    rownames(rval) <- colnames(rval) <- names(coef(object))
    rval
}

print.dcglm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}

summary.dcglm <- function(object, ...){
    COEF <- coef(object)
    SE <- object$std.error
    z <- COEF / SE
    p <-  2 * pnorm(-abs(z))
    stab <- cbind("Estimate" = COEF, "Std. Error" = SE,
        "z value" = z, "Pr(>|z|)" = p)
    rval <- list(call = object$call, 
        coefficients = stab, 
        loglik = object$loglik,
        df.residual = object$df.residual,
        df.null = object$df.null)
    class(rval) <- "summary.dcglm"
    rval
}

print.summary.dcglm <- 
function (x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...) 
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, digits = digits, signif.stars = signif.stars, na.print = "NA", ...)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}

glm.pred <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(z[i])
        Y[i] ~ dbin(z[i], k)
        log(z[i]) <- mu[i]
        logit(z[i]) <- mu[i]
        mu[i] <- inprod(X[i,], beta[1,])
    }
    beta[1,1:np] <- mvn[1:np]
    mvn[1:np] ~ dmnorm(coefs[], prec[,])
    }

custommodel(glm.pred, c(4,6))
custommodel(glm.pred, c(3,5))

predict.dcglm <- function(object, newdata = NULL, type = c("link", "response"), se = FALSE, ...){
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
        "poisson" = custommodel(glm.pred, c(4,6)),
        "binomial" = custommodel(glm.pred, c(3,5)))
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


package.skeleton("dcglm", c("coef.dcglm","confint.dcglm","dcglm",
    "fitted.dcglm","logLik.dcglm","predict.dcglm",
    "print.dcglm","print.summary.dcglm","summary.dcglm","vcov.dcglm"))
