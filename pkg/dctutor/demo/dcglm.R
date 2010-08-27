###################################################
### chunk number 1: 
###################################################
options(prompt = "R> ", continue = "+   ", useFancyQuotes = FALSE, width = 76)
load("dcglm.tutorial.Rdata")


###################################################
### chunk number 2: 
###################################################
library(dclone)
set.seed(1234)
n <- 20
x <- runif(n, -1, 1)
X <- model.matrix(~x)


###################################################
### chunk number 3: 
###################################################
beta1 <- c(2, -1)
mu1 <- X %*% beta1
Y1 <- rpois(n, exp(mu1))


###################################################
### chunk number 4: 
###################################################
beta2 <- c(0, -1)
mu2 <- X %*% beta2
Y2 <- rbinom(n, 1, exp(mu2) / (1 + exp(mu2)))


###################################################
### chunk number 5: 
###################################################
m1 <- glm(Y1 ~ x, family=poisson)
summary(m1)
m2 <- glm(Y2 ~ x, family=binomial)
summary(m2)


###################################################
### chunk number 6: 
###################################################
glm.pois <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}


###################################################
### chunk number 7: 
###################################################
dat1 <- list(n = length(Y1), Y = Y1, X = X, np = ncol(X))
str(dat1)


###################################################
### chunk number 8: 
###################################################
n.clones <- 5
dcdat1 <- dclone(dat1, n.clones, multiply = "n", unchanged = "np")
str(dcdat1)


###################################################
### chunk number 9:  eval=FALSE
###################################################
## mod1 <- jags.fit(dcdat1, "beta", glm.pois, n.iter = 1000)


###################################################
### chunk number 10: 
###################################################
summary(mod1)


###################################################
### chunk number 11: 
###################################################
cbind(true.values=beta1,
    glm.estimates=coef(m1), glm.se=summary(m1)$coefficients[,2],
    dc.estimates=coef(mod1), dc.se=dcsd(mod1))


###################################################
### chunk number 12: 
###################################################
glm.bin <- function() {
    for (i in 1:n) {
        Y[i] ~ dbin(p[i], k)
        logit(p[i]) <- inprod(X[i,], beta[1,])
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
}


###################################################
### chunk number 13: 
###################################################
dat2 <- list(n = length(Y2), Y = Y2, k = 1, X = X, np = ncol(X))
str(dat2)


###################################################
### chunk number 14: 
###################################################
dcdat2 <- dclone(dat2, n.clones, multiply = c("Y","k"), unchanged = c("n", "np", "X"))
str(dcdat2)


###################################################
### chunk number 15:  eval=FALSE
###################################################
## mod2 <- jags.fit(dcdat2, "beta", glm.bin, n.iter = 1000)


###################################################
### chunk number 16: 
###################################################
summary(mod2)


###################################################
### chunk number 17: 
###################################################
cbind(true.values=beta2,
    glm.estimates=coef(m2), glm.se=summary(m2)$coefficients[,2],
    dc.estimates=coef(mod2), dc.se=dcsd(mod2))


###################################################
### chunk number 18: 
###################################################
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


###################################################
### chunk number 19: 
###################################################
dclone:::custommodel(glm.model, c(4,6))
dclone:::custommodel(glm.model, c(3,5))


###################################################
### chunk number 20: 
###################################################
dcglm <- 
function(formula, data = parent.frame(), 
family=c("poisson", "binomial"), n.clones=5, ...)
{
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
    formula.orig <- formula
    Y <- eval(lhs, data)
    formula[[2]] <- NULL
    rhs <- model.frame(formula, data)
    X <- model.matrix(attr(rhs, "terms"), rhs)
    if (family == "poisson") {
        dat <- list(n = length(Y), Y = Y, X = X, np = ncol(X))
        dcdat <- dclone(dat, n.clones, multiply = "n", unchanged = "np")
        model <- dclone:::custommodel(glm.model, c(4,6))
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
        fitval <- drop(exp(mu))
        ll <-  sum(dpois(Y, fitval, log=TRUE))
    } else {
        fitval <- drop(exp(mu) / (1 + exp(mu)))
        ll <-  sum(dbinom(Y, 1, fitval, log=TRUE))
    }
    rval <- list(call=match.call(),
        mcmc = mod,
        y = Y,
        x = rhs, 
        model = X,
        fitted.values = fitval,
        linear.predictors = mu,
        formula = formula.orig,
        coefficients = COEF,
        std.error = SE,
        loglik = ll,
        family = family,
        df.residual = length(Y) - length(COEF),
        df.null = length(Y) - 1)
    class(rval) <- c("dcglm")
    rval
}


###################################################
### chunk number 21:  eval=FALSE
###################################################
## dcm1 <- dcglm(Y1 ~ x, n.iter = 1000)
## dcm2 <- dcglm(Y2 ~ x, family = "binomial", n.iter = 1000)


###################################################
### chunk number 22: 
###################################################
coef.dcglm <- function(object, ...) object$coefficients
fitted.dcglm <- function(object, ...) object$fitted.values


###################################################
### chunk number 23: 
###################################################
rbind(glm=coef(m1), dcglm=coef(dcm1))
rbind(glm=coef(m2), dcglm=coef(dcm2))
rbind(glm=fitted(m1), dcglm=fitted(dcm1))
rbind(glm=fitted(m2), dcglm=fitted(dcm2))


###################################################
### chunk number 24: 
###################################################
logLik.dcglm <- function (object, ...)
    structure(object$loglik,
        df = object$df.null + 1 - object$df.residual,
        nobs = object$df.null + 1,
        class = "logLik")


###################################################
### chunk number 25: 
###################################################
logLik(m1)
logLik(dcm1)
logLik(m2)
logLik(dcm2)
AIC(m1, dcm1, m2, dcm2)


###################################################
### chunk number 26: 
###################################################
print.dcglm <- function(x, digits = max(3, getOption("digits") - 3), ...) {
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("Coefficients:\n")
    print.default(format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ", x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}


###################################################
### chunk number 27: 
###################################################
dcm1
dcm2


###################################################
### chunk number 28: 
###################################################
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


###################################################
### chunk number 29: 
###################################################
print.summary.dcglm <- 
function (x, digits = max(3, getOption("digits") - 3), 
    signif.stars = getOption("show.signif.stars"), ...) 
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"), "\n", sep = "")
    cat("\nCoefficients:\n")
    printCoefmat(x$coefficients, digits = digits, 
        signif.stars = signif.stars, na.print = "NA", ...)
    cat("\nDegrees of Freedom:", x$df.null, "Total (i.e. Null); ",
        x$df.residual, "Residual\n")
    cat("Log Likelihood:\t   ", format(signif(x$loglik, digits)), "\n")
    invisible(x)
}


###################################################
### chunk number 30: 
###################################################
summary(m1)
summary(dcm1)
summary(m2)
summary(dcm2)


###################################################
### chunk number 31: 
###################################################
confint.dcglm <- function(object, parm, level = 0.95, ...) {
    rval <- confint(object$mcmc, parm, level, ...)
    rownames(rval) <- names(coef(object))
    rval
}


###################################################
### chunk number 32: 
###################################################
confint(m1)
confint(dcm1)
confint(m2)
confint(dcm2)


###################################################
### chunk number 33: 
###################################################
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


###################################################
### chunk number 34: 
###################################################
dclone:::custommodel(glm.pred, c(4,6))
dclone:::custommodel(glm.pred, c(3,5))


###################################################
### chunk number 35: 
###################################################
prec <- make.symmetric(solve(vcov(mod1)))
coefs <- coef(mod1)
prdat <- list(n = nrow(X), X = X, 
    np = ncol(X), k = 1, coefs = coefs, prec = prec)


###################################################
### chunk number 36:  eval=FALSE
###################################################
## prval <- jags.fit(prdat, "z", dclone:::custommodel(glm.pred, c(4,6)), 
##     n.chains = 1, n.iter = 1000)


###################################################
### chunk number 37: 
###################################################
vcov.dcglm <- function(object, ...) {
    rval <- vcov(object$mcmc, ...)
    rownames(rval) <- colnames(rval) <- names(coef(object))
    rval
}


###################################################
### chunk number 38: 
###################################################
vcov(m1)
vcov(dcm1)
vcov(m2)
vcov(dcm2)


###################################################
### chunk number 39: 
###################################################
predict.dcglm <- 
function(object, newdata = NULL, 
type = c("link", "response"), se = FALSE, ...)
{
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
        formul <- object$formula
        formul[[2]] <- NULL
        rhs <- model.frame(formul, newdata)
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
        np = ncol(X), coefs = coefs, prec = prec)
    if (object$family == "binomial")
        prdat[["k"]] <- 1
    prval <- jags.fit(prdat, params, model, ...)
    if (!se) {
        rval <- coef(prval)
    } else {
        rval <- list(fit = coef(prval), 
            se.fit = mcmcapply(prval, sd))
    }
    rval
}


###################################################
### chunk number 40: 
###################################################
px <- data.frame(x=seq(-1, 1, len = 10))
px


###################################################
### chunk number 41: 
###################################################
pm1link <- predict(m1, newdata=px, type="link", se=TRUE)
pm1resp <- predict(m1, newdata=px, type="response", se=TRUE)
pm2link <- predict(m2, newdata=px, type="link", se=TRUE)
pm2resp <- predict(m2, newdata=px, type="response", se=TRUE)


###################################################
### chunk number 42:  eval=FALSE
###################################################
## pdcm1link <- predict(dcm1, newdata=px, type="link", 
##     se=TRUE, n.iter = 1000)
## pdcm1resp <- predict(dcm1, newdata=px, type="response", 
##     se=TRUE, n.iter = 1000)
## pdcm2link <- predict(dcm2, newdata=px, type="link", 
##     se=TRUE, n.iter = 1000)
## pdcm2resp <- predict(dcm2, newdata=px, type="response", 
##     se=TRUE, n.iter = 1000)


###################################################
### chunk number 43: predfig
###################################################
opar <- par(mfrow=c(2,2))
offs <- 0.02
se <- cbind(pm1link$fit - pm1link$se.fit, pm1link$fit + pm1link$se.fit)
plot(px$x-offs, pm1link$fit, col=2, ylim=range(se), xlim=range(px$x-offs, px$x+offs),
    main="Poisson GLM, predictor scale", xlab="linear predictor", ylab="log(lambda)")
errlines(px$x-offs, se, col=2)
se <- cbind(pdcm1link$fit - pdcm1link$se.fit, pdcm1link$fit + pdcm1link$se.fit)
points(px$x+offs, pdcm1link$fit, ylim=range(se), col=4)
errlines(px$x+offs, se, col=4)
legend("topright", col=c(2,4), lty=1, pch=21, legend=c("glm", "dcglm"))
se <- cbind(pm1resp$fit - pm1resp$se.fit, pm1resp$fit + pm1resp$se.fit)
plot(px$x-offs, pm1resp$fit, col=2, ylim=range(se), xlim=range(px$x-offs, px$x+offs),
    main="Poisson GLM, response scale", xlab="linear predictor", ylab="lambda")
errlines(px$x-offs, se, col=2)
se <- cbind(pdcm1resp$fit - pdcm1resp$se.fit, pdcm1resp$fit + pdcm1resp$se.fit)
points(px$x+offs, pdcm1resp$fit, ylim=range(se), col=4)
errlines(px$x+offs, se, col=4)
legend("topright", col=c(2,4), lty=1, pch=21, legend=c("glm", "dcglm"))
se <- cbind(pm2link$fit - pm2link$se.fit, pm2link$fit + pm2link$se.fit)
plot(px$x-offs, pm2link$fit, col=2, ylim=range(se), xlim=range(px$x-offs, px$x+offs),
    main="Binomial GLM, predictor scale", xlab="linear predictor", ylab="logit(p)")
errlines(px$x-offs, se, col=2)
se <- cbind(pdcm2link$fit - pdcm2link$se.fit, pdcm2link$fit + pdcm2link$se.fit)
points(px$x+offs, pdcm2link$fit, ylim=range(se), col=4)
errlines(px$x+offs, se, col=4)
legend("topright", col=c(2,4), lty=1, pch=21, legend=c("glm", "dcglm"))
se <- cbind(pm2resp$fit - pm2resp$se.fit, pm2resp$fit + pm2resp$se.fit)
plot(px$x-offs, pm2resp$fit, col=2, ylim=range(se), xlim=range(px$x-offs, px$x+offs),
    main="Binomial GLM, response scale", xlab="linear predictor", ylab="p")
errlines(px$x-offs, se, col=2)
se <- cbind(pdcm2resp$fit - pdcm2resp$se.fit, pdcm2resp$fit + pdcm2resp$se.fit)
points(px$x+offs, pdcm2resp$fit, ylim=range(se), col=4)
errlines(px$x+offs, se, col=4)
legend("topright", col=c(2,4), lty=1, pch=21, legend=c("glm", "dcglm"))
par(opar)


###################################################
### chunk number 44:  eval=FALSE
###################################################
## package.skeleton("dcglm", c("coef.dcglm","confint.dcglm","dcglm",
##     "fitted.dcglm","logLik.dcglm","predict.dcglm",
##     "print.dcglm","print.summary.dcglm","summary.dcglm","vcov.dcglm"))


