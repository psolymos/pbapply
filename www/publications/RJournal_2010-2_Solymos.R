###################################################
### chunk number 1: setup
###################################################
library(dclone)


###################################################
### chunk number 2: datagen
###################################################
library(dclone)
set.seed(1234)
n <- 50
beta <- c(1.8, -0.9)
sigma <- 0.2
x <- runif(n, min = 0, max = 1)
X <- model.matrix(~ x)
alpha <- rnorm(n, mean = 0, sd = sigma)
lambda <- exp(alpha + drop(X %*% beta))
Y <- rpois(n, lambda)


###################################################
### chunk number 3: bugsmodel
###################################################
glmm.model <- function() {
   for (i in 1:n) {
      Y[i] ~ dpois(lambda[i])
      lambda[i] <- exp(alpha[i] +
         inprod(X[i,], beta[1,]))
      alpha[i] ~ dnorm(0, tau)
   }
   for (j in 1:np) {
      beta[1,j] ~ dnorm(0, 0.001)
   }
   log.sigma ~ dnorm(0, 0.001)
   sigma <- exp(log.sigma)
   tau <- 1 / pow(sigma, 2)
}


###################################################
### chunk number 4: bayesian eval=FALSE
###################################################
dat <- list(Y = Y, X = X, n = n,
   np = ncol(X))
mod <- jags.fit(dat, 
   c("beta", "sigma"), glmm.model, n.iter = 1000)


###################################################
### chunk number 5: winbugs
###################################################
glmm.model.bugs <- function() {
   for (i in 1:n) {
      Y[i] ~ dpois(lambda[i])
      lambda[i] <- exp(alpha[i] +
         inprod(X[i,], beta[1,]))
      alpha[i] ~ dnorm(0, tau) %_% I(-5, 5)
   }
   for (j in 1:np) {
      beta[1,j] ~ dnorm(0, 0.01) %_% I(-5, 5)
   }
   log.sigma ~ dnorm(0, 0.01) %_% I(-5, 5)
   sigma <- exp(log.sigma)
   tau <- 1 / pow(sigma, 2)
}


###################################################
### chunk number 6: winmugsmodels eval=FALSE
###################################################
mod.wb <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1)
mod.ob <- bugs.fit(dat, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1)


###################################################
### chunk number 7: comparison
###################################################
sapply(list(JAGS = mod, WinBUGS = mod.wb, 
   OpenBUGS = mod.ob), coef)


###################################################
### chunk number 8: dc.ex
###################################################
dclone(1:5, 1)
dclone(1:5, 2)
dclone(matrix(1:4, 2, 2), 2)
dclone(data.frame(a=1:2, b=3:4), 2)


###################################################
### chunk number 9: dc.list
###################################################
dat2 <- dclone(dat, n.clones = 2, 
    multiply = "n", unchanged = "np")
nclones(dat2)


###################################################
### chunk number 10: clone2 eval=FALSE
###################################################
mod2 <- jags.fit(dat2, 
   c("beta", "sigma"), glmm.model, n.iter = 1000)


###################################################
### chunk number 11: clone2winbugs eval=FALSE
###################################################
mod.wb2 <- bugs.fit(dat2, c("beta", "sigma"), 
   glmm.model.bugs, DIC = FALSE, n.thin = 1)
mod.ob2 <- bugs.fit(dat2, c("beta", "sigma"), 
   glmm.model.bugs, program = "openbugs", 
   DIC = FALSE, n.thin = 1)


###################################################
### chunk number 12: comparison2
###################################################
sapply(list(JAGS = mod2, WinBUGS = mod.wb2, 
   OpenBUGS = mod.ob2), coef)


###################################################
### chunk number 13: dcdim
###################################################
(obj <- dclone(dcdim(data.matrix(1:5)), 2))


###################################################
### chunk number 14: bhmod
###################################################
beverton.holt <- function() {
   for (j in 1:k) {
      for(i in 2:(n+1)){
         Y[(i-1),j] ~ dpois(exp(log.N[i,j]))
         log.N[i,j] ~ dnorm(mu[i,j], 1 / sigma^2)
         mu[i,j] <- log(lambda) + log.N[(i-1),j] 
             - log(1 + beta * exp(log.N[(i-1),j]))
      }
      log.N[1,j] ~ dnorm(mu0, 1 / sigma^2)
   }
   beta ~ dlnorm(-1, 1)
   sigma ~ dlnorm(0, 1)
   tmp ~ dlnorm(0, 1)
   lambda <- tmp + 1
   mu0 <- log(lambda) + log(2) - log(1 + beta * 2)
}


###################################################
### chunk number 15: bhdata
###################################################
paurelia <- c(17, 29, 39, 63, 185, 258, 267, 
   392, 510, 570, 650, 560, 575, 650, 550, 
   480, 520, 500)
bhdat <- list(Y=dcdim(data.matrix(paurelia)),
   n=length(paurelia), k=1)
dcbhdat <- dclone(bhdat, n.clones = 5, 
   multiply = "k", unchanged = "n")


###################################################
### chunk number 16: bhmod eval=FALSE
###################################################
bhmod <- jags.fit(dcbhdat, 
   c("lambda","beta","sigma"), beverton.holt, 
   n.iter=1000)


###################################################
### chunk number 17: bhres
###################################################
coef(bhmod)


###################################################
### chunk number 18: update
###################################################
glmm.model.up <- function() {
   for (i in 1:n) {
      Y[i] ~ dpois(lambda[i])
      lambda[i] <- exp(alpha[i] + 
         inprod(X[i,], beta[1,]))
      alpha[i] ~ dnorm(0, 1/sigma^2)
   }
   for (j in 1:np) {
      beta[1,j] ~ dnorm(pr[j,1], pr[j,2])
   }
   log.sigma ~ dnorm(pr[(np+1),1], pr[(np+1),2])
   sigma <- exp(log.sigma)
   tau <- 1 / pow(sigma, 2)
}


###################################################
### chunk number 19: upfun
###################################################
upfun <- function(x) {
   if (missing(x)) {
      np <- ncol(X)
      return(cbind(rep(0, np+1),
          rep(0.001, np+1)))
   } else {
      ncl <- nclones(x)
      if (is.null(ncl))
         ncl <- 1
      par <- coef(x)
      se <- dcsd(x)
      log.sigma <- mcmcapply(x[,"sigma"], log)
      par[length(par)] <- mean(log.sigma)
      se[length(se)] <- sd(log.sigma) * sqrt(ncl)
      return(cbind(par, se))
   }
}


###################################################
### chunk number 20: dcmod eval=FALSE
###################################################
updat <- list(Y = Y, X = X, n = n, 
   np = ncol(X), pr = upfun())
k <- c(1, 5, 10, 20)
dcmod <- dc.fit(updat, c("beta", "sigma"), 
   glmm.model.up, n.clones = k, n.iter = 1000,
   multiply = "n", unchanged = "np",
   update = "pr", updatefun = upfun)


###################################################
### chunk number 21: dcmodsumm
###################################################
summary(dcmod)


###################################################
### chunk number 22: figdct
###################################################
dct <- dctable(dcmod)
plot(dct)


###################################################
### chunk number 23: figdctsd
###################################################
plot(dct, type="log.var")


###################################################
### chunk number 24: dcdiag
###################################################
dcdiag(dcmod)


###################################################
### chunk number 25: nn
###################################################
gamma <- 2.5
sigma <- 0.2
tau <- 0.5
set.seed(2345)
mu <- rnorm(n, gamma, tau)
Y <- rnorm(n, mu, sigma)
nn.model <- function() {
   for (i in 1:n) {
      Y[i] ~ dnorm(mu[i], prec1)
      mu[i] ~ dnorm(gamma, prec2)
   }
   gamma ~ dnorm(0, 0.001)
   log.sigma ~ dnorm(0, 0.001)
   sigma <- exp(log.sigma)
   prec1 <- 1 / pow(sigma, 2)
   log.tau ~ dnorm(0, 0.001)
   tau <- exp(log.tau)
   prec2 <- 1 / pow(tau, 2)
}
nndat <- list(Y = Y, n = n)


###################################################
### chunk number 26: nnmod eval=FALSE
###################################################
nnmod <- dc.fit(nndat, c("gamma","sigma","tau"), 
   nn.model, n.clones=c(1,10,20,30,40,50), 
   n.iter=1000, multiply="n")


###################################################
### chunk number 27: nnres
###################################################
dcdiag(nnmod)
vars <- mcmcapply(nnmod[,c("sigma","tau")],
   array)^2
sigma^2 + tau^2
summary(rowSums(vars))


###################################################
### chunk number 28: methods
###################################################
coef(dcmod)
dcsd(dcmod)
mcmcapply(dcmod, sd) * sqrt(nclones(dcmod))


###################################################
### chunk number 29: methods2
###################################################
confint(dcmod)
vcov(dcmod)


###################################################
### chunk number 30: predmod
###################################################
glmm.pred <- function() {
   for (i in 1:n) {
      Y[i] ~ dpois(lambda[i])
      lambda[i] <- exp(mu[i])
      mu[i] <- alpha[i] + 
         inprod(X[i,], beta[1,])
      alpha[i] ~ dnorm(0, tau)
   }
   tmp[1:(np+1)] ~ dmnorm(param[], prec[,])
   beta[1,1:np] <- tmp[1:np]
   sigma <- tmp[(np+1)]
   tau <- 1 / pow(sigma, 2)
}


###################################################
### chunk number 31: predict eval=FALSE
###################################################
prec <- make.symmetric(solve(vcov(dcmod)))
prdat <- list(X = X, n = nrow(X), np = ncol(X), 
   param = coef(dcmod), prec = prec)
prmod <- jags.fit(prdat, "lambda", glmm.pred, 
   n.iter = 1000)


###################################################
### chunk number 32: confun
###################################################
glmmPois <- function(formula, 
data = parent.frame(), n.clones, ...) {
   lhs <- formula[[2]]
   Y <- eval(lhs, data)
   formula[[2]] <- NULL
   rhs <- model.frame(formula, data)
   X <- model.matrix(attr(rhs, "terms"), rhs)
   dat <- list(n = length(Y), Y = Y, 
      X = X, np = ncol(X))
   dcdat <- dclone(dat, n.clones, 
      multiply = "n", unchanged = "np")
   mod <- jags.fit(dcdat, c("beta", "sigma"), 
      glmm.model, ...)
   coefs <- coef(mod)
   names(coefs) <- c(colnames(X), 
      "sigma")
   rval <- list(coefficients = coefs,
      call = match.call(),
      mcmc = mod, y = Y, x = rhs, 
      model = X, formula = formula)
   class(rval) <- "glmmPois"
   rval
}
print.glmmPois <- function(x, ...) {
   cat("glmmPois model\n\n")
   print(format(coef(x), digits = 4), 
      print.gap = 2, quote = FALSE)
   cat("\n")
   invisible(x)
}
summary.glmmPois <- function(object, ...) {
   x <- cbind("Estimate" = coef(object),
      "Std. Error" = dcsd(object$mcmc),
      confint(object$mcmc))
   cat("Call:", deparse(object$call, 
      width.cutoff = getOption("width")), 
      "\n", sep="\n")
   cat("glmmPois model\n\n")
   printCoefmat(x, ...)
   cat("\n")
   invisible(x)
}
predict.glmmPois <- function(object, 
newdata = NULL, type = c("mu", "lambda", "Y"), 
level = 0.95, ...){
   prec <- solve(vcov(object$mcmc))
   prec <- make.symmetric(prec)
   param <- coef(object)
   if (is.null(newdata)) {
      X <- object$model
   } else {
      rhs <- model.frame(object$formula, newdata)
      X <- model.matrix(attr(rhs, "terms"), rhs)
   }
   type <- match.arg(type)
   prdat <- list(n = nrow(X), X = X, 
      np = ncol(X), param = param, prec = prec)
   prval <- jags.fit(prdat, type, glmm.pred, ...)
   a <- (1 - level)/2
   a <- c(a, 1 - a)
   rval <- list(fit = coef(prval), 
      ci.fit = quantile(prval, probs = a))
   rval
}


###################################################
### chunk number 33: cheads eval=FALSE
###################################################
data(ovenbird)
obmod <- glmmPois(count ~ uplow + thd, 
   ovenbird, n.clones = 5, n.update = 1000, 
   n.iter = 1000)


###################################################
### chunk number 34: obmodsumm
###################################################
obmod
summary(obmod)


###################################################
### chunk number 35: obpred eval=FALSE
###################################################
thd <- seq(0, 100, len = 101)
ndata <- data.frame(uplow = rep("lowland", 
   length(thd)), thd = thd)
levels(ndata$uplow) <- levels(ovenbird$uplow)
obpred <- predict(obmod, ndata, "lambda")


###################################################
### chunk number 36: predfig
###################################################
toPlot <- data.frame(thd=ndata$thd, 
   lambda=obpred$fit, t(obpred$ci.fit))
with(toPlot, plot(lambda ~ thd, type="n", 
   ylim=range(toPlot[,-1]), las=1))
polygon(c(toPlot$thd, rev(toPlot$thd)), 
   c(toPlot[,3], rev(toPlot[,4])), 
   col="lightgrey", border=NA)
lines(toPlot[,2] ~ toPlot$thd, lwd=2)
with(ovenbird, points(count ~ thd))
