#### (1) Bayesian toolkit (coda and rjags packages)

## load dclone package
library(dclone)
## generate data
set.seed(1234)
n <- 20
beta <- c(1, -1)
sigma <- 0.5
x <- runif(n)
X <- model.matrix(~x)
mu <- drop(X %*% beta)
Y <- rnorm(n, mean = mu, sd = sigma)


lm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dnorm(mu[i], 1/sigma^2)
        mu[i] <- inprod(X[i,], beta[])
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0.0, 1.0E-4)
    }
    sigma ~ dgamma(1.0E-3, 1.0E-3)
}
## list of data for the model
dat <- list(n = n, Y = Y, X = X, np=ncol(X))
## the JAGS way of adatpting/updating BUGS models
## and ways of defining the BUGS models
model <- write.jags.model(lm.model)
model
## without data arg
np <- ncol(X)
m0 <- jags.model(model, n.chains=3, n.adapt=1000)
## with data arg
m <- jags.model(model, dat, n.chains=3, n.adapt=1000) # 1000
m
str(m)
update(m, 1000) # 2000
mod <- coda.samples(m, c("beta", "sigma"), n.iter=1000, thin = 1) #3000
clean.jags.model(model)
summary(mod)
## further updating
update(m, 1000) # 4000
mod1 <- coda.samples(m, c("beta", "sigma"), n.iter=1000, thin = 1) # 5000
summary(mod1)
## structure of an mcmc.list object
str(mod)
## coda tools
plot(mod)
gelman.diag(mod)
gelman.plot(mod)
## subsetting mcmc.objects
summary(mod[,1:2])
## minor but useful methods
nvar(mod)
varnames(mod)
niter(mod)
thin(mod)
start(mod)
end(mod)
nchain(mod)
## setting variable names
varnames(mod) <- c("beta1", "beta2", "sigma")
summary(mod)

#### (2) the jags.fit wrapper in  dclone

## 1 line istead of 5 (note, start=n.adapt+n.update+1, end=n.adapt+n.update+n.iter)
mod2 <- jags.fit(dat, c("beta", "sigma"), lm.model, n.chains=3, n.update=1000, n.iter=1000, thin=1) # 3000
summary(mod2)

## further updating, the rjags way
update(updated.model(mod2), 1000) # 4000
mod3 <- coda.samples(updated.model(mod2), c("beta", "sigma"), n.iter=1000, thin = 1) # 5000
summary(mod3)

## autoupdate for mcmc.list, the dclone way (why? -- see later)
mm2 <- update(mod2, times=1)
mm3 <- update(mm2, times=1)

## initial values
initsfun1 <- function(i) list(beta=rep(rnorm(1), dat$np), sigma=rlnorm(1))
initsfun1()
## or with RNG seed
initsfun2 <- function(i) list(beta=rep(rnorm(1), dat$np), sigma=rlnorm(1),
    ".RNG.name"="base::Wichmann-Hill", ".RNG.seed"=rpois(1, 1000))
initsfun1()
mod4 <- jags.fit(dat, c("beta", "sigma"), lm.model, inits=initsfun1)
mod5 <- jags.fit(dat, c("beta", "sigma"), lm.model, inits=initsfun2)

## sampling parts of vector nodes
mod6 <- jags.fit(dat, c("beta[2]", "mu[2:4]"), lm.model)
summary(mod6)

## progress.bar
tmp <- jags.fit(dat, c("beta", "sigma"), lm.model, progress.bar="text")
tmp <- jags.fit(dat, c("beta", "sigma"), lm.model, progress.bar="none")
tmp <- jags.fit(dat, c("beta", "sigma"), lm.model, progress.bar="gui", by=2)
## update can only be interrupted when the progress bar is refreshed
## default 'by' argument is either min(n.iter/50, 100) (this is happening in update.jags)


#### (3) convenient data structure definitions for data cloning

n.clones <- 2
## rep version
dclone(1, n.clones)
dclone(1:4, n.clones)
dclone(1:4, n.clones, attrib=FALSE) # needed for WinBUGS
dclone(1:4)
dclone(matrix(1:12, 3, 4), n.clones)
dclone(data.frame(a=1:2, b=c(TRUE, FALSE)), n.clones)
## dim version
dclone(as.ts(1:4), n.clones)
dclone(dcdim(1:4), n.clones)
dclone(dcdim(matrix(1:12, 3, 4)), n.clones)
## drop
dm <- data.matrix(1:4)
dm
dclone(dcdim(dm), n.clones)

## how many clones?
nclones(dclone(1:4, 1))
nclones(dclone(1:4, n.clones))
nclones(dclone(dcdim(1:4), n.clones))

## cloning lists, data definitions
dcdat <- dclone(dat, n.clones, multiply="n", unchanged="np")
str(dat)
str(dcdat)
nclones(dcdat)

## the most important thing!
cat("\nLet's have a break!\n\n")

#### (4) fitting the model with cloned data

mod7 <- jags.fit(dcdat, c("beta", "sigma"), lm.model)
nclones(mod7)
nclones(mod)
## note DC SD and R_hat values
summary(mod7)

## further updating, the rjags way
update(updated.model(mod7), 1000)
mod7x <- coda.samples(updated.model(mod7), c("beta", "sigma"), n.iter=1000, thin = 1)
nclones(mod7x)
## what if we loose track of n.clones attributes? (note -- this is the undocumented hacker's interface :)
mod7x <- as.mcmc.list.dc(mod7x, nclones(mod7))
nclones(mod7x)

## autoupdate, the dclone way
mod8 <- jags.fit(dcdat, c("beta", "sigma"), lm.model)
dcmm2 <- update(mod8, times=1)
dcmm3 <- update(dcmm2, times=1)
sapply(list(mod7, dcmm2, dcmm3), function(z) c(start(z), end(z), nclones(z)))
## other args in update: fun

#### (5) DC diagnostics

## dctable
dct <- dctable(mod7)
dct
plot(dct)

## iterative model fitting (not a learning process)
dcm1 <- jags.fit(dclone(dat, 1, multiply="n", unchanged="np"), c("beta", "sigma"), lm.model)
dcm2 <- jags.fit(dclone(dat, 2, multiply="n", unchanged="np"), c("beta", "sigma"), lm.model)
dcm5 <- jags.fit(dclone(dat, 5, multiply="n", unchanged="np"), c("beta", "sigma"), lm.model)
dcm10 <- jags.fit(dclone(dat, 10, multiply="n", unchanged="np"), c("beta", "sigma"), lm.model)

## dctable for multiple models
dct2 <- dctable(dcm1, dcm2, dcm5, dcm10)
dct2
plot(dct2)
plot(dct2, 2:3)
plot(dct2, type="var")

## diagnostics
lambdamax.diag(dcm1)
lambdamax.diag(dcm10)
c1 <- chisq.diag(dcm1)
c2 <- chisq.diag(dcm10)
c1
c2
plot(c1)
plot(c2)

## dcdiag
dcdiag(dcm1)
dcd <- dcdiag(dcm1, dcm2, dcm5, dcm10)
dcd
plot(dcd)

## useful functions (methods) for data cloned MCMC objects
confint(dcm10)
vcov(dcm10)
coef(dcm10)
dcsd(dcm10)

#### (6) iterative model fitting for data cloning (learning process)

set.seed(1234)
n <- 100
beta <- c(1, -1)
sigma <- 0.5
x <- runif(n)
X <- model.matrix(~x)
sigma <- 0.25
alpha <- rnorm(n, 0, sd=sigma)
mu <- drop(X %*% beta) + alpha
Y <- rpois(n, exp(mu))
## JAGS model as a function
jfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dgamma(0.001, 0.001)
}
## data
jdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jmod <- dc.fit(jdata, c("beta", "sigma"), jfun1, 
    n.clones = 1:5, multiply = "n", unchanged = "np", 
    n.update=1000, n.iter=1000)
## summary with DC SE and R hat
summary(jmod)
dct <- dctable(jmod)
plot(dct)
plot(dct, type="var")
dcd <- dcdiag(jmod)
dcd
plot(dcd)

## How to use estimates to make priors more informative? -- learning process
glmm.model.up <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(lambda[i])
        log(lambda[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:p) {
        beta[1,j] ~ dnorm(priors[j,1], priors[j,2])
    }
    sigma ~ dgamma(priors[(p+1),2], priors[(p+1),1])
}
## function for updating, x is an MCMC object
upfun <- function(x) {
    if (missing(x)) {
        p <- ncol(X)
        return(cbind(c(rep(0, p), 0.001), rep(0.001, p+1)))
    } else {
        par <- coef(x)
        return(cbind(par, rep(0.01, length(par))))
    }
}
## we can also use initial values that are depending on the previous MCMC object
## by setting initsfun
updat <- list(n = n, Y = Y, X = X, p = ncol(X), priors = upfun())
dcmod <- dc.fit(updat, c("beta", "sigma"), glmm.model.up,
    n.clones = 1:5, multiply = "n", unchanged = "p",
    update = "priors", updatefun = upfun)
summary(dcmod)
dct <- dctable(jmod)
plot(dct)
plot(dct, type="var")
dcd <- dcdiag(jmod)
dcd
plot(dcd)

## a question in development: based on dcdiag, should it stop if all criteria are met?
## or that should be considered dangerous, because of MCMC variation?
## or that can be an option, defaulted to FALSE (i.e. stop.if.converged = FALSE argument)

## non identifiable model
set.seed(1234)
Y <- rbinom(n, 1, exp(mu) / (1 + exp(mu)))
## JAGS model as a function
jjfun1 <- function() {
    for (i in 1:n) {
        Y[i] ~ dbern(p[i])
        logit(p[i]) <- alpha[i] + inprod(X[i,], beta[1,])
        alpha[i] ~ dnorm(0, 1/sigma^2)
    }
    for (j in 1:np) {
        beta[1,j] ~ dnorm(0, 0.001)
    }
    sigma ~ dgamma(0.001, 0.001)
}
## data
jjdata <- list(n = n, Y = Y, X = X, np = NCOL(X))
## number of clones to be used, etc.
## iteartive fitting
jjmod <- dc.fit(jjdata, c("beta", "sigma"), jjfun1, 
    n.clones = 1:5, multiply = "n", unchanged = "np", 
    n.update=1000, n.iter=1000)
## summary with DC SE and R hat
summary(jjmod)
ddct <- dctable(jjmod)
plot(ddct)
plot(ddct, type="var")
ddcd <- dcdiag(jjmod)
ddcd
plot(ddcd)
plot(chisq.diag(jjmod))

## glmm example for parfit

library(dcpar)
glmm.model <- function() {
    for (i in 1:n) {
        Y[i] ~ dpois(exp(mu[i]))
        Y[i] ~ dbern(ilogit(mu[i]))
        mu[i] ~ dnorm(inprod(X[i,], beta), 1/exp(log.sigma)^2)
    }
    for (j in 1:np) {
        beta[j] ~ dnorm(0, 0.001)
    }
    log.sigma ~ dnorm(0, 0.001)
}

n <- 200
n.update <- 4000
n.iter <- 2000
k <- c(1,2,5,10)

#n <- 20
#n.update <- 400
#n.iter <- 200
#k <- c(1,2)

beta <- c(1, -1)
sigma <- 0.5

set.seed(1234)
x <- runif(n)
X <- model.matrix(~x)
sigma <- 0.25
alpha <- rnorm(n, 0, sd=sigma)
mu <- drop(X %*% beta) + alpha
Yp <- rpois(n, exp(mu))
Yb <- rbinom(n, 1, plogis(mu))

datp <- list(Y = Yp, X = X, n = n, np = NCOL(X))
datb <- list(Y = Yb, X = X, n = n, np = NCOL(X))
dcdatp <- dclone(datp, n.clones = max(k), multiply = "n", unchanged = "np")

## 1 Poisson model, df.fit
t0 <- proc.time()
mod1 <- dc.fit(datp, c("beta", "log.sigma"), custommodel(glmm.model, 4),
    n.clones = k, multiply = "n", unchanged = "np",
    n.update = n.update, n.iter = n.iter)
t1 <- proc.time() - t0

## 2 Bernoulli model, df.fit
t0 <- proc.time()
mod2 <- dc.fit(datb, c("beta", "log.sigma"), custommodel(glmm.model, 3),
    n.clones = k, multiply = "n", unchanged = "np",
    n.update = n.update, n.iter = n.iter)
t2 <- proc.time() - t0

## 5 Poisson model, jags.fit, k_max
t0 <- proc.time()
mod5 <- jags.fit(dcdatp, c("beta", "log.sigma"), custommodel(glmm.model, 4),
    n.update = n.update, n.iter = n.iter)
t5 <- proc.time() - t0

## 3 Poisson model, dc.parfit
cl <- makeCluster(3, type = "SOCK")
t0 <- proc.time()
mod3 <- dc.parfit(cl, datp, c("beta", "log.sigma"), custommodel(glmm.model, 4),
    n.clones = k, multiply = "n", unchanged = "np",
    n.update = n.update, n.iter = n.iter)
t3 <- proc.time() - t0
#stopCluster(cl)

## 4 Bernoulli model, dc.parfit
#cl <- makeCluster(3, type = "SOCK")
t0 <- proc.time()
mod4 <- dc.parfit(cl, datb, c("beta", "log.sigma"), custommodel(glmm.model, 3),
    n.clones = k, multiply = "n", unchanged = "np",
    n.update = n.update, n.iter = n.iter)
t4 <- proc.time() - t0
#stopCluster(cl)

## 6 Poisson model, jags.parfit, k_max
#cl <- makeCluster(3, type = "SOCK")
t0 <- proc.time()
mod5 <- jags.parfit(cl, dcdatp, c("beta", "log.sigma"), custommodel(glmm.model, 4),
    n.update = n.update, n.iter = n.iter)
t6 <- proc.time() - t0
stopCluster(cl)
## end

rbind(dc.pois=c(fit=t1[3], parfit=t3[3]),
    dc.bern=c(fit=t2[3], parfit=t4[3]),
    jags.pois=c(fit=t5[3], parfit=t6[3]))
